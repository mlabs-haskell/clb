{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Clb (
  -- * Parameters
  X.defaultBabbage,

  -- * CLB Monad
  Clb (unwrapClb),

  -- * CLB internals (revise)
  ClbRuntime (..),
  EmulatedLedgerState (..),

  -- * CLB Runner
  initClb,
  runClb,

  -- * Actions
  txOutRefAt,
  -- FIXME: implement
  txOutRefAtPaymentCred,
  sendTx,
  ValidationResult (..),
  OnChainTx (..),

  -- * Querying
  getCurrentSlot,
  getEpochInfo,

  -- * Working with logs
  LogEntry (..),
  LogLevel (..),
  Log (Log),
  unLog,
  fromLog,
  logError,

  -- * Log utils
  ppLog,

  -- * Emulator configuration
  MockConfig (..),
  SlotConfig (..),

  -- * Protocol parameters
  PParams,

  -- * key utils
  intToKeyPair,
  intToCardanoSk,
  waitSlot,
)
where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Plutus.TxInfo (transDataHash, transTxIn)
import Cardano.Ledger.Shelley.API qualified as L hiding (TxOutCompact)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Clb.ClbLedgerState (EmulatedLedgerState (..))
import Clb.Era (EmulatorEra)
import Clb.MockConfig (MockConfig (..))
import Clb.MockConfig qualified as X (defaultBabbage)
import Clb.Params (PParams, genesisDefaultsFromParams)
import Clb.TimeSlot (SlotConfig (..), slotConfigToEpochInfo, utcTimeToPOSIXTime)
import Clb.Tx (OnChainTx (..))
import Control.Arrow ((&&&))
import Control.Monad (forever, void)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT), asks, MonadReader)
import Control.Monad.Writer (WriterT (runWriterT), tell, MonadWriter)
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import PlutusLedgerApi.V1 qualified as P (Credential, Datum, DatumHash, TxOutRef)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import Prettyprinter (Doc, Pretty, colon, fillSep, hang, indent, pretty, vcat, vsep, (<+>))
import Test.Cardano.Ledger.Core.KeyPair qualified as TL
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Ouroboros

import Cardano.Testnet

import qualified Hedgehog as H
import qualified Control.Concurrent.STM as STM

import Testnet.Property.Utils (integrationWorkspace)
import Control.Concurrent (threadDelay, ThreadId)
import Control.Monad.Trans.Resource
import qualified Hedgehog.Extras.Stock as H'
import Testnet.Runtime
    ( PaymentKeyInfo(paymentKeyInfoPair),
      PaymentKeyPair(paymentVKey, paymentSKey),
      PoolNode(poolRuntime),
      TestnetRuntime(..), getStartTime, shelleyGenesis )
import Control.Exception (Exception, throwIO)
import qualified Data.Set as Set
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import qualified Cardano.Api.InMode as C

--------------------------------------------------------------------------------
-- Base emulator types
--------------------------------------------------------------------------------

type KeyPair = TL.KeyPair L.Payment (Core.EraCrypto EmulatorEra)

data ClbRuntime = ClbRuntime
  { nodeConnectInfo :: Api.LocalNodeConnectInfo
  , wallets         :: [(FilePath, FilePath)]
  , slotConfig      :: SlotConfig
  , threadId        :: ThreadId
  }

-- FIXME: legacy types converted into type synonyms
type CardanoTx = Core.Tx EmulatorEra
type ValidationError = L.ApplyTxError EmulatorEra

data ValidationResult
  = -- | A transaction failed to be validated by Ledger
    Fail !CardanoTx !ValidationError
  | Success !OnChainTx
  deriving stock (Show)

--------------------------------------------------------------------------------
-- CLB base monad (instead of PSM's Run)
--------------------------------------------------------------------------------

newtype Clb a = Clb {unwrapClb :: ReaderT ClbRuntime (WriterT (Log LogEntry) IO) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader ClbRuntime
    , MonadWriter (Log LogEntry)
    )

data LogEntry = LogEntry
  { leLevel :: !LogLevel
  , leMsg :: !String
  }

data LogLevel = Debug | Info | Warning | Error

instance Pretty LogLevel where
  pretty Debug = "[DEBUG]"
  pretty Info = "[ INFO]"
  pretty Warning = "[ WARN]"
  pretty Error = "[ERROR]"

instance Pretty LogEntry where
  pretty (LogEntry l msg) = pretty l <+> hang 1 msg'
    where
      ws = wordsIdent <$> lines msg
      wsD = fmap pretty <$> ws
      ls = fillSep <$> wsD
      msg' = vsep ls

-- | Like 'words' but keeps leading identation.
wordsIdent :: String -> [String]
wordsIdent s = takeWhile isSpace s : words s

-- | Fail reasons.
newtype FailReason
  = GenericFail String
  deriving (Show)

-- | Run CLB emulator.
runClb :: Clb a -> ClbRuntime -> IO (a, Log LogEntry)
runClb (Clb act) = runWriterT . runReaderT act

-- Based on: https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Property/Run.hs
-- They are using hedgehog (property testing framework) to orchestrate a testnet running in the background
-- ....for some god forsaken reason
-- the result is very awkward.
initClb :: IO ClbRuntime
initClb = do
  tmvRuntime <- STM.newEmptyTMVarIO

  void . H.check $ integrationWorkspace "testnet" $ \workspaceDir -> do
    conf <- mkConf workspaceDir

    -- Fork a thread to keep alive indefinitely any resources allocated by testnet.
    tid <- H.evalM . liftResourceT . resourceForkIO . forever . liftIO $ threadDelay 10000000

    let testnetOpts = cardanoDefaultTestnetOptions
    tr@TestnetRuntime
      { wallets
      , poolNodes
      , testnetMagic
      } <- cardanoTestnetDefault testnetOpts conf
    startTime <- getStartTime workspaceDir tr

    liftIO . STM.atomically
      $ STM.writeTMVar tmvRuntime ClbRuntime
          -- TODO: Consider obtaining everything here from shelleyGenesis rather than testnetOpts.
          -- See: https://www.doitwithlovelace.io/haddock/cardano-ledger-shelley/html/Cardano-Ledger-Shelley-Genesis.html
          -- See: https://github.com/IntersectMBO/cardano-node/blob/43149909fc4942e93e14a2686826543a2d9432bf/cardano-testnet/src/Testnet/Types.hs#L155
          { nodeConnectInfo = Api.LocalNodeConnectInfo
              { localConsensusModeParams = Api.CardanoModeParams
                  . Api.EpochSlots
                  . fromIntegral
                  $ cardanoEpochLength testnetOpts
              , localNodeNetworkId       = Api.Testnet . Api.NetworkMagic $ fromIntegral testnetMagic
              , localNodeSocketPath      = Api.File
                  . H'.sprocketSystemName
                  . nodeSprocket
                  . poolRuntime
                  $ head poolNodes
              }
          , slotConfig = SlotConfig
              { scSlotZeroTime = utcTimeToPOSIXTime startTime
              , scSlotLength   = round $ cardanoSlotLength testnetOpts * 1000
              }
          , wallets         = (paymentSKey &&& paymentVKey) . paymentKeyInfoPair <$> wallets
          , threadId        = tid
          }

  STM.atomically $ STM.readTMVar tmvRuntime

--------------------------------------------------------------------------------
-- Trace log (from PSM)
--------------------------------------------------------------------------------

-- | Log of Slot-timestamped events
newtype Log a = Log {unLog :: Seq (Slot, a)}
  deriving (Functor)

instance Semigroup (Log a) where
  (<>) (Log sa) (Log sb) = Log (merge sa sb)
    where
      merge Empty b = b
      merge a Empty = a
      merge (a :<| as) (b :<| bs) =
        if fst a <= fst b
          then a Seq.<| merge as (b Seq.<| bs)
          else b Seq.<| merge (a Seq.<| as) bs

instance Monoid (Log a) where
  mempty = Log Seq.empty

ppLog :: Log LogEntry -> Doc ann
ppLog = vcat . fmap ppSlot . fromGroupLog
  where
    ppSlot (slot, events) = vcat [pretty slot <> colon, indent 2 (vcat $ pretty <$> events)]

fromGroupLog :: Log a -> [(Slot, [a])]
fromGroupLog = fmap toGroup . groupBy ((==) `on` fst) . fromLog
  where
    toGroup ((a, b) : rest) = (a, b : fmap snd rest)
    toGroup [] = error "toGroup: Empty list"

-- | Convert log to plain list
fromLog :: Log a -> [(Slot, a)]
fromLog (Log s) = toList s

newtype Slot = Slot {getSlot :: Integer}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Enum, Real, Integral)

instance Pretty Slot where
  pretty (Slot i) = "Slot" <+> pretty i

--------------------------------------------------------------------------------
-- Actions in Clb monad
--------------------------------------------------------------------------------

getCurrentSlot :: Clb C.SlotNo
getCurrentSlot = do
  info <- asks nodeConnectInfo
  Api.ChainTip s _ _ <- liftIO $ Api.getLocalChainTip info
  pure s

-- | Log a generic (non-typed) error.
logError :: String -> Clb ()
logError msg = do
  logEntry $ LogEntry Error msg

logEntry :: LogEntry -> Clb ()
logEntry le = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  tell $ mkLog slot le

-- | Insert event to log
mkLog :: Slot -> a -> Log a
mkLog slot val = Log $ pure (slot, val)

-- FIXME: Implement utxosAtAddress instead of txOutRefAt and also implement utxoAtRef

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: C.AddressInEra C.BabbageEra -> Clb [P.TxOutRef]
txOutRefAt addr = asks nodeConnectInfo >>= txOutRefAtState (C.toShelleyAddr addr)

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: L.Addr L.StandardCrypto -> Api.LocalNodeConnectInfo -> Clb [P.TxOutRef]
txOutRefAtState addr nodeConnectInfo = do
  x <- fmap Api.unUTxO . liftIO . queryUTxO nodeConnectInfo
    . Api.QueryUTxOByAddress
    . Set.singleton
    $ C.fromShelleyAddrToAny addr
  pure $ transTxIn . C.toShelleyTxIn <$> M.keys x

newtype CardanoQueryException = CardanoQueryException String
  deriving stock (Show)
  deriving anyclass (Exception)

queryCardanoMode :: Api.LocalNodeConnectInfo -> Api.QueryInMode a -> IO a
queryCardanoMode info q = do
    e <- Api.queryNodeLocalState info Ouroboros.VolatileTip q
    case e of
        Left err -> throwIO $ CardanoQueryException $ show err
        Right x  -> return x

queryBabbageEra :: Api.LocalNodeConnectInfo -> Api.QueryInShelleyBasedEra Api.BabbageEra a -> IO a
queryBabbageEra info q = do
    e <- queryCardanoMode info $ Api.QueryInEra $ Api.QueryInShelleyBasedEra Api.ShelleyBasedEraBabbage q
    case e of
        Left err -> throwIO $ CardanoQueryException $ show err
        Right x  -> return x

queryUTxO :: Api.LocalNodeConnectInfo -> Api.QueryUTxOFilter -> IO (C.UTxO C.BabbageEra)
queryUTxO info q = queryBabbageEra info $ Api.QueryUTxO q

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCred :: P.Credential -> Clb [P.TxOutRef]
txOutRefAtPaymentCred cred = undefined -- FIXME:

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCredState :: P.Credential -> ClbRuntime -> [P.TxOutRef]
txOutRefAtPaymentCredState _cred _st = undefined -- FIXME:

getEpochInfo :: Clb (EpochInfo (Either Text))
getEpochInfo = asks (slotConfigToEpochInfo . slotConfig)

getGlobals :: Clb Globals
getGlobals = do
  info <- asks nodeConnectInfo
  pparams <- liftIO $ queryBabbageEra info Api.QueryProtocolParameters
  -- FIXME: fromJust
  let majorVer =
        fromJust $
          runIdentity $
            runMaybeT @Identity $
              L.mkVersion $
                fst $
                  C.protocolParamProtocolVersion $
                    C.fromLedgerPParams C.ShelleyBasedEraBabbage pparams
  epochInfo <- getEpochInfo
  return $
    L.mkShelleyGlobals
      genesisDefaultsFromParams
      epochInfo
      majorVer

sendTx :: C.Tx C.BabbageEra -> Clb ValidationResult
sendTx apiTx@(C.ShelleyTx _ tx) = do
  info <- asks nodeConnectInfo
  res <- liftIO $ Api.submitTxToNodeLocal info $ Api.TxInMode Api.ShelleyBasedEraBabbage apiTx
  case res of
    SubmitSuccess  -> pure $ Success . OnChainTx $ L.unsafeMakeValidated tx
    SubmitFail (C.TxValidationErrorInCardanoMode (C.ShelleyTxValidationError C.ShelleyBasedEraBabbage x)) -> pure $ Fail tx x
    -- TODO: Checked error?
    _ -> error "absurd!"

{- | Given a 'C.TxBody from a 'C.Tx era', return the datums and redeemers along
with their hashes.
-}
scriptDataFromCardanoTxBody ::
  C.TxBody era ->
  -- -> (Map P.DatumHash P.Datum, PV1.Redeemers)
  M.Map P.DatumHash P.Datum
scriptDataFromCardanoTxBody (C.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) = mempty
scriptDataFromCardanoTxBody
  (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (L.TxDats' dats) _) _ _) =
    let datums =
          M.fromList
            ( (\d -> (datumHash d, d))
                . PV1.Datum
                . fromCardanoScriptData
                . C.getScriptData
                . C.fromAlonzoData
                <$> M.elems dats
            )
     in datums

fromCardanoScriptData :: C.ScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData

datumHash :: PV2.Datum -> PV2.DatumHash
datumHash (PV2.Datum (PV2.BuiltinData dat)) =
  transDataHash $ L.hashData $ L.Data @(L.AlonzoEra L.StandardCrypto) dat

--------------------------------------------------------------------------------
-- Key utils
--------------------------------------------------------------------------------

-- | Create key pair from an integer (deterministic)
intToKeyPair :: (L.Crypto c) => Integer -> TL.KeyPair r c
intToKeyPair n = TL.KeyPair vk sk
  where
    sk = Crypto.genKeyDSIGN $ mkSeedFromInteger n
    vk = L.VKey $ Crypto.deriveVerKeyDSIGN sk

    -- \| Construct a seed from a bunch of Word64s
    --
    --      We multiply these words by some extra stuff to make sure they contain
    --      enough bits for our seed.
    --
    mkSeedFromInteger :: Integer -> Crypto.Seed
    mkSeedFromInteger stuff =
      Crypto.mkSeedFromBytes . Crypto.hashToBytes $
        Crypto.hashWithSerialiser @Crypto.Blake2b_256 CBOR.toCBOR stuff

intToCardanoSk :: Integer -> C.SigningKey C.PaymentKey
intToCardanoSk n = case intToKeyPair @(Core.EraCrypto EmulatorEra) n of
  TL.KeyPair _ sk -> C.PaymentSigningKey sk

waitSlot :: C.SlotNo -> Clb ()
waitSlot s = void loop
  where
    loop :: Clb C.SlotNo
    loop = do
        t <- getCurrentSlot
        if t >= s
            then pure t
            else do
                liftIO $ threadDelay 100_000
                loop
