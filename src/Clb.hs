{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Clb (
  -- * Parameters
  X.defaultBabbage,

  -- * CLB Monad
  Clb,
  ClbT (unwrapClbT),

  -- * CLB internals (revise)
  ClbState (..),
  EmulatedLedgerState (..),

  -- * CLB Runner
  initClb,
  runClb,

  -- * Actions
  txOutRefAt,
  txOutRefAtState,
  -- FIXME: implement
  txOutRefAtPaymentCred,
  sendTx,
  ValidationResult (..),
  OnChainTx (..),

  -- * Querying
  getCurrentSlot,
  getUtxosAtState,
  getEpochInfo,

  -- * Working with logs
  LogEntry (LogEntry),
  LogLevel (..),
  Log (Log),
  unLog,
  fromLog,
  logInfo,
  logFail,
  logError,
  dumpUtxoState,

  -- * Log utils
  checkErrors,
  getFails,
  ppLog,

  -- * Emulator configuration
  MockConfig (..),
  SlotConfig (..),

  -- * Protocol parameters
  PParams,

  -- * key utils
  intToKeyPair,
  intToCardanoSk,
)
where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Address qualified as L (compactAddr)
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.TxOut qualified as L (BabbageTxOut (TxOutCompact), getEitherAddrBabbageTxOut)
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Compactible qualified as L
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Plutus.TxInfo (transDataHash, transTxIn)
import Cardano.Ledger.SafeHash qualified as L
import Cardano.Ledger.Shelley.API qualified as L hiding (TxOutCompact)
import Cardano.Ledger.Shelley.Core (EraRule)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.TxIn qualified as L
import Cardano.Slotting.EpochInfo (EpochInfo)
import Clb.ClbLedgerState (EmulatedLedgerState (..), currentBlock, initialState, memPoolState, setSlot, setUtxo)
import Clb.Era (EmulatorEra)
import Clb.MockConfig (MockConfig (..))
import Clb.MockConfig qualified as X (defaultBabbage)
import Clb.Params (PParams, genesisDefaultsFromParams)
import Clb.TimeSlot (SlotConfig (..), slotConfigToEpochInfo)
import Clb.Tx (OnChainTx (..))
import Control.Arrow (ArrowChoice (..))
import Control.Lens (over, (&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (runReader)
import Control.Monad.State (MonadState (get), State, StateT, gets, modify, modify', put, runState)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.State.Transition (SingEP (..), globalAssertionPolicy)
import Control.State.Transition.Extended (
  ApplySTSOpts (..),
  TRC (..),
  ValidationPolicy (..),
  applySTSOptsEither,
 )
import Text.Show.Pretty (ppShow)
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

--------------------------------------------------------------------------------
-- Base emulator types
--------------------------------------------------------------------------------

-- FIXME: legacy types converted into type synonyms
type CardanoTx = Core.Tx EmulatorEra
type ValidationError = L.ApplyTxError EmulatorEra

data ValidationResult
  = -- | A transaction failed to be validated by Ledger
    Fail !CardanoTx !ValidationError
  | Success !EmulatedLedgerState !OnChainTx
  deriving stock (Show)

--------------------------------------------------------------------------------
-- CLB base monad (instead of PSM's Run)
--------------------------------------------------------------------------------

newtype ClbT m a = ClbT {unwrapClbT :: StateT ClbState m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadState ClbState
    )

-- | State monad wrapper to run emulator.
type Clb a = ClbT Identity a

{- | Emulator state: ledger state + some additional things
FIXME: remove non-state parts like MockConfig and Log (?)
-}
data ClbState = ClbState
  { emulatedLedgerState :: !EmulatedLedgerState
  , mockConfig :: !MockConfig
  , -- FIXME: rename, this is non-inline dataums cache
    mockDatums :: !(M.Map P.DatumHash P.Datum)
  , mockInfo :: !(Log LogEntry)
  , mockFails :: !(Log FailReason)
  }

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
runClb :: Clb a -> ClbState -> (a, ClbState)
runClb (ClbT act) = runState act

-- | Init emulator state.
initClb :: MockConfig -> Api.Value -> Api.Value -> ClbState
initClb
  cfg@MockConfig {mockConfigProtocol = pparams}
  _initVal
  walletFunds =
    ClbState
      { emulatedLedgerState = setUtxo pparams utxos (initialState pparams)
      , mockDatums = M.empty
      , mockConfig = cfg
      , mockInfo = mempty
      , mockFails = mempty
      }
    where
      utxos = L.UTxO $ M.fromList $ mkGenesis walletFunds <$> [1 .. 9]

      -- genesis :: (L.TxIn (Core.EraCrypto EmulatorEra), Core.TxOut EmulatorEra)
      -- genesis =
      --   ( L.mkTxInPartial genesisTxId 0
      --   , L.TxOutCompact
      --       (L.compactAddr $ mkAddr' $ intToKeyPair 0) -- TODO: use wallet distribution
      --       (fromJust $ L.toCompact $ C.toMaryValue $ valueToApi initVal)
      --   )

      mkGenesis :: Api.Value -> Integer -> (L.TxIn (Core.EraCrypto EmulatorEra), Core.TxOut EmulatorEra)
      mkGenesis walletFund wallet =
        ( L.mkTxInPartial genesisTxId wallet
        , L.TxOutCompact
            (L.compactAddr $ mkAddr' $ intToKeyPair wallet)
            (fromJust $ L.toCompact $ C.toMaryValue walletFund)
        )

      mkAddr' payKey = L.Addr L.Testnet (TL.mkCred payKey) L.StakeRefNull

      -- \| genesis transaction ID
      genesisTxId :: L.TxId L.StandardCrypto
      genesisTxId = L.TxId $ L.unsafeMakeSafeHash dummyHash

      -- Hash for genesis transaction
      dummyHash :: Crypto.Hash Crypto.Blake2b_256 Core.EraIndependentTxBody
      dummyHash = Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

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

getCurrentSlot :: (Monad m) => ClbT m C.SlotNo
getCurrentSlot = gets (L.ledgerSlotNo . _ledgerEnv . emulatedLedgerState)

-- | Log a generic (non-typed) error.
logError :: (Monad m) => String -> ClbT m ()
logError msg = do
  logInfo $ LogEntry Error msg
  logFail $ GenericFail msg

-- | Add a non-error log enty.
logInfo :: (Monad m) => LogEntry -> ClbT m ()
logInfo le = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ \s -> s {mockInfo = appendLog slot le (mockInfo s)}

-- | Log failure.
logFail :: (Monad m) => FailReason -> ClbT m ()
logFail res = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ \s -> s {mockFails = appendLog slot res (mockFails s)}

-- | Insert event to log
appendLog :: Slot -> a -> Log a -> Log a
appendLog slot val (Log xs) = Log (xs Seq.|> (slot, val))

getUtxosAtState :: ClbState -> L.UTxO EmulatorEra
getUtxosAtState state =
  L.utxosUtxo $
    L.lsUTxOState $
      _memPoolState $
        emulatedLedgerState state

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: (Monad m) => C.AddressInEra C.BabbageEra -> ClbT m [P.TxOutRef]
txOutRefAt addr = gets (txOutRefAtState $ C.toShelleyAddr addr)

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: L.Addr L.StandardCrypto -> ClbState -> [P.TxOutRef]
txOutRefAtState addr st = transTxIn <$> M.keys (M.filter atAddr utxos)
  where
    utxos = L.unUTxO $ getUtxosAtState st
    atAddr :: L.BabbageTxOut EmulatorEra -> Bool
    atAddr out = case L.getEitherAddrBabbageTxOut out of
      Right cAddr -> L.compactAddr addr == cAddr
      -- Left should never happen (abuse of Either in fact)
      Left addr' -> addr == addr'

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCred :: P.Credential -> Clb [P.TxOutRef]
txOutRefAtPaymentCred cred = gets (txOutRefAtPaymentCredState cred)

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCredState :: P.Credential -> ClbState -> [P.TxOutRef]
txOutRefAtPaymentCredState _cred _st = undefined -- FIXME:

getEpochInfo :: (Monad m) => ClbT m (EpochInfo (Either Text))
getEpochInfo =
  gets (slotConfigToEpochInfo . mockConfigSlotConfig . mockConfig)

getGlobals :: (Monad m) => ClbT m Globals
getGlobals = do
  pparams <- gets (mockConfigProtocol . mockConfig)
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

-- | Run `applyTx`, if succeed update state and record datums
sendTx :: (Monad m) => C.Tx C.BabbageEra -> ClbT m ValidationResult
sendTx apiTx@(C.ShelleyTx _ tx) = do
  state@ClbState {emulatedLedgerState} <- get
  globals <- getGlobals
  case applyTx ValidateAll globals emulatedLedgerState tx of
    Right (newState, vtx) -> do
      put $ state {emulatedLedgerState = newState}
      recordNewDatums
      return $ Success newState vtx
    Left err -> return $ Fail tx err
  where
    recordNewDatums = do
      state@ClbState {mockDatums} <- get
      let txDatums = scriptDataFromCardanoTxBody $ C.getTxBody apiTx
      put $
        state
          { mockDatums = M.union mockDatums txDatums
          }

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

dumpUtxoState :: Clb ()
dumpUtxoState = do
  s <- gets ((^. memPoolState) . emulatedLedgerState)
  logInfo $ LogEntry Info $ ppShow s

--------------------------------------------------------------------------------
-- Helpers for working with emulator traces
--------------------------------------------------------------------------------

{- | Checks that script runs without errors and returns pretty printed failure
 if something bad happens.
-}
checkErrors :: Clb (Maybe String)
checkErrors = do
  failures <- fromLog <$> getFails
  pure $
    if null failures
      then Nothing
      else Just (init . unlines $ fmap show failures)

-- | Return list of failures
getFails :: Clb (Log FailReason)
getFails = gets mockFails

--------------------------------------------------------------------------------
-- Transactions validation TODO: factor out
--------------------------------------------------------------------------------

-- | Code copy-pasted from ledger's `applyTx` to use custom `ApplySTSOpts`
applyTx ::
  forall stsUsed.
  (stsUsed ~ EraRule "LEDGER" EmulatorEra) =>
  ValidationPolicy ->
  L.Globals ->
  EmulatedLedgerState ->
  Core.Tx EmulatorEra ->
  Either (L.ApplyTxError EmulatorEra) (EmulatedLedgerState, OnChainTx)
applyTx asoValidation globals oldState@EmulatedLedgerState {_ledgerEnv, _memPoolState} tx = do
  newMempool <-
    left L.ApplyTxError
      $ flip runReader globals
        . applySTSOptsEither @stsUsed opts
      $ TRC (_ledgerEnv, _memPoolState, tx)
  let vtx = OnChainTx $ L.unsafeMakeValidated tx
  pure (oldState & memPoolState .~ newMempool & over currentBlock (vtx :), vtx)
  where
    opts =
      ApplySTSOpts
        { asoAssertions = globalAssertionPolicy
        , asoValidation
        , asoEvents = EPDiscard
        }

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

intToCardanoSk n = case intToKeyPair @(Core.EraCrypto EmulatorEra) n of
  TL.KeyPair _ sk -> C.PaymentSigningKey sk

waitSlot :: SlotNo -> Clb ()
waitSlot slot = do
  currSlot <- getCurrentSlot
  -- TODO: shall we throw?
  when (currSlot < slot) $
    modify $
      \s@ClbState {emulatedLedgerState = state} ->
        s {emulatedLedgerState = setSlot slot state}
