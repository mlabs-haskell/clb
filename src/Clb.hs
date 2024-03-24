{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Clb (
  -- * Parameters
  X.defaultBabbage,

  -- * CLB Monad
  Clb,

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
  waitSlot,
  ValidationResult (..),
  OnChainTx (..),

  -- * Utils
  getCurrentSlot,

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
  PParams (..),
  babbageOnly,

  -- * key utils
  intToKeyPair,
)
where

import Cardano.Api qualified as Api
import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Address qualified as L (compactAddr)
import Cardano.Ledger.Alonzo.TxInfo (transDataHash', txInfoIn')
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.TxOut qualified as L (BabbageTxOut (TxOutCompact), getEitherAddrBabbageTxOut)
import Cardano.Ledger.BaseTypes qualified as L (Globals, Network (Testnet), mkVersion)
import Cardano.Ledger.Compactible qualified as L
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Pretty (ppLedgerState)
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash qualified as L
import Cardano.Ledger.Shelley.API qualified as L (LedgerState (..), StakeReference (..), UTxOState (utxosUtxo), applyTx, ledgerSlotNo)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.TxIn qualified as L (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO qualified as L (UTxO (..))
import Clb.ClbLedgerState (EmulatedLedgerState (..), currentBlock, initialState, ledgerEnv, memPoolState, nextSlot, setSlot, setUtxo)
import Clb.Era (EmulatorEra)
import Clb.MockConfig (MockConfig (..))
import Clb.MockConfig qualified as X (defaultBabbage)
import Clb.Params (PParams (AlonzoParams, BabbageParams), babbageOnly, mkGlobals)
import Clb.TimeSlot (SlotConfig (..), slotLength)
import Clb.Tx (OnChainTx (..))
import Control.Lens (over, (&), (.~), (^.))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get), State, gets, modify, modify', put, runState)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import PlutusLedgerApi.V1 qualified as P (Credential, Datum, DatumHash, TxOutRef)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Scripts qualified as P (ScriptError)
import PlutusLedgerApi.V2 qualified as PV2
import Prettyprinter (Doc, Pretty, colon, fillSep, hang, indent, pretty, vcat, vsep, (<+>))
import Test.Cardano.Ledger.Core.KeyPair qualified as TL

--------------------------------------------------------------------------------
-- Base emulator types
--------------------------------------------------------------------------------

-- | Cardano tx from any era.
data CardanoTx where
  CardanoTx :: (C.IsCardanoEra era) => C.Tx era -> CardanoTx

-- | A reason why a transaction is invalid.
data ValidationError
  = -- | The transaction output consumed by a transaction input could not be found
    -- (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    TxOutRefNotFound -- TxIn
  | -- | For pay-to-script outputs: evaluation of the validator script failed.
    ScriptFailure P.ScriptError
  | -- | An error from Cardano.Ledger validation
    CardanoLedgerValidationError Text
  | -- | Balancing failed, it needed more than the maximum number of collateral inputs
    MaxCollateralInputsExceeded
  deriving (Eq, Show)

data ValidationResult
  = -- | A transaction failed to validate in phase 1.
    FailPhase1 !CardanoTx !ValidationError
  | -- | A transaction failed to validate in phase 2. The @Value@ indicates the amount of collateral stored in the transaction.
    FailPhase2 !OnChainTx !ValidationError !(MaryValue L.StandardCrypto)
  | Success !EmulatedLedgerState !OnChainTx -- !RedeemerReport
  -- deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- CLB base monad (instead of PSM's Run)
--------------------------------------------------------------------------------

-- | State monad wrapper to run emulator.
newtype Clb a = Clb (State ClbState a)
  deriving newtype (Functor, Applicative, Monad, MonadState ClbState)

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
runClb (Clb act) = runState act

-- | Init emulator state.
initClb :: MockConfig -> Api.Value -> Api.Value -> ClbState
initClb MockConfig {mockConfigProtocol = (AlonzoParams _)} _ _ = error "Unsupported params"
initClb
  cfg@MockConfig {mockConfigProtocol = params@(BabbageParams pparams)}
  _initVal
  walletFunds =
    ClbState
      { emulatedLedgerState = setUtxo pparams utxos (initialState params)
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

getCurrentSlot :: Clb C.SlotNo
getCurrentSlot = gets (L.ledgerSlotNo . _ledgerEnv . emulatedLedgerState)

-- | Log a generic (non-typed) error.
logError :: String -> Clb ()
logError msg = do
  logInfo $ LogEntry Error msg
  logFail $ GenericFail msg

-- | Add a non-error log enty.
logInfo :: LogEntry -> Clb ()
logInfo le = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ \s -> s {mockInfo = appendLog slot le (mockInfo s)}

-- | Log failure.
logFail :: FailReason -> Clb ()
logFail res = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ \s -> s {mockFails = appendLog slot res (mockFails s)}

-- | Insert event to log
appendLog :: Slot -> a -> Log a -> Log a
appendLog slot val (Log xs) = Log (xs Seq.|> (slot, val))

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: C.AddressInEra C.BabbageEra -> Clb [P.TxOutRef]
txOutRefAt addr = gets (txOutRefAtState $ C.toShelleyAddr addr)

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: L.Addr L.StandardCrypto -> ClbState -> [P.TxOutRef]
txOutRefAtState addr st = txInfoIn' <$> M.keys (M.filter atAddr utxos)
  where
    utxos = L.unUTxO $ L.utxosUtxo $ L.lsUTxOState $ _memPoolState $ emulatedLedgerState st

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

sendTx :: C.Tx C.BabbageEra -> Clb ValidationResult
sendTx apiTx@(C.ShelleyTx _ tx) = do
  -- FIXME: use patterns?
  state@ClbState
    { mockConfig = MockConfig {mockConfigProtocol, mockConfigSlotConfig}
    , mockDatums = mockDatums
    } <-
    get
  -- FIXME: fromJust
  let majorVer =
        fromJust $
          runIdentity $
            runMaybeT @Identity $
              L.mkVersion $
                fst $
                  C.protocolParamProtocolVersion $
                    C.fromLedgerPParams C.ShelleyBasedEraBabbage $
                      babbageOnly mockConfigProtocol
  let globals = mkGlobals (slotLength mockConfigSlotConfig) majorVer
  let ret =
        validateTx
          globals
          (emulatedLedgerState state)
          tx
  case ret of
    Success newState _ -> do
      let txDatums = scriptDataFromCardanoTxBody $ C.getTxBody apiTx
      put $
        state
          { -- TODO: introduce slot modes
            emulatedLedgerState = nextSlot newState
          , mockDatums = M.union mockDatums txDatums
          }
    FailPhase1 {} -> pure ()
    FailPhase2 {} -> pure ()
  pure ret

{- | Given a 'C.TxBody from a 'C.Tx era', return the datums and redeemers along
with their hashes.
-}
scriptDataFromCardanoTxBody ::
  C.TxBody era ->
  -- -> (Map P.DatumHash P.Datum, PV1.Redeemers)
  M.Map P.DatumHash P.Datum
scriptDataFromCardanoTxBody C.ByronTxBody {} = mempty
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
  transDataHash' $ L.hashData $ L.Data @(L.AlonzoEra L.StandardCrypto) dat

dumpUtxoState :: Clb ()
dumpUtxoState = do
  s <- gets ((^. memPoolState) . emulatedLedgerState)
  let dump = show $ ppLedgerState s
  logInfo $ LogEntry Info dump

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

validateTx :: L.Globals -> EmulatedLedgerState -> Core.Tx EmulatorEra -> ValidationResult
validateTx globals state tx =
  case res of
    -- FIXME: why Phase1, not sure here?
    Left err ->
      FailPhase1
        -- (CardanoTx (C.ShelleyTx C.ShelleyBasedEraBabbage tx) C.BabbageEraInCardanoMode)
        (CardanoTx (C.ShelleyTx C.ShelleyBasedEraBabbage tx))
        err
    Right (newState, vtx) ->
      Success newState vtx
  where
    res = applyTx globals state tx

{- | A wrapper around the ledger's applyTx
TODO: step slot somewhere, since this is not ledger's responsibility!
-}
applyTx ::
  L.Globals ->
  EmulatedLedgerState ->
  Core.Tx EmulatorEra ->
  Either ValidationError (EmulatedLedgerState, OnChainTx)
applyTx globals oldState@EmulatedLedgerState {_ledgerEnv, _memPoolState} tx = do
  (newMempool, OnChainTx -> vtx) <-
    first
      (CardanoLedgerValidationError . Text.pack . show)
      (L.applyTx globals _ledgerEnv _memPoolState tx)
  pure (oldState & memPoolState .~ newMempool & over currentBlock (vtx :), vtx)

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

waitSlot :: SlotNo -> Clb ()
waitSlot slot = do
  currSlot <- gets (L.ledgerSlotNo . (^. ledgerEnv) . emulatedLedgerState)
  -- TODO: shall we throw?
  when (currSlot < slot) $
    modify $
      \s@ClbState {emulatedLedgerState = state} -> s {emulatedLedgerState = setSlot slot state}

-- TODO: implement
-- pure ()
-- now <- slotOfCurrentBlock
-- let d = slotToInteger slot - slotToInteger now
-- if | d < 0     -> fail $ printf "can't wait for slot %d, because current slot is %d" (slotToInteger slot) (slotToInteger now)
--    | d == 0    -> return ()
--    | otherwise -> liftRun $ waitNSlots $ Fork.Slot d

-- undefined
