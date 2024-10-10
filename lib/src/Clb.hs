{-# LANGUAGE RankNTypes #-}

module Clb (
  -- * Parameters
  X.defaultBabbageClbConfig,
  X.defaultConwayClbConfig,

  -- * CLB Monad
  Clb,
  ClbT (..),

  -- * CLB internals (revise)
  ClbState (..),
  emulatedLedgerState,
  clbConfig,
  mockDatums,
  mockInfo,
  mockFails,
  EmulatedLedgerState (..),

  -- * CLB Runner
  initClb,
  runClb,

  -- * Actions
  txOutRefAt,
  txOutRefAtState,
  txOutRefAtPaymentCred,
  sendTx,
  validateTx,
  ValidationResult (..),
  ValidationError,
  OnChainTx (..),
  Block,

  -- * Querying
  getClbConfig,
  getCurrentSlot,
  getUtxosAt,
  getUtxosAtState,
  getEpochInfo,
  getGlobals,

  -- * Working with logs
  LogEntry (..),
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
  ClbConfig (..),
  SlotConfig (..),

  -- * Protocol parameters
  PParams,

  -- * key utils
  intToKeyPair,
  intToCardanoSk,
  waitSlot,
  modifySlot,
  processBlock,
  addTxToPool,
  applyTx,
)
where

import Cardano.Api (shelleyBasedEra)
import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Address qualified as L (compactAddr, decompactAddr)
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Plutus.TxInfo (transCred, transDataHash, transTxIn)
import Cardano.Ledger.SafeHash qualified as L
import Cardano.Ledger.Shelley.API qualified as L hiding (TxOutCompact)
import Cardano.Ledger.Shelley.Core (EraRule)
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn qualified as L
import Cardano.Slotting.EpochInfo (EpochInfo)
import Clb.ClbLedgerState (CardanoTx, EmulatedLedgerState (..), TxPool, currentBlock, getEmulatorEraTx, initialState, memPoolState, setSlot, setUtxo)
import Clb.Era (CardanoLedgerEra, IsCardanoLedgerEra, maryBasedEra)
import Clb.MockConfig (ClbConfig (..))
import Clb.MockConfig qualified as X (defaultBabbageClbConfig, defaultConwayClbConfig)
import Clb.Params (PParams, emulatorShelleyGenesisDefaults)
import Clb.TimeSlot (Slot (..), SlotConfig (..), slotConfigToEpochInfo)
import Clb.TimeSlot qualified as TimeSlot
import Clb.Tx (Block, OnChainTx (..))
import Control.Arrow (ArrowChoice (..))
import Control.Lens (makeLenses, over, view, (&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (runReader)
import Control.Monad.State (MonadState (get), StateT, gets, modify, modify', put, runState)
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.State.Transition (SingEP (..), globalAssertionPolicy)
import Control.State.Transition.Extended (
  ApplySTSOpts (..),
  TRC (..),
  ValidationPolicy (..),
  applySTSOptsEither,
 )
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import PlutusLedgerApi.V1 qualified as P (Credential, Datum, DatumHash, TxOutRef)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import Prettyprinter (Doc, Pretty, colon, fillSep, hang, indent, pretty, vcat, vsep, (<+>))
import Test.Cardano.Ledger.Core.KeyPair qualified as TL
import Text.Show.Pretty (ppShow)

--------------------------------------------------------------------------------
-- Base emulator types
--------------------------------------------------------------------------------

-- FIXME: legacy types converted into type synonyms
-- type CardanoTx era = Core.Tx (CardanoLedgerEra era)
type ValidationError era = L.ApplyTxError (CardanoLedgerEra era)

data ValidationResult era
  = -- | A transaction failed to be validated by Ledger
    Fail !(Core.Tx (CardanoLedgerEra era)) !(ValidationError era)
  | Success !(EmulatedLedgerState era) !(OnChainTx era)

deriving stock instance
  (IsCardanoLedgerEra era, Show (Core.Tx (CardanoLedgerEra era))) =>
  Show (ValidationResult era)

--------------------------------------------------------------------------------
-- CLB base monad (instead of PSM's Run)
--------------------------------------------------------------------------------

newtype ClbT era m a = ClbT {unwrapClbT :: StateT (ClbState era) m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadState (ClbState era)
    , MonadTrans
    )

-- | State monad wrapper to run emulator.
type Clb era = ClbT era Identity

{- | Emulator state: ledger state + some additional things
FIXME: remove non-state parts like MockConfig and Log (?)
-}
data ClbState era = ClbState
  { _emulatedLedgerState :: !(EmulatedLedgerState era)
  , _clbConfig :: !(ClbConfig era)
  , -- FIXME: rename, this is non-inline dataums cache
    _mockDatums :: !(M.Map P.DatumHash P.Datum)
  , _mockInfo :: !(Log LogEntry)
  , _mockFails :: !(Log FailReason)
  , _txPool :: !(TxPool era)
  }

-- deriving stock (Show)

data LogEntry = LogEntry
  { leLevel :: !LogLevel
  , leMsg :: !String
  }
  deriving stock (Show)

data LogLevel = Debug | Info | Warning | Error
  deriving stock (Show)

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
runClb :: Clb era a -> ClbState era -> (a, ClbState era)
runClb (ClbT act) = runState act

-- | Init emulator state.
initClb ::
  forall era.
  (IsCardanoLedgerEra era) =>
  ClbConfig era ->
  Api.Value ->
  Api.Value ->
  ClbState era
initClb
  cfg@ClbConfig {clbConfigProtocol = pparams}
  _initVal
  walletFunds =
    ClbState
      { _emulatedLedgerState = setUtxo pparams utxos (initialState pparams)
      , _mockDatums = M.empty
      , _clbConfig = cfg
      , _mockInfo = mempty
      , _mockFails = mempty
      , _txPool = mempty
      }
    where
      utxos :: L.UTxO (CardanoLedgerEra era)
      utxos = L.UTxO $ M.fromList $ mkGenesis walletFunds <$> [1 .. 10]

      mkGenesis :: Api.Value -> Integer -> (L.TxIn L.StandardCrypto, Core.TxOut (C.ShelleyLedgerEra era))
      mkGenesis walletFund wallet =
        ( L.mkTxInPartial genesisTxId wallet
        , L.mkBasicTxOut
            (mkAddr' $ intToKeyPair wallet)
            (C.toLedgerValue (maryBasedEra @era) walletFund)
        )

      mkAddr' :: TL.KeyPair 'L.Payment L.StandardCrypto -> L.Addr L.StandardCrypto
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
  deriving stock (Show, Functor)

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

--------------------------------------------------------------------------------
-- Actions in Clb monad
--------------------------------------------------------------------------------

getClbConfig :: (Monad m) => ClbT era m (ClbConfig era)
getClbConfig = gets _clbConfig

getCurrentSlot :: (Monad m) => ClbT era m C.SlotNo
getCurrentSlot = gets (L.ledgerSlotNo . _ledgerEnv . _emulatedLedgerState)

-- | Log a generic (non-typed) error.
logError :: (Monad m) => String -> ClbT era m ()
logError msg = do
  logInfo $ LogEntry Error msg
  logFail $ GenericFail msg

-- | Add a non-error log enty.
logInfo :: (Monad m) => LogEntry -> ClbT era m ()
logInfo le = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ \s -> s {_mockInfo = appendLog slot le (_mockInfo s)}

-- | Log failure.
logFail :: (Monad m) => FailReason -> ClbT era m ()
logFail res = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ \s -> s {_mockFails = appendLog slot res (_mockFails s)}

-- | Insert event to log
appendLog :: Slot -> a -> Log a -> Log a
appendLog slot val (Log xs) = Log (xs Seq.|> (slot, val))

getUtxosAt ::
  (Monad m, Core.EraTxOut (CardanoLedgerEra era)) =>
  L.Addr (L.EraCrypto (CardanoLedgerEra era)) ->
  ClbT era m (L.UTxO (CardanoLedgerEra era))
getUtxosAt addr = do
  allUTxOs <- gets getUtxosAtState
  let hasAddr txOut = txOut ^. L.addrTxOutL == addr
  pure . L.UTxO $ Map.filter hasAddr (L.unUTxO allUTxOs)

getUtxosAtState :: ClbState era -> L.UTxO (CardanoLedgerEra era)
getUtxosAtState state =
  L.utxosUtxo $
    L.lsUTxOState $
      _memPoolState $
        _emulatedLedgerState state

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: forall era m. (Monad m, IsCardanoLedgerEra era) => C.AddressInEra era -> ClbT era m [P.TxOutRef]
txOutRefAt addr = gets (txOutRefAtState @era $ C.toShelleyAddr addr)

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: (IsCardanoLedgerEra era) => L.Addr L.StandardCrypto -> ClbState era -> [P.TxOutRef]
txOutRefAtState addr st = transTxIn <$> M.keys (M.filter atAddr utxos)
  where
    utxos = L.unUTxO $ getUtxosAtState st
    atAddr out = case out ^. Core.addrEitherTxOutL of
      Right cAddr -> L.compactAddr addr == cAddr
      -- Left should never happen (abuse of Either in fact)
      Left addr' -> addr == addr'

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCred :: (IsCardanoLedgerEra era) => P.Credential -> Clb era [P.TxOutRef]
txOutRefAtPaymentCred cred = gets (txOutRefAtPaymentCredState cred)

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCredState :: (IsCardanoLedgerEra era) => P.Credential -> ClbState era -> [P.TxOutRef]
txOutRefAtPaymentCredState cred st = fmap transTxIn . M.keys $ M.filter withCred utxos
  where
    withCred out = case out ^. Core.addrEitherTxOutL of
      Right addr -> addrHasCred $ L.decompactAddr addr
      Left addr -> addrHasCred addr
    addrHasCred (L.Addr _ paymentCred _) = transCred paymentCred == cred
    -- Not considering byron addresses.
    addrHasCred _ = False
    utxos = L.unUTxO $ getUtxosAtState st

getEpochInfo :: (Monad m) => ClbT era m (EpochInfo (Either Text))
getEpochInfo =
  gets (slotConfigToEpochInfo . clbConfigSlotConfig . _clbConfig)

getGlobals :: forall era m. (Monad m, IsCardanoLedgerEra era) => ClbT era m Globals
getGlobals = do
  pparams <- gets (clbConfigProtocol . _clbConfig)
  let majorVer = L.pvMajor $ view L.ppProtocolVersionL pparams
  epochInfo <- getEpochInfo
  pure $
    L.mkShelleyGlobals
      emulatorShelleyGenesisDefaults
      epochInfo
      majorVer

-- | Run `applyTx`, if succeed update state and record datums
sendTx :: forall era m. (Monad m, IsCardanoLedgerEra era) => C.Tx era -> ClbT era m (ValidationResult era)
sendTx apiTx@(C.ShelleyTx _ tx) = do
  state@ClbState {_emulatedLedgerState} <- get
  globals <- getGlobals
  case applyTx ValidateAll globals _emulatedLedgerState tx of
    Right (newState, vtx) -> do
      put $ state {_emulatedLedgerState = newState}
      recordNewDatums
      return $ Success newState vtx
    Left err -> return $ Fail tx err
  where
    recordNewDatums = do
      state@ClbState {_mockDatums} <- get
      let txDatums = scriptDataFromCardanoTxBody $ C.getTxBody apiTx
      put $
        state
          { _mockDatums = M.union _mockDatums txDatums
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

dumpUtxoState :: (IsCardanoLedgerEra era) => Clb era ()
dumpUtxoState = do
  s <- gets ((^. memPoolState) . _emulatedLedgerState)
  logInfo $ LogEntry Info $ ppShow s

--------------------------------------------------------------------------------
-- Helpers for working with emulator traces
--------------------------------------------------------------------------------

{- | Checks that script runs without errors and returns pretty printed failure
 if something bad happens.
-}
checkErrors :: Clb era (Maybe String)
checkErrors = do
  failures <- fromLog <$> getFails
  pure $
    if null failures
      then Nothing
      else Just (init . unlines $ fmap show failures)

-- | Return list of failures
getFails :: Clb era (Log FailReason)
getFails = gets _mockFails

--------------------------------------------------------------------------------
-- Transactions validation TODO: factor out
--------------------------------------------------------------------------------

-- | Code copy-pasted from ledger's `applyTx` to use custom `ApplySTSOpts`
applyTx ::
  forall era stsUsed.
  (stsUsed ~ EraRule "LEDGER" (CardanoLedgerEra era), IsCardanoLedgerEra era) =>
  ValidationPolicy ->
  L.Globals ->
  EmulatedLedgerState era ->
  Core.Tx (CardanoLedgerEra era) ->
  Either (L.ApplyTxError (CardanoLedgerEra era)) (EmulatedLedgerState era, OnChainTx era)
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
intToKeyPair :: Integer -> TL.KeyPair r L.StandardCrypto
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
intToCardanoSk n = case intToKeyPair n of
  TL.KeyPair _ sk -> C.PaymentSigningKey sk

waitSlot :: SlotNo -> Clb era ()
waitSlot slot = do
  currSlot <- getCurrentSlot
  -- TODO: shall we throw?
  when (currSlot < slot) $
    modify $
      \s@ClbState {_emulatedLedgerState = state} ->
        s {_emulatedLedgerState = setSlot slot state}

-- Lenses
makeLenses ''ClbState

-- Additional actions

modifySlot :: (Monad m) => (Slot -> Slot) -> ClbT era m Slot
modifySlot f = do
  logInfo $ LogEntry Info "modify slot..."
  newSlot <- f . toSlot <$> getCurrentSlot
  modify (over emulatedLedgerState (setSlot $ fromSlot newSlot))
  return newSlot

toSlot :: SlotNo -> Slot
toSlot = Slot . fromIntegral . L.unSlotNo

fromSlot :: Slot -> SlotNo
fromSlot = fromIntegral . TimeSlot.getSlot

{- | Evaluates all Txs in the pool
Add them to chain if Successful, otherwise just dump them.
Thus creating new Block
Then updates currentBlock @EmulatedLedgerState to latest Block
-}
processBlock :: (Monad m, IsCardanoLedgerEra era) => ClbT era m (Block era)
processBlock = do
  logInfo $ LogEntry Info "process block..."
  poolTxs <- gets _txPool
  newBlock <- catMaybes <$> mapM (processSingleTx . getEmulatorEraTx) poolTxs
  modify (over txPool (const mempty)) -- Clears txPool
  modify (over emulatedLedgerState (over currentBlock (const newBlock))) -- Update to latest Block
  return newBlock

processSingleTx :: (Monad m, IsCardanoLedgerEra era) => C.Tx era -> ClbT era m (Maybe (OnChainTx era))
processSingleTx tx = validateTx tx >>= commitTx

validateTx :: (Monad m, IsCardanoLedgerEra era) => C.Tx era -> ClbT era m (ValidationResult era)
validateTx (C.ShelleyTx _ tx) = do
  ClbState {_emulatedLedgerState} <- get
  globals <- getGlobals
  case applyTx ValidateAll globals _emulatedLedgerState tx of
    Right (newState, vtx) -> return $ Success newState vtx
    Left err -> return $ Fail tx err

commitTx :: (Monad m, IsCardanoLedgerEra era) => ValidationResult era -> ClbT era m (Maybe (OnChainTx era))
commitTx (Success newState vtx) = do
  state@ClbState {_mockDatums} <- get
  let apiTx = C.ShelleyTx shelleyBasedEra (L.extractTx $ getOnChainTx vtx)
  let txDatums = scriptDataFromCardanoTxBody $ C.getTxBody apiTx
  put $ state {_emulatedLedgerState = newState, _mockDatums = M.union _mockDatums txDatums}
  pure $ Just vtx
commitTx (Fail _tx err) = do
  logError $
    "Transaction Failed: "
      <> show err
  -- TODO: Should we include transaction details ?
  -- <> " - Transaction Details: "
  -- <> show tx
  pure Nothing

addTxToPool :: CardanoTx era -> ClbState era -> ClbState era
addTxToPool tx = over txPool (tx :)
