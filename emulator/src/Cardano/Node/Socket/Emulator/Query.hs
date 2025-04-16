{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Socket.Emulator.Query (HandleQuery (..)) where

import Cardano.Api qualified as Api
import Cardano.Api.Internal.Eras qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.Trace (Trace)
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Ledger.BaseTypes (epochInfo)
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Ledger.UTxO qualified as L
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  EmulatorMsg,
  clbState,
  getChainPointTime,
  getTip,
  runClbInIO',
  socketEmulatorState,
 )
import Cardano.Slotting.EpochInfo (epochInfoEpoch)
import Cardano.Slotting.Slot (WithOrigin (..))
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Clb (ClbT, chainState, clbConfig, getClbConfig, getCurrentSlot, getGlobals, getStakePools, getUtxosAt)
import Clb qualified as E
import Clb.Config (ClbConfig (..))
import Clb.EmulatedLedgerState (ledgerState)
import Clb.TimeSlot (
  SlotConfig (scSlotZeroTime),
  emulatorEpochSize,
  posixTimeToNominalDiffTime,
  posixTimeToUTCTime,
  scSlotLength,
 )
import Control.Concurrent (MVar, readMVar)
import Control.Lens (use, view, (^.))
import Control.Monad (foldM)
import Control.Monad.Freer.Extras (logError, logInfo)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map qualified as Map
import Data.SOP (K (K))
import Data.SOP.Counting qualified as Ouroboros
import Data.SOP.NonEmpty qualified as Ouroboros
import Data.SOP.Strict (NP (Nil, (:*)), NS (S, Z))
import Data.Set qualified as Set
import Ouroboros.Consensus.Block (EpochSize, GenesisWindow (..))
import Ouroboros.Consensus.Cardano.Block (BlockQuery (..), CardanoBlock)
import Ouroboros.Consensus.HardFork.Combinator (QueryHardFork (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.History (EraParams (..))
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History.Summary (
  Bound (..),
  EraSummary (..),
  Summary (..),
  neverForksSummary,
 )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra, StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (..))
import Ouroboros.Network.Block qualified as O
import Plutus.Monitoring.Util (runLogEffects)
import PlutusLedgerApi.V1 (POSIXTime (POSIXTime))

class HandleQuery era where
  handleQuery ::
    (block ~ CardanoBlock StandardCrypto) =>
    Trace IO EmulatorMsg ->
    MVar (AppState era) ->
    Query block result ->
    IO result

instance HandleQuery C.ConwayEra where
  handleQuery = handleQueryConwayEra

handleQueryConwayEra ::
  (block ~ CardanoBlock StandardCrypto) =>
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  Query block result ->
  IO result
handleQueryConwayEra trace state q =
  runLogEffects trace $ do
    case q of
      query@(BlockQuery (QueryIfCurrentConway qicc)) -> do
        logInfo $ "Query was received (1): " ++ show query
        res <- liftIO $ runClbInIO' trace state $ queryIfCurrentConway qicc
        either (printError . show) (pure . Right) res
      query@(BlockQuery (QueryHardFork GetInterpreter)) -> do
        logInfo $ "Query was received (2): " ++ show query
        as <- liftIO $ readMVar state
        let config = as ^. (socketEmulatorState . clbState . clbConfig)
        let C.EraHistory interpreter = emulatorEraHistory config
        pure interpreter
      query@(BlockQuery (QueryHardFork GetCurrentEra)) -> do
        logInfo $ "Query was received (3): " ++ show query
        pure $ Consensus.EraIndex (S (S (S (S (S (S (Z (K ())))))))) -- ConwayEra
      BlockQuery bq -> do
        let msg = "Unimplemented BlockQuery was received: " ++ show bq
        logError msg
        printError msg
      query@GetSystemStart -> do
        logInfo $ "Query was received (4): " ++ show query
        as <- liftIO $ readMVar state
        let slotConfig = clbConfigSlotConfig $ as ^. (socketEmulatorState . clbState . clbConfig)
        let ss = C.SystemStart $ posixTimeToUTCTime $ scSlotZeroTime slotConfig
        logInfo $ "System start is: " <> show ss
        pure ss
      query@GetChainBlockNo -> do
        logInfo $ "Query was received (5): " ++ show query
        tip <- getTip state
        case tip of
          O.TipGenesis -> do
            logInfo "Tip is origin"
            pure Origin
          (O.Tip _ _ curBlockNo) -> do
            let ret = At curBlockNo
            logInfo $ "Tip is: " <> show ret
            pure ret
      query@GetChainPoint -> do
        logInfo $ "Query was received (5): " ++ show query
        ret <- getChainPointTime state
        logInfo $ "Chain point time is: " <> show ret
        pure ret

queryIfCurrentConway ::
  (block ~ Shelley.ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)) =>
  BlockQuery block result ->
  ClbT C.ConwayEra IO result
queryIfCurrentConway = \case
  GetGenesisConfig -> do
    E.logInfo $ E.mkDebug "Handling GetGenesisConfig..."
    Shelley.compactGenesis . view L.tcShelleyGenesisL . clbConfigConfig <$> getClbConfig
  GetCurrentPParams -> do
    E.logInfo $ E.mkDebug "Handling GetCurrentPParams..."
    clbConfigProtocol <$> getClbConfig
  GetEpochNo -> do
    E.logInfo $ E.mkDebug "Handling GetEpochNo..."
    ei <- epochInfo <$> getGlobals
    slotNo <- getCurrentSlot
    case epochInfoEpoch ei slotNo of
      Left err -> do
        let msg = "Error calculating epoch: " ++ show err
        E.logError msg
        printError msg
      Right epoch -> do
        E.logInfo (E.mkDebug $ "Epoch is: " <> show epoch)
        pure epoch
  GetStakePools -> do
    E.logInfo $ E.mkDebug "Hnadling GetStakePool"
    getStakePools
  GetUTxOByAddress addrs -> do
    E.logInfo $ E.mkDebug $ "Hnadling GetUTxOByAddress for addrs: " <> show addrs
    let f b addr = (b <>) <$> getUtxosAt addr in foldM f mempty addrs
  GetUTxOByTxIn txIns -> do
    E.logInfo $ E.mkDebug $ "Hnadling GetUTxOByTxIn for txIns: " <> show txIns
    C.toLedgerUTxO C.ShelleyBasedEraConway <$> utxosAtTxIns (Set.map C.fromShelleyTxIn txIns)
  q -> do
    let msg = "Unimplemented BlockQuery(QueryIfCurrentConway) received: " ++ show q
    E.logError msg
    printError msg

-- TODO: shall we get rig of it?
printError :: (MonadIO m) => String -> m a
printError s = liftIO (print s) >> error s

-- | A sensible default 'EraHistory' value for the emulator
emulatorEraHistory :: ClbConfig era -> C.EraHistory
emulatorEraHistory params = C.EraHistory (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list)
  where
    one =
      Ouroboros.nonEmptyHead $
        Ouroboros.getSummary $
          skipSummary Ouroboros.initBound emulatorEpochSize (slotLength params) emulatorGenesisWindow
    lastS =
      Ouroboros.nonEmptyHead $
        Ouroboros.getSummary $
          neverForksSummary emulatorEpochSize (slotLength params) emulatorGenesisWindow
    list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K lastS :* Nil

-- | 'Summary' for a ledger that never forks
skipSummary :: Bound -> EpochSize -> SlotLength -> GenesisWindow -> Summary '[x]
skipSummary endBound epochSize slotLen genesisWindow =
  Summary $
    Ouroboros.NonEmptyOne $
      EraSummary
        { eraStart = Ouroboros.initBound
        , eraEnd = Ouroboros.EraEnd endBound
        , eraParams =
            EraParams
              { eraEpochSize = epochSize
              , eraSlotLength = slotLen
              , eraSafeZone = Ouroboros.UnsafeIndefiniteSafeZone
              , eraGenesisWin = genesisWindow
              }
        }

emulatorGenesisWindow :: GenesisWindow
emulatorGenesisWindow = GenesisWindow window
  where
    -- A good default value for eras that never fork is
    -- 3k/f, with k = 2160 and f = 20 (given by the Genesis team).
    window = (3 * 2160) `div` 20

-- | Calculate the cardano-ledger `SlotLength`
slotLength :: ClbConfig era -> SlotLength
slotLength ClbConfig {clbConfigSlotConfig} =
  mkSlotLength $ posixTimeToNominalDiffTime $ POSIXTime $ scSlotLength clbConfigSlotConfig

-- Helper Functions

deriving newtype instance Semigroup (C.UTxO era)
deriving newtype instance Monoid (C.UTxO era)

-- | Find an unspent transaction output (using the Ledger type) by the 'TxIn' that spends it.
lookupUTxO :: C.TxIn -> L.UTxO (C.CardanoLedgerEra C.ConwayEra) -> Maybe (C.TxOut C.CtxUTxO C.ConwayEra)
lookupUTxO i index = C.fromShelleyTxOut C.ShelleyBasedEraConway <$> Map.lookup (C.toShelleyTxIn i) (L.unUTxO index)

-- | Create an index with a single UTxO.
singletonUTxO :: C.TxIn -> C.TxOut C.CtxUTxO C.ConwayEra -> C.UTxO C.ConwayEra
singletonUTxO txIn txOut = C.UTxO $ Map.singleton txIn txOut

-- | Query the unspent transaction outputs at the given transaction inputs.
utxosAtTxIns :: (Monad m, Foldable f) => f C.TxIn -> ClbT C.ConwayEra m (C.UTxO C.ConwayEra)
utxosAtTxIns txIns = do
  idx <- use (chainState . ledgerState . L.lsUTxOStateL . L.utxosUtxoL)
  pure $ foldMap (\txIn -> maybe mempty (singletonUTxO txIn) $ lookupUTxO txIn idx) txIns
