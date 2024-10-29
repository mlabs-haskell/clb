{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Socket.Emulator.Query (HandleQuery (..)) where

import Cardano.Api qualified as Api
import Cardano.Api.Eras qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Ledger.BaseTypes (epochInfo)
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Ledger.UTxO qualified as L
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  getTip,
  runChainEffects,
 )
import Cardano.Slotting.EpochInfo (epochInfoEpoch)
import Cardano.Slotting.Slot (WithOrigin (..))
import Cardano.Slotting.Time (RelativeTime (RelativeTime), SlotLength, mkSlotLength)
import Clb (ClbT, emulatedLedgerState, getClbConfig, getCurrentSlot, getGlobals, getStakePools, getUtxosAt, logInfo, txOutRefAt)
import Clb.ClbLedgerState (ledgerState)
import Clb.Config (ClbConfig (..))
import Clb.TimeSlot (
  SlotConfig (scSlotZeroTime),
  emulatorEpochSize,
  posixTimeToNominalDiffTime,
  posixTimeToUTCTime,
  scSlotLength,
 )
import Control.Concurrent (MVar, readMVar)
import Control.Lens (alaf, use, view)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map qualified as Map
import Data.Monoid (Ap (Ap))
import Data.SOP (K (K))
import Data.SOP.Counting qualified as Ouroboros
import Data.SOP.NonEmpty qualified as Ouroboros
import Data.SOP.Strict (NP (Nil, (:*)), NS (S, Z))
import Data.Set qualified as Set
import Ouroboros.Consensus.Block (EpochNo (EpochNo), EpochSize, GenesisWindow (..), SlotNo (SlotNo))
import Ouroboros.Consensus.Cardano.Block (BlockQuery (..), CardanoBlock)
import Ouroboros.Consensus.HardFork.Combinator (QueryHardFork (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.History (EraParams (..))
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History.Summary (
  Bound (..),
  EraSummary (..),
  Summary (..),
 )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra, StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (..))
import Ouroboros.Network.Block qualified as O
import PlutusLedgerApi.V1 (POSIXTime (POSIXTime, getPOSIXTime))

class HandleQuery era where
  handleQuery ::
    (block ~ CardanoBlock StandardCrypto) =>
    MVar (AppState era) ->
    Query block result ->
    IO result

instance HandleQuery C.ConwayEra where
  handleQuery = handleQueryConwayEra

handleQueryConwayEra ::
  (block ~ CardanoBlock StandardCrypto) =>
  MVar (AppState C.ConwayEra) ->
  Query block result ->
  IO result
handleQueryConwayEra state = \case
  query@(BlockQuery (QueryIfCurrentConway q)) -> do
    putStrLn $ "1 - Query was received: " ++ show query
    (_logs, res) <- runChainEffects state $ queryIfCurrentConway q
    either (printError . show) (pure . Right) res
  query@(BlockQuery (QueryHardFork GetInterpreter)) -> do
    putStrLn $ "2 - Query was received: " ++ show query
    AppState _ _ config <- readMVar state
    let C.EraHistory interpreter = emulatorEraHistory config
    pure interpreter
  query@(BlockQuery (QueryHardFork GetCurrentEra)) -> do
    putStrLn $ "3- Query was received: " ++ show query
    pure $ Consensus.EraIndex (S (S (S (S (S (S (Z (K ())))))))) -- ConwayEra
  BlockQuery q -> printError $ "Unimplemented BlockQuery received: " ++ show q
  query@GetSystemStart -> do
    putStrLn $ "4 - Query was received: " ++ show query
    AppState _ _ ClbConfig {clbConfigSlotConfig} <- readMVar state
    pure $ C.SystemStart $ posixTimeToUTCTime $ scSlotZeroTime clbConfigSlotConfig
  query@GetChainBlockNo -> do
    putStrLn $ "5 - Query was received: " ++ show query
    tip <- getTip state
    case tip of
      O.TipGenesis -> pure Origin
      (O.Tip _ _ curBlockNo) -> pure $ At curBlockNo
  query@GetChainPoint -> printError "Unimplemented: GetChainPoint"

-- q -> do
--   printError $ "Query was received: " ++ show q

queryIfCurrentConway ::
  (block ~ Shelley.ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)) =>
  BlockQuery block result ->
  ClbT C.ConwayEra IO result
queryIfCurrentConway = \case
  GetGenesisConfig -> Shelley.compactGenesis . view L.tcShelleyGenesisL . clbConfigConfig <$> getClbConfig
  GetCurrentPParams -> clbConfigProtocol <$> getClbConfig
  GetEpochNo -> do
    ei <- epochInfo <$> getGlobals
    slotNo <- getCurrentSlot
    case epochInfoEpoch ei slotNo of
      Left err -> printError $ "Error calculating epoch: " ++ show err
      Right epoch -> pure epoch
  GetStakePools -> getStakePools
  GetUTxOByAddress addrs -> let f b addr = (b <>) <$> getUtxosAt addr in foldM f mempty addrs
  GetUTxOByTxIn txIns -> C.toLedgerUTxO C.ShelleyBasedEraConway <$> utxosAtTxIns (Set.map C.fromShelleyTxIn txIns)
  q -> printError $ "Unimplemented BlockQuery(QueryIfCurrentConway) received: " ++ show q

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
    last =
      Ouroboros.nonEmptyHead $
        Ouroboros.getSummary $
          skipSummary lastEndBound emulatorEpochSize (slotLength params) emulatorGenesisWindow
    list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K last :* Nil

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

lastEndBound :: Bound
lastEndBound =
  Bound
    { boundTime = RelativeTime 50
    , boundSlot = SlotNo 1
    , boundEpoch = EpochNo 500
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
  idx <- use (emulatedLedgerState . ledgerState . L.lsUTxOStateL . L.utxosUtxoL)
  pure $ foldMap (\txIn -> maybe mempty (singletonUTxO txIn) $ lookupUTxO txIn idx) txIns
