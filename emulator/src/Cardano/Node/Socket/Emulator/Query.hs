{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Cardano.Node.Socket.Emulator.Query (handleQuery) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api.Transition qualified as C
import Cardano.Ledger.BaseTypes (epochInfo)
import Cardano.Slotting.EpochInfo (epochInfoEpoch)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Concurrent (MVar, readMVar)
import Control.Lens (alaf, view)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Monoid (Ap (Ap))
import Data.SOP (K (K))
import Data.SOP.Counting qualified as Ouroboros
import Data.SOP.NonEmpty qualified as Ouroboros
import Data.SOP.Strict (NP (Nil, (:*)), NS (S, Z))
import Data.Set qualified as Set

-- import Ledger.Tx.CardanoAPI (fromPlutusIndex)
import Ouroboros.Consensus.Cardano.Block (BlockQuery (..), CardanoBlock)
import Ouroboros.Consensus.HardFork.Combinator (QueryHardFork (..))
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra, StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (..))
import Ouroboros.Network.Block qualified as O

-- import Cardano.Node.Emulator.API qualified as E
-- import Cardano.Node.Emulator.Internal.Node.Params (
--   Params (..),
--   emulatorEraHistory,
--   emulatorGlobals,
--   emulatorPParams,
--  )
-- import Cardano.Node.Emulator.Internal.Node.TimeSlot (posixTimeToUTCTime, scSlotZeroTime)
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  getTip,
  -- runChainEffects,
 )
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Clb.MockConfig (MockConfig (..))
import Clb.Params (emulatorEpochSize)
import Clb.TimeSlot (
  SlotConfig (scSlotZeroTime),
  posixTimeToNominalDiffTime,
  posixTimeToUTCTime,
  scSlotLength,
 )
import Ouroboros.Consensus.Block (GenesisWindow (..))
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import PlutusLedgerApi.V1 (POSIXTime (POSIXTime, getPOSIXTime))

handleQuery ::
  (block ~ CardanoBlock StandardCrypto) =>
  MVar (AppState era) ->
  Query block result ->
  IO result
handleQuery state = \case
  -- FIXME:
  -- BlockQuery (QueryIfCurrentConway q) -> do
  --   (_logs, res) <- runChainEffects state $ queryIfCurrentConway q
  --   either (printError . show) (pure . Right) res
  BlockQuery (QueryHardFork GetInterpreter) -> do
    AppState _ _ config <- readMVar state
    let C.EraHistory interpreter = emulatorEraHistory config
    pure interpreter
  BlockQuery (QueryHardFork GetCurrentEra) -> do
    pure $ Consensus.EraIndex (S (S (S (S (S (S (Z (K ())))))))) -- ConwayEra
  BlockQuery q -> printError $ "Unimplemented BlockQuery received: " ++ show q
  GetSystemStart -> do
    AppState _ _ MockConfig {mockConfigSlotConfig} <- readMVar state
    pure $ C.SystemStart $ posixTimeToUTCTime $ scSlotZeroTime mockConfigSlotConfig
  GetChainBlockNo -> do
    tip <- getTip state
    case tip of
      O.TipGenesis -> pure Origin
      (O.Tip _ _ curBlockNo) -> pure $ At curBlockNo
  GetChainPoint -> printError "Unimplemented: GetChainPoint"

-- FIXME:
-- queryIfCurrentConway ::
--   (block ~ Shelley.ShelleyBlock (Praos StandardCrypto) (ConwayEra StandardCrypto)) =>
--   BlockQuery block result ->
--   E.EmulatorT IO result
-- queryIfCurrentConway = \case
--   GetGenesisConfig -> Shelley.compactGenesis . view C.tcShelleyGenesisL . E.pConfig <$> E.getParams
--   GetCurrentPParams -> emulatorPParams <$> E.getParams
--   GetEpochNo -> do
--     ei <- epochInfo . emulatorGlobals <$> E.getParams
--     slotNo <- E.currentSlot
--     case epochInfoEpoch ei (fromIntegral slotNo) of
--       Left err -> printError $ "Error calculating epoch: " ++ show err
--       Right epoch -> pure epoch
--   GetStakePools -> pure mempty
--   GetUTxOByAddress addrs ->
--     fromPlutusIndex <$> alaf Ap foldMap (E.utxosAt . C.fromShelleyAddrIsSbe C.shelleyBasedEra) addrs
--   GetUTxOByTxIn txIns -> fromPlutusIndex <$> E.utxosAtTxIns (Set.map C.fromShelleyTxIn txIns)
--   q -> printError $ "Unimplemented BlockQuery(QueryIfCurrentConway) received: " ++ show q

printError :: (MonadIO m) => String -> m a
printError s = liftIO (print s) >> error s

-- | A sensible default 'EraHistory' value for the emulator
emulatorEraHistory :: MockConfig era -> C.EraHistory
emulatorEraHistory params = C.EraHistory (Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly list)
  where
    one =
      Ouroboros.nonEmptyHead $
        Ouroboros.getSummary $
          -- Ouroboros.neverForksSummary (pEpochSize params) (slotLength params) emulatorGenesisWindow
          Ouroboros.neverForksSummary emulatorEpochSize (slotLength params) emulatorGenesisWindow
    list = Ouroboros.Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil

emulatorGenesisWindow :: GenesisWindow
emulatorGenesisWindow = GenesisWindow window
  where
    -- A good default value for eras that never fork is
    -- 3k/f, with k = 2160 and f = 20 (given by the Genesis team).
    window = (3 * 2160) `div` 20

-- | Calculate the cardano-ledger `SlotLength`
slotLength :: MockConfig era -> SlotLength
slotLength MockConfig {mockConfigSlotConfig} =
  mkSlotLength $ posixTimeToNominalDiffTime $ POSIXTime $ scSlotLength mockConfigSlotConfig
