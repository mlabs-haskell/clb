{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.Node.Socket.Emulator (
  main,
  prettyTrace,
  -- startTestnet,
) where

import Cardano.Api (toNetworkMagic)
import Cardano.BM.Trace (Trace, stdoutTrace)
import Cardano.Node.Socket.Emulator.Params qualified as Params
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Cardano.Node.Socket.Emulator.SlotCoordinator (slotCoordinator)
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  CNSEServerLogMsg (..),
  NodeServerConfig (..),
  initialChainState,
 )
import Clb.Config (ClbConfig (clbConfigSlotConfig), clbConfigNetworkId, keptBlocks)
import Clb.TimeSlot (SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Units (Millisecond)
import Plutus.Monitoring.Util qualified as LM
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

main :: Trace IO CNSEServerLogMsg -> NodeServerConfig -> IO ()
main trace nodeServerConfig = core trace nodeServerConfig id

core :: Trace IO CNSEServerLogMsg -> NodeServerConfig -> Params.ShelleyConfigUpdater -> IO ()
core
  trace
  nodeServerConfig@NodeServerConfig {nscSocketPath}
  updateShelley =
    LM.runLogEffects trace $ do
      logInfo $ ProcessingEmulatorMsg "Welcome to cardano-node emulator"
      params <- liftIO $ Params.fromNodeServerConfig updateShelley nodeServerConfig
      initialState <- initialChainState params
      let appState = AppState initialState
      let trace' = LM.convertLog ProcessingEmulatorMsg trace
      serverHandler <-
        liftIO $
          Server.runServerNode
            trace'
            (toNetworkMagic $ clbConfigNetworkId params)
            nscSocketPath
            (keptBlocks params)
            appState

      let slotConfig@SlotConfig {scSlotZeroTime, scSlotLength} = clbConfigSlotConfig params
      logInfo $
        StartingSlotCoordination
          (posixSecondsToUTCTime $ realToFrac scSlotZeroTime / 1_000)
          (fromInteger scSlotLength :: Millisecond)

      void $ liftIO $ forkIO $ slotCoordinator trace' slotConfig serverHandler

      logInfo StartingCNSEServer
      liftIO $ forever $ threadDelay 10_000_000

prettyTrace :: Trace IO CNSEServerLogMsg
prettyTrace =
  LM.convertLog
    (renderStrict . layoutPretty defaultLayoutOptions . pretty)
    stdoutTrace
