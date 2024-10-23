{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.Node.Socket.Emulator (
  main,
  prettyTrace,
  startTestnet,
) where

import Cardano.Api (NetworkId, NetworkMagic (NetworkMagic), toNetworkMagic)
import Cardano.Api.Genesis (ShelleyGenesis (sgNetworkMagic, sgSlotLength))
import Cardano.Api.NetworkId qualified as C
import Cardano.BM.Trace (Trace, stdoutTrace)
import Cardano.Node.Socket.Emulator.Mock (slotCoordinator)
import Cardano.Node.Socket.Emulator.Params qualified as Params
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  CNSEServerLogMsg (..),
  NodeServerConfig (..),
  initialChainState,
 )
import Clb.Config (ClbConfig (clbConfigSlotConfig), clbConfigNetworkId, keptBlocks)
import Clb.TimeSlot (SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Map.Strict qualified as Map
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
  nodeServerConfig@NodeServerConfig
    { nscInitialTxWallets
    , nscSocketPath
    }
  updateShelley =
    LM.runLogEffects trace $ do
      params <- liftIO $ Params.fromNodeServerConfig updateShelley nodeServerConfig
      initialState <- initialChainState params
      let appState = AppState initialState mempty params
      serverHandler <-
        liftIO $
          Server.runServerNode
            (LM.convertLog ProcessingEmulatorMsg trace)
            (toNetworkMagic $ clbConfigNetworkId params)
            nscSocketPath
            (keptBlocks params)
            appState

      let slotConfig@SlotConfig {scSlotZeroTime, scSlotLength} = clbConfigSlotConfig params
      logInfo $
        StartingSlotCoordination
          (posixSecondsToUTCTime $ realToFrac scSlotZeroTime / 1_000)
          (fromInteger scSlotLength :: Millisecond)

      void $ liftIO $ forkIO $ slotCoordinator slotConfig serverHandler

      logInfo StartingCNSEServer
      liftIO $ forever $ threadDelay 10_000_000

prettyTrace :: Trace IO CNSEServerLogMsg
prettyTrace = LM.convertLog (renderStrict . layoutPretty defaultLayoutOptions . pretty) stdoutTrace

startTestnet :: FilePath -> Integer -> NetworkId -> IO ()
startTestnet socketPath slotLength networkId =
  let
    updateShelley config =
      config
        { sgSlotLength = fromIntegral slotLength / 1000.0
        , sgNetworkMagic = case toNetworkMagic networkId of NetworkMagic nm -> nm
        }
   in
    core prettyTrace def {nscSocketPath = socketPath} updateShelley
