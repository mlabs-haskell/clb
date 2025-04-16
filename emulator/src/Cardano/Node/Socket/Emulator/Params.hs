module Cardano.Node.Socket.Emulator.Params where

import Cardano.Api (ConwayEra)
import Cardano.Api.Shelley (ShelleyGenesis)
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Node.Socket.Emulator.Types
import Clb (ClbConfig)
import Clb.Config (paramsFromConfig)
import Clb.Params (emulatorAlonzoGenesisDefaults, emulatorConwayGenesisDefaults, emulatorShelleyGenesisDefaults)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL

type ShelleyConfigUpdater = ShelleyGenesis -> ShelleyGenesis

fromNodeServerConfig :: ShelleyConfigUpdater -> NodeServerConfig -> IO (ClbConfig ConwayEra)
fromNodeServerConfig updateShelley NodeServerConfig {nscShelleyGenesisPath, nscAlonzoGenesisPath, nscConwayGenesisPath} = do
  shelleyConfig <- readConfig emulatorShelleyGenesisDefaults nscShelleyGenesisPath
  alonzoConfig <- readConfig emulatorAlonzoGenesisDefaults nscAlonzoGenesisPath
  conwayConfig <- readConfig emulatorConwayGenesisDefaults nscConwayGenesisPath
  pure $
    paramsFromConfig $
      L.mkLatestTransitionConfig (updateShelley shelleyConfig) alonzoConfig conwayConfig

readConfig :: (FromJSON a) => a -> Maybe FilePath -> IO a
readConfig a = maybe (pure a) readPP
  where
    readPP path = do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err ->
          error $
            "Error reading JSON file: "
              ++ show path
              ++ " ("
              ++ err
              ++ ")"
        Right params -> pure params
