module Cardano.Node.Socket.Emulator.Params where

import Cardano.Api (ConwayEra)
import Cardano.Api.Genesis (ShelleyGenesis)
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Node.Socket.Emulator.Types
import Clb (MockConfig, defaultBabbage)
import Clb.MockConfig (defaultConwayTransitionConfig, paramsFromConfig)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL

type ShelleyConfigUpdater = ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto

fromNodeServerConfig :: ShelleyConfigUpdater -> NodeServerConfig -> IO (MockConfig ConwayEra)
fromNodeServerConfig updateShelley NodeServerConfig {nscShelleyGenesisPath, nscAlonzoGenesisPath, nscConwayGenesisPath} = do
  -- FIXME: use L.mkLatestTransitionConfig
  -- shelleyConfig <- readConfig emulatorShelleyGenesisDefaults nscShelleyGenesisPath
  -- alonzoConfig <- readConfig emulatorAlonzoGenesisDefaults nscAlonzoGenesisPath
  -- conwayConfig <- readConfig emulatorConwayGenesisDefaults nscConwayGenesisPath
  pure $
    -- L.mkLatestTransitionConfig (updateShelley shelleyConfig) alonzoConfig conwayConfig
    paramsFromConfig defaultConwayTransitionConfig

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
