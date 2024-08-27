{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Socket.Emulator.Params where

import Cardano.Api.Genesis (ShelleyGenesis)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Node.Socket.Emulator.Types
import Clb (MockConfig, defaultBabbage)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL

type ShelleyConfigUpdater = ShelleyGenesis StandardCrypto -> ShelleyGenesis StandardCrypto

-- FIXME: build the configuration, rename to parameters
fromNodeServerConfig :: ShelleyConfigUpdater -> NodeServerConfig -> IO MockConfig
fromNodeServerConfig
  _updateShelley
  _ =
    -- NodeServerConfig
    --   { nscShelleyGenesisPath
    --   , nscAlonzoGenesisPath
    --   , nscConwayGenesisPath}
    do
      -- shelleyConfig <- readConfig emulatorShelleyGenesisDefaults nscShelleyGenesisPath
      -- alonzoConfig <- readConfig emulatorAlonzoGenesisDefaults nscAlonzoGenesisPath
      -- conwayConfig <- readConfig emulatorConwayGenesisDefaults nscConwayGenesisPath
      -- pure $
      --   paramsFromConfig $
      --     mkLatestTransitionConfig (updateShelley shelleyConfig) alonzoConfig conwayConfig
      pure defaultBabbage

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
