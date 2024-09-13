{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Socket.Emulator.Configuration where

import Data.Aeson (FromJSON (..), withObject, (.:))

data EmulatorConfiguration = EmulatorConfiguration
  { ecShelleyGenesisFile :: !FilePath
  , ecAlonzoGenesisFile :: !FilePath
  , ecConwayGenesisFile :: !FilePath
  }
  deriving (Eq, Show)

instance FromJSON EmulatorConfiguration where
  parseJSON = withObject "EmualatorConfiguration" $ \v -> do
    shelleyGenesisFile <- v .: "ShelleyGenesisFile"
    alonzoGenesisFile <- v .: "AlonzoGenesisFile"
    conwayGenesisFile <- v .: "ConwayGenesisFile"
    pure $
      EmulatorConfiguration
        shelleyGenesisFile
        alonzoGenesisFile
        conwayGenesisFile
