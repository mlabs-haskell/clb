{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Node.Socket.Emulator.CLI (buildConfig) where

import Cardano.Node.Socket.Emulator.Configuration (EmulatorConfiguration (..))
import Cardano.Node.Socket.Emulator.Types (NodeServerConfig (..), defaultNodeServerConfig)
import Data.Aeson (FromJSON, ToJSON)
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)
import Options.Applicative (
  ParseError (ExpectsArgError),
  Parser,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  noArgError,
  option,
  optional,
  progDesc,
  strOption,
  (<**>),
 )

-- Main function to parse the command line arguments
buildConfig :: IO NodeServerConfig
buildConfig = do
  RunCommand opts <- execParser optsParser
  buildNodeServerConfig opts
  where
    optsParser =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Start a node server with the given configuration"
            <> header "Node Server Configuration Parser"
        )

-- Function to convert NodeServerOptions to NodeServerConfig
buildNodeServerConfig :: NodeServerRunOptions -> IO NodeServerConfig
buildNodeServerConfig RunOptions {..} = do
  EmulatorConfiguration {..} <- decodeFileThrow nsoConfig
  -- lcd <- getCurrentDirectory
  -- putStrLn $ "Current directory: " <> lcd
  let config =
        defaultNodeServerConfig
          { nscSocketPath = nsoSocketPath
          , nscShelleyGenesisPath = Just ecShelleyGenesisFile
          , nscAlonzoGenesisPath = Just ecAlonzoGenesisFile
          , nscConwayGenesisPath = Just ecConwayGenesisFile
          }
  print config
  pure config

-- | Data type representing the possible commands
newtype NodeServerCommands
  = RunCommand NodeServerRunOptions -- Represents the `run` command and its options
  deriving stock (Show, Eq, Generic)

-- | Parser for NodeServerCommands
commandParser :: Parser NodeServerCommands
commandParser =
  hsubparser
    ( command
        "run"
        ( info
            (RunCommand <$> nodeServerOptionsParser)
            (progDesc "Run the server with the given configuration")
        )
    )

-- | Data type for the `run` command options
data NodeServerRunOptions = RunOptions
  { nsoConfig :: FilePath
  , nsoTopology :: Maybe FilePath
  , nsoDatabasePath :: Maybe FilePath
  , nsoShelleyKESKey :: Maybe FilePath
  , nsoShelleyVRFKey :: Maybe FilePath
  , nsoByronDelegationCert :: Maybe FilePath
  , nsoByronSigningKey :: Maybe FilePath
  , nsoShelleyOpCert :: Maybe FilePath
  , nsoSocketPath :: FilePath
  , nsoPort :: Maybe Int
  , nsoHostAddr :: Maybe String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Parser for NodeServerRunOptions
nodeServerOptionsParser :: Parser NodeServerRunOptions
nodeServerOptionsParser =
  RunOptions
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> noArgError (ExpectsArgError "--config")
          <> help "Path to the configuration file"
      )
    <*> optional
      ( strOption
          ( long "topology"
              <> metavar "TOPOLOGY"
              <> help "Path to the topology file"
          )
      )
    <*> optional
      ( strOption
          ( long "database-path"
              <> metavar "DATABASE_PATH"
              <> help "Path to the database"
          )
      )
    <*> optional
      ( strOption
          ( long "shelley-kes-key"
              <> metavar "KES_KEY"
              <> help "Path to the Shelley KES key"
          )
      )
    <*> optional
      ( strOption
          ( long "shelley-vrf-key"
              <> metavar "VRF_KEY"
              <> help "Path to the Shelley VRF key"
          )
      )
    <*> optional
      ( strOption
          ( long "byron-delegation-certificate"
              <> metavar "DELEGATION_CERT"
              <> help "Path to the Byron delegation certificate"
          )
      )
    <*> optional
      ( strOption
          ( long "byron-signing-key"
              <> metavar "SIGNING_KEY"
              <> help "Path to the Byron signing key"
          )
      )
    <*> optional
      ( strOption
          ( long "shelley-operational-certificate"
              <> metavar "OPERATIONAL_CERT"
              <> help "Path to the Shelley operational certificate"
          )
      )
    <*> strOption
      ( long "socket-path"
          <> metavar "SOCKET_PATH"
          <> noArgError (ExpectsArgError "--socket-path")
          <> help "Path to the socket used to communicate with the server"
      )
    <*> optional
      ( option
          auto
          ( long "port"
              <> metavar "PORT"
              <> help "Port number for the server"
          )
      )
    <*> optional
      ( strOption
          ( long "host-addr"
              <> metavar "HOST_ADDR"
              <> help "Host address for the server"
          )
      )
