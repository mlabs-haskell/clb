module Main (main) where

import Cardano.Node.Socket.Emulator qualified as NodeServer
import Cardano.Node.Socket.Emulator.Types (NodeServerConfig)
import Data.Default (def)
import Data.List (foldl')
import Data.Map (fromList)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- parseEmulatorArgs <$> getArgs
  config <- case args of
    Left err -> die err
    Right config -> pure config
  NodeServer.main NodeServer.prettyTrace config

parseEmulatorArgs :: [String] -> Either String NodeServerConfig
parseEmulatorArgs ("run" : opts)
  | odd (length opts) = Left "Number of option arguments is wrong, should be even"
  | otherwise =
      let
        (_, names, values) =
          foldl'
            ( \(isName, ns, vs) sth ->
                if isName
                  then (not isName, sth : ns, vs)
                  else (not isName, ns, sth : vs)
            )
            (True, [], [])
            opts
        map = fromList $ zip names values
       in
        trace (show map) $ Right def
parseEmulatorArgs _ = Left "Only `run` command is supported."
