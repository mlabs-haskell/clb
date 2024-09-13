module Main (main) where

import Cardano.Node.Socket.Emulator qualified as NodeServer
import Cardano.Node.Socket.Emulator.CLI (buildConfig)
import Cardano.Node.Socket.Emulator.Types (NodeServerConfig)
import Data.Default (def)
import Data.List (foldl')
import Data.Map (fromList)
import Debug.Trace (trace)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  buildConfig >>= NodeServer.main NodeServer.prettyTrace
