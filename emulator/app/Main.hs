module Main (main) where

import Cardano.Node.Socket.Emulator qualified as NodeServer
import Cardano.Node.Socket.Emulator.CLI (buildConfig)

main :: IO ()
main = do
  buildConfig >>= NodeServer.main NodeServer.prettyTrace
