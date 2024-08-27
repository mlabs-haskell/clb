module Main (main) where

import Cardano.Node.Socket.Emulator qualified as NodeServer
import Data.Default (def)

main :: IO ()
main = NodeServer.main NodeServer.prettyTrace def
