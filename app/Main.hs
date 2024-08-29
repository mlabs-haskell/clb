module Main where

import Clb (ClbState, mockInfo, ppLog)
import ClbSocket (ServerConfig (ServerConfig), getEmulatorConfig, runEmulator, runServer)
import Control.Concurrent.MVar (newEmptyMVar)
import Prettyprinter (LayoutOptions (layoutPageWidth), PageWidth (AvailablePerLine), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

main :: IO ()
main = do
  serverConfig <- getServerConfig
  runServer serverConfig

  emulatorConfig <- getEmulatorConfig
  finalClbState <- runEmulator emulatorConfig serverConfig

  putStrLn "\nEmulator log :\n--------------\n"
  printLog finalClbState

printLog :: ClbState -> IO ()
printLog clbState = do
  let logDoc = ppLog $ mockInfo clbState
  let options = defaultLayoutOptions {layoutPageWidth = AvailablePerLine 150 1.0}
  let logString = renderString $ layoutPretty options logDoc
  putStrLn logString

getServerConfig :: IO ServerConfig
getServerConfig =
  ServerConfig
    <$> createSocketPath "." "clb-emulator-control.socket"
    <*> createSocketPath "." "clb-emulator-comm.socket"
    <*> newEmptyMVar
    <*> newEmptyMVar

createSocketPath :: FilePath -> String -> IO FilePath
createSocketPath baseDir socketName = do
  let fullDir = baseDir </> "clb-emulator-socket"
  createDirectoryIfMissing True fullDir
  let socketPath = fullDir </> socketName
  return socketPath
