module Main where

import Clb (ClbState, mockInfo, ppLog)
import ClbSocket (EmulatorConfig, ServerConfig, runEmulator, runServer)
import Prettyprinter (LayoutOptions (layoutPageWidth), PageWidth (AvailablePerLine), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)

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
getServerConfig = undefined

getEmulatorConfig :: IO EmulatorConfig
getEmulatorConfig = undefined
