module Main where

import Cardano.Api (Lovelace (Lovelace), lovelaceToValue)
import Clb (
  ClbState (_mockInfo),
  checkErrors,
  defaultBabbage,
  dumpUtxoState,
  initClb,
  ppLog,
  runClb,
 )
import Prettyprinter
import Prettyprinter.Render.String (renderString)

main :: IO ()
main = do
  putStrLn "Welcome to the smoke-test CLB harness!"
  let _dummyTotalNotUsedNow = lovelaceToValue $ Lovelace 1_000_000_000_000
  let perWallet = lovelaceToValue $ Lovelace 1_000_000_000
  let (_mbErrors, clb) =
        runClb (dumpUtxoState >> checkErrors) $
          initClb defaultBabbage _dummyTotalNotUsedNow perWallet
  let logDoc = ppLog $ _mockInfo clb
  let options = defaultLayoutOptions {layoutPageWidth = AvailablePerLine 150 1.0}
  let logString = renderString $ layoutPretty options logDoc
  let mockLog = "\nEmulator log :\n--------------\n" <> logString
  putStrLn mockLog
