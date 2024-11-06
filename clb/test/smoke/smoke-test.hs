module Main where

import Cardano.Api.Value (lovelaceToValue)
import Cardano.Ledger.Coin (Coin (Coin))

import Clb (
  ClbState (_clbLog),
  checkErrors,
  defaultBabbageClbConfig,
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
  let _dummyTotalNotUsedNow = lovelaceToValue $ Coin 1_000_000_000_000
  let perWallet = lovelaceToValue $ Coin 1_000_000_000
  let (_mbErrors, clb) =
        runClb (dumpUtxoState >> checkErrors) $
          initClb defaultBabbageClbConfig _dummyTotalNotUsedNow perWallet Nothing
  let logDoc = ppLog $ _clbLog clb
  let options = defaultLayoutOptions {layoutPageWidth = AvailablePerLine 150 1.0}
  let logString = renderString $ layoutPretty options logDoc
  let mockLog = "\nEmulator log :\n--------------\n" <> logString
  putStrLn mockLog
