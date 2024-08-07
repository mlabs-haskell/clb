module Main where

import Cardano.Api.Value (lovelaceToValue)
import Cardano.Ledger.Coin (Coin (Coin))
import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Clb (
  ClbState (mockInfo),
  checkErrors,
  defaultBabbage,
  dumpUtxoState,
  initClb,
  ppLog,
  runClb,
 )

main :: IO ()
main = do
  putStrLn "Welcome to the smoke-test CLB harness!"
  let _dummyTotalNotUsedNow = lovelaceToValue $ Coin 1_000_000_000_000
  let perWallet = lovelaceToValue $ Coin 1_000_000_000
  let (_mbErrors, clb) =
        runClb (dumpUtxoState >> checkErrors) $
          initClb defaultBabbage _dummyTotalNotUsedNow perWallet
  let logDoc = ppLog $ mockInfo clb
  let options = defaultLayoutOptions {layoutPageWidth = AvailablePerLine 150 1.0}
  let logString = renderString $ layoutPretty options logDoc
  let mockLog = "\nEmulator log :\n--------------\n" <> logString
  putStrLn mockLog
