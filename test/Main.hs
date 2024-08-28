module Main where

import qualified ClbSocketTest.Parse as Parse

main :: IO ()
main = do
  putStrLn "This is a test suit!"
  Parse.runTest
