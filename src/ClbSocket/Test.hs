{-# LANGUAGE ImportQualifiedPost #-}

module ClbSocket.Test (runTest) where

import Cardano.Api.Ledger (StrictMaybe (SNothing))
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Core (ValidityInterval (ValidityInterval))
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.Tx qualified as CL
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody)
import Cardano.Ledger.Shelley.Core (Withdrawals (Withdrawals))
import ClbSocket.Parse (parseBabbageEraTx)
import ClbSocket.Serialise (serializeBabbageEraTx)
import Data.ByteString.Lazy qualified as BSL
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

runTest :: IO ()
runTest = do
  putStrLn "Testing ClbSocket .."
  testTx

testTx :: IO ()
testTx = do
  let filePath = "./test/Assets/babbageEraTx.json"
  putStrLn "-----------------------------------------------------"
  putStrLn "Babbage Era Tx:"
  print babbageEraTx

  putStrLn "Serialized Babbage Era Tx:"
  let serializedTx = serializeBabbageEraTx babbageEraTx
  writeJsonToFile filePath serializedTx
  print serializedTx

  putStrLn "Parsed Babbage Era Tx:"
  serializedTx' <- BSL.readFile filePath
  let parsedTx = case parseBabbageEraTx serializedTx' of
        Just tx -> tx
        Nothing -> error "Can't Parse Tx!"
  print parsedTx

  putStr "Tx == parse (serialize Tx) : "
  print $ babbageEraTx == parsedTx

babbageEraTx :: C.Tx C.BabbageEra
babbageEraTx = C.Tx txBody []

txBody :: C.TxBody C.BabbageEra
txBody = C.ShelleyTxBody C.ShelleyBasedEraBabbage txBody' [] C.TxBodyNoScriptData Nothing C.TxScriptValidityNone
  where
    txBody' :: BabbageTxBody Babbage
    txBody' = CL.BabbageTxBody mempty mempty mempty mempty SNothing mempty mempty (Withdrawals mempty) mempty (ValidityInterval SNothing SNothing) SNothing mempty mempty SNothing SNothing SNothing

-- Helper Functions

-- This function ensures the directory exists before writing the JSON file
writeJsonToFile :: FilePath -> BSL.ByteString -> IO ()
writeJsonToFile filePath = (createDirectoryIfMissing True (takeDirectory filePath) >>) . BSL.writeFile filePath
