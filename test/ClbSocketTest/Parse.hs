{-# LANGUAGE ImportQualifiedPost #-}

module ClbSocketTest.Parse (runTest) where

import Cardano.Api.Ledger (StrictMaybe (SNothing))
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Core (ValidityInterval (ValidityInterval))
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.Tx qualified as CL
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody)
import Cardano.Ledger.Shelley.Core (Withdrawals (Withdrawals))
import ClbSocket (getEmulatorBabbageEraTx, parseRequest, submitTransaction)
import Data.ByteString.Lazy qualified as BSL

runTest :: IO ()
runTest = do
  putStrLn "Testing ClbSocket.Parse .."
  test_parseRequest

test_parseRequest :: IO ()
test_parseRequest = do
  let filePath = "./test/SampleRequests/submitTxRequest.json"
  rawRequest <- BSL.readFile filePath

  case parseRequest rawRequest of
    Right req -> print req >> print (getEmulatorBabbageEraTx (submitTransaction req) == babbageEraTx)
    Left e -> error $ show e

-- Helper Functions
babbageEraTx :: C.Tx C.BabbageEra
babbageEraTx = C.Tx txBody []

txBody :: C.TxBody C.BabbageEra
txBody = C.ShelleyTxBody C.ShelleyBasedEraBabbage txBody' [] C.TxBodyNoScriptData Nothing C.TxScriptValidityNone
  where
    txBody' :: BabbageTxBody Babbage
    txBody' = CL.BabbageTxBody mempty mempty mempty mempty SNothing mempty mempty (Withdrawals mempty) mempty (ValidityInterval SNothing SNothing) SNothing mempty mempty SNothing SNothing SNothing

-- -- Helper Functions

-- -- This function ensures the directory exists before writing the JSON file
-- writeJsonToFile :: FilePath -> BSL.ByteString -> IO ()
-- writeJsonToFile filePath = (createDirectoryIfMissing True (takeDirectory filePath) >>) . BSL.writeFile filePath
