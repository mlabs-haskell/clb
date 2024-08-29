module ClbSocket.Serialise (
  serializeResponse,
  Response (..),
) where

import Cardano.Binary (serialize)
import Cardano.Ledger.Shelley.API.Mempool (extractTx)
import Clb (ValidationResult (Fail, Success))
import Clb.Tx (OnChainTx (OnChainTx))
import Data.ByteString.Lazy qualified as BSL
import PlutusPrelude (Generic)

-- | Data Serialization Functions
newtype Response = SendTxResponse ValidationResult deriving (Generic)

serializeResponse :: Response -> BSL.ByteString
serializeResponse (SendTxResponse (Fail _ validationError)) = serialize validationError
serializeResponse (SendTxResponse (Success _ (OnChainTx validatedTx))) = serialize $ extractTx validatedTx
