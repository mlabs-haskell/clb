module ClbSocket.Parse (parseRequest) where

import Cardano.Api (SerialiseAsCBOR (deserialiseFromCBOR), proxyToAsType)
import Cardano.Api.HasTypeProxy (Proxy (Proxy))
import Cardano.Api.Shelley qualified as C
import ClbSocket.Serialise.Tx (CardanoTx (..))
import ClbSocket.Types (CBOR (CBOR), Request (SendTx), Transaction (Transaction))
import Data.Aeson (FromJSON (..), Value (Object), decode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A general JSON-RPC request data type
data JsonRpcRequest = JsonRpcRequest
  { jsonrpc :: Text
  , method :: Text
  , params :: Parameters
  , reqId :: Maybe Text
  }
  deriving (Generic)

instance FromJSON JsonRpcRequest

data Parameters
  = ParamTx Transaction
  | NoParameters
  deriving (Generic)

instance FromJSON Parameters where
  parseJSON (Object v)
    | null v = return NoParameters
    | otherwise = ParamTx <$> parseJSON (Object v)
  parseJSON _ = return NoParameters -- Treat anything else as `NoParameters`

parseJsonRpcRequest :: BSL.ByteString -> Either String JsonRpcRequest
parseJsonRpcRequest = maybe (Left "Failed to parse JsonRpcRequest!") Right . decode

decodeJsonRpcRequest :: JsonRpcRequest -> Either String Request
decodeJsonRpcRequest (JsonRpcRequest _ "submitTransaction" (ParamTx tx) _id) = SendTx <$> decodeTransaction tx
-- Ledger State Queries
-- decodeJsonRpcRequest (JsonRpcRequest _ "queryLedgerState/utxo" NoParameters _id) = undefined
decodeJsonRpcRequest (JsonRpcRequest {}) = Left "Unsupported JSON-RPC method or invalid parameters!"

decodeTransaction :: Transaction -> Either String CardanoTx
decodeTransaction (Transaction (CBOR cbor)) = case deserialiseFromCBOR (proxyToAsType Proxy) cbor of
  Right tx -> Right $ CardanoTx tx C.ShelleyBasedEraBabbage
  Left e -> Left $ "Failed to decode Transaction : " <> show e

-- | High-level function to parse and decode a JSON-RPC request
parseRequest :: BSL.ByteString -> Either String Request
parseRequest rawRequest = parseJsonRpcRequest rawRequest >>= decodeJsonRpcRequest
