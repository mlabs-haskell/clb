{-# OPTIONS_GHC -Wno-orphans #-}

module ClbSocket.Types (
  Request (..),
  CBOR (..),
  Transaction (..),
) where

import Cardano.Api.Shelley qualified as C
import ClbSocket.Serialise.Tx (CardanoTx)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

-- | Request Types
data Request
  = SendTx {submitTransaction :: CardanoTx}
  | TxOutRefAt (C.AddressInEra C.BabbageEra)
  --   | TxOutRefAtState (L.Addr L.StandardCrypto) ClbState
  --   | TxOutRefAtPaymentCred P.Credential
  --   | GetCurrentSlot
  --   | GetUtxosAtState ClbState
  --   | GetEpochInfo
  deriving (Generic, Show)

instance ToJSON Request
instance FromJSON Request

newtype CBOR = CBOR {cbor :: BS.ByteString} deriving (Generic)

instance ToJSON CBOR
instance FromJSON CBOR

newtype Transaction = Transaction {transaction :: CBOR} deriving (Generic)

instance ToJSON Transaction
instance FromJSON Transaction

-- | Encode a lazy `ByteString` as a Base16 JSON string.
instance ToJSON BS.ByteString where
  toJSON bs =
    let encoded = BS16.encode bs
     in String $ TE.decodeUtf8 encoded

-- | Decode a Base16 JSON string into a lazy `ByteString`.
instance FromJSON BS.ByteString where
  parseJSON (String t) =
    let decoded = BS16.decode (TE.encodeUtf8 t)
     in case decoded of
          Right bs -> pure bs
          Left e -> fail e
  parseJSON _ = fail "Expected a JSON string for ByteString"
