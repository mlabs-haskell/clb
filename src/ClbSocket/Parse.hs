{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module ClbSocket.Parse (
  Request (..),

  -- * Data Parsing Functions
  parseData,
  parseBabbageEraTx,
) where

import qualified Cardano.Api.Shelley as C
import ClbSocket.Serialise.Tx (CardanoTx (..), getEmulatorBabbageEraTx)
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics (Generic)

-- | Request Types
data Request
  = -- \| Actions that can be performed
    SendTx CardanoTx
  | -- \| Queries that can be made
    TxOutRefAt
  | TxOutRefAtState
  | TxOutRefAtPaymentCred
  | GetCurrentSlot
  | GetUtxosAtState
  | GetEpochInfo
  deriving (Generic)

instance ToJSON Request
instance FromJSON Request

-- | Parse a Request
parseData :: BSL.ByteString -> Maybe Request
parseData = decode

-- | Parse a Babbage Era Transaction
parseBabbageEraTx :: BSL.ByteString -> Maybe (C.Tx C.BabbageEra)
parseBabbageEraTx str = getEmulatorBabbageEraTx <$> decode str
