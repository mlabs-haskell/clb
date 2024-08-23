{-# LANGUAGE ImportQualifiedPost #-}

module ClbSocket.Serialise where

import Cardano.Api.Shelley qualified as C
import ClbSocket.Serialise.Tx (CardanoTx (CardanoTx))
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BSL
import PlutusPrelude (Generic)

-- | Data Serialization Functions
newtype Response a = Response a deriving (Generic)

instance (ToJSON a) => ToJSON (Response a)

serializeData :: (ToJSON a) => Response a -> BSL.ByteString
serializeData = encode

serializeBabbageEraTx :: C.Tx C.BabbageEra -> BSL.ByteString
serializeBabbageEraTx tx = encode $ CardanoTx tx C.ShelleyBasedEraBabbage
