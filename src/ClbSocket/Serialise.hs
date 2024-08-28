module ClbSocket.Serialise (
  serializeData,
  serializeBabbageEraTx,
  Response (..),
) where

import Cardano.Api (serialiseToCBOR)
import Cardano.Api.Shelley qualified as C
import ClbSocket.Types (CBOR (CBOR), Transaction (Transaction))
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BSL
import PlutusPrelude (Generic)

-- | Data Serialization Functions
data Response = Response deriving (Generic)

instance ToJSON Response

serializeData :: Response -> BSL.ByteString
serializeData = encode

serializeBabbageEraTx :: C.Tx C.BabbageEra -> BSL.ByteString
-- serializeBabbageEraTx tx = encode $ CardanoTx tx C.ShelleyBasedEraBabbage
serializeBabbageEraTx = encode . Transaction . CBOR . serialiseToCBOR @(C.Tx C.BabbageEra)
