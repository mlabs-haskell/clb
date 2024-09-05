module Clb.TimeSlot (
  SlotConfig (..),
  slotConfigToEpochInfo,
  utcTimeToPOSIXTime,
  posixTimeToUTCTime,
  nominalDiffTimeToPOSIXTime,
  posixTimeToNominalDiffTime,
) where

import PlutusTx.Prelude hiding (Eq, (<$>))
import Prelude (Eq, Show)

import Data.Text (Text)

import Data.Time.Clock qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Time (POSIXTime (POSIXTime, getPOSIXTime))
import Prettyprinter (Pretty (pretty), (<+>))
import Prelude qualified as Haskell

import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Time (slotLengthFromMillisec)

import Clb.Params (emulatorEpochSize)

{- | Datatype to configure the length (ms) of one slot and the beginning of the
 first slot.

 `cardano-slotting` `EpochInfo` is not serializable.
 See: https://github.com/IntersectMBO/cardano-base/issues/469
-}
data SlotConfig = SlotConfig
  { scSlotLength :: Integer
  -- ^ Length (number of milliseconds) of one slot
  , scSlotZeroTime :: POSIXTime
  -- ^ Beginning of slot 0 (in milliseconds)
  }
  deriving stock (Eq, Show, Generic)

instance Pretty SlotConfig where
  pretty SlotConfig {scSlotLength, scSlotZeroTime} =
    "Slot 0 starts at"
      <+> pretty scSlotZeroTime
      <+> "and one slot has length of"
      <+> pretty scSlotLength
      <+> "ms"

slotConfigToEpochInfo :: SlotConfig -> EpochInfo (Either Text)
slotConfigToEpochInfo sc = do
  fixedEpochInfo emulatorEpochSize (slotLengthFromMillisec $ scSlotLength sc)

-- Utils

utcTimeToPOSIXTime :: Time.UTCTime -> POSIXTime
utcTimeToPOSIXTime = nominalDiffTimeToPOSIXTime . Time.utcTimeToPOSIXSeconds

posixTimeToUTCTime :: POSIXTime -> Time.UTCTime
posixTimeToUTCTime = Time.posixSecondsToUTCTime . posixTimeToNominalDiffTime

nominalDiffTimeToPOSIXTime :: Time.NominalDiffTime -> POSIXTime
nominalDiffTimeToPOSIXTime =
  POSIXTime
    . Haskell.truncate
    . (Haskell.* 1000) -- Convert to ms
    . Time.nominalDiffTimeToSeconds

posixTimeToNominalDiffTime :: POSIXTime -> Time.NominalDiffTime
posixTimeToNominalDiffTime =
  Time.secondsToNominalDiffTime
    . (Haskell./ 1000)
    . Haskell.fromInteger
    . getPOSIXTime
