module Clb.TimeSlot (
  SlotConfig (..),
  slotConfigToEpochInfo,
  utcTimeToPOSIXTime,
  posixTimeToUTCTime,
  nominalDiffTimeToPOSIXTime,
  posixTimeToNominalDiffTime,
  Slot (..),
  slotToBeginPOSIXTime,
  currentSlot,
  emulatorEpochSize,
  beginningOfTime,
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

import Cardano.Ledger.Slot qualified as L
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Time (slotLengthFromMillisec)

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

newtype Slot = Slot {getSlot :: Integer}
  deriving stock (Eq, Haskell.Ord, Show, Generic)
  deriving newtype
    ( AdditiveSemigroup
    , AdditiveMonoid
    , AdditiveGroup
    )
  deriving newtype (Haskell.Num, Haskell.Enum, Haskell.Real, Haskell.Integral)

instance Pretty Slot where
  pretty (Slot i) = "Slot" <+> pretty i

{-# INLINEABLE slotToBeginPOSIXTime #-}

-- | Get the starting 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
slotToBeginPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToBeginPOSIXTime SlotConfig {scSlotLength, scSlotZeroTime} (Slot n) =
  let msAfterBegin = n * scSlotLength
   in POSIXTime $ getPOSIXTime scSlotZeroTime + msAfterBegin

{- | Get the current slot number. If scSlotZeroTime may produce negative results,
which is nonsense.
-}
currentSlot :: SlotConfig -> Haskell.IO Slot
currentSlot sc = timeToSlot Haskell.<$> Time.getPOSIXTime
  where
    timeToSlot =
      posixTimeToEnclosingSlot sc
        . nominalDiffTimeToPOSIXTime

{-# INLINEABLE posixTimeToEnclosingSlot #-}

-- | Convert a 'POSIXTime' to 'Slot' given a 'SlotConfig'.
posixTimeToEnclosingSlot :: SlotConfig -> POSIXTime -> Slot
posixTimeToEnclosingSlot SlotConfig {scSlotLength, scSlotZeroTime} (POSIXTime t) =
  let timePassed = t - getPOSIXTime scSlotZeroTime
      slotsPassed = divide timePassed scSlotLength
   in Slot slotsPassed

{-# INLINEABLE beginningOfTime #-}

{- | 'beginningOfTime' corresponds to the Shelley launch date
(2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
(number of milliseconds since 1970-01-01T00:00:00Z).
-}
beginningOfTime :: Integer
beginningOfTime = 1596059091000

-- | A sensible default 'EpochSize' value for the emulator
emulatorEpochSize :: L.EpochSize
emulatorEpochSize = L.EpochSize 432000
