-- | Config for emulator (from PSM)
module Clb.MockConfig (
  MockConfig (..),
  CheckLimits (..),
  defaultSlotConfig,
  defaultMockConfig,
  defaultAlonzo,
  defaultBabbage,
  defaultAlonzoParams,
  defaultBabbageParams,
  skipLimits,
  warnLimits,
  forceLimits,
) where

import Cardano.Api qualified as C
import Clb.Params (
  PParams,
  defaultAlonzoParams,
  defaultBabbageParams,
 )
import Clb.TimeSlot

-- | Config for the blockchain.
data MockConfig = MockConfig
  { mockConfigCheckLimits :: !CheckLimits
  -- ^ limits check mode
  , mockConfigProtocol :: !PParams
  -- ^ Protocol parameters
  , mockConfigNetworkId :: !C.NetworkId
  -- ^ Network id
  , mockConfigSlotConfig :: !SlotConfig
  -- ^ Slot config
  }

data CheckLimits
  = -- | ignore TX-limits
    IgnoreLimits
  | -- | log TX to error log if it exceeds limits but accept TX
    WarnLimits
  | -- | reject TX if it exceeds the limits
    ErrorLimits
  deriving (Show)

-- | Default slot config
defaultSlotConfig :: SlotConfig
defaultSlotConfig =
  SlotConfig
    { scSlotLength = 1000 -- each slot lasts for 1 second
    , scSlotZeroTime = 0 -- starts at unix epoch start
    }

{- | Default Alonzo era config. If we use this parameter
 then Alonzo era TXs will be used for testing
-}
defaultAlonzo :: MockConfig
defaultAlonzo = defaultMockConfig defaultAlonzoParams

{- | Default Babbage era config. If we use this parameter
 then Babbage era TXs will be used for testing
-}
defaultBabbage :: MockConfig
defaultBabbage = defaultMockConfig defaultBabbageParams

-- | Default blockchain config.
defaultMockConfig :: PParams -> MockConfig
defaultMockConfig params =
  MockConfig
    { mockConfigCheckLimits = ErrorLimits
    , mockConfigProtocol = params
    , mockConfigNetworkId = C.Testnet $ C.NetworkMagic 42
    , mockConfigSlotConfig = defaultSlotConfig
    }

-- | Do not check for limits
skipLimits :: MockConfig -> MockConfig
skipLimits cfg = cfg {mockConfigCheckLimits = IgnoreLimits}

-- | Warn on limits
warnLimits :: MockConfig -> MockConfig
warnLimits cfg = cfg {mockConfigCheckLimits = WarnLimits}

-- | Error on limits
forceLimits :: MockConfig -> MockConfig
forceLimits cfg = cfg {mockConfigCheckLimits = ErrorLimits}
