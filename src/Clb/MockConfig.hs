-- | Config for emulator (from PSM)
module Clb.MockConfig (
  -- Stat (..),
  MockConfig (..),
  CheckLimits (..),
  defaultSlotConfig,
  defaultMockConfig,
  defaultBabbage,
  defaultBabbageParams,
  defaultConway,
  defaultConwayParams,
  skipLimits,
  warnLimits,
  forceLimits,
) where

import Cardano.Api qualified as C
import Cardano.Ledger.Api.PParams qualified as L
import Clb.Era (CardanoLedgerEra)
import Clb.Params (
  PParams,
  defaultBabbageParams,
  defaultConwayParams,
 )
import Clb.TimeSlot

-- | Config for the blockchain.
data MockConfig era = MockConfig
  { mockConfigCheckLimits :: !CheckLimits
  -- ^ limits check mode
  , mockConfigProtocol :: !(PParams era)
  -- ^ Protocol parameters
  , mockConfigNetworkId :: !C.NetworkId
  -- ^ Network id (mainnet / testnet)
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

{- | Default Babbage era config. If we use this parameter
 then Babbage era TXs will be used for testing
-}
defaultBabbage :: MockConfig C.BabbageEra
defaultBabbage = defaultMockConfig defaultBabbageParams

{- | Default Babbage era config. If we use this parameter
 then Babbage era TXs will be used for testing
-}
defaultConway :: MockConfig C.ConwayEra
defaultConway = defaultMockConfig defaultConwayParams

-- | Default blockchain config.
defaultMockConfig :: L.PParams (CardanoLedgerEra era) -> MockConfig era
defaultMockConfig params =
  MockConfig
    { mockConfigCheckLimits = ErrorLimits
    , mockConfigProtocol = params
    , mockConfigNetworkId = C.Testnet $ C.NetworkMagic 42
    , mockConfigSlotConfig = defaultSlotConfig
    }

-- | Do not check for limits
skipLimits :: MockConfig era -> MockConfig era
skipLimits cfg = cfg {mockConfigCheckLimits = IgnoreLimits}

-- | Warn on limits
warnLimits :: MockConfig era -> MockConfig era
warnLimits cfg = cfg {mockConfigCheckLimits = WarnLimits}

-- | Error on limits
forceLimits :: MockConfig era -> MockConfig era
forceLimits cfg = cfg {mockConfigCheckLimits = ErrorLimits}
