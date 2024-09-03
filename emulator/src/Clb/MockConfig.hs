-- | Config for emulator (from PSM)
module Clb.MockConfig (
  -- Stat (..),
  MockConfig (..),
  CheckLimits (..),
  defaultSlotConfig,
  defaultMockConfig,
  defaultBabbage,
  defaultBabbageParams,
  skipLimits,
  warnLimits,
  forceLimits,
  keptBlocks,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as L
import Cardano.Ledger.Shelley.Transition qualified as L
import Clb.Params (
  PParams,
  TransitionConfig,
  defaultBabbageParams,
 )
import Clb.TimeSlot

{- | Config for the blockchain.
TODO: rename to ClbConfig
-}
data MockConfig = MockConfig
  { mockConfigCheckLimits :: !CheckLimits
  -- ^ limits check mode
  , mockConfigProtocol :: !PParams
  -- ^ Protocol parameters
  , mockConfigNetworkId :: !C.NetworkId
  -- ^ Network id (mainnet / testnet)
  , mockConfigSlotConfig :: !SlotConfig
  -- ^ Slot config
  , mockConfigConfig :: !TransitionConfig
  -- ^ Transition configuration : needed to be able to start in a current era
  }
  deriving stock (Show)

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
 FIXME: remove rest of `Babbage` naming distinction (not applicable anymore)
-}
defaultBabbage :: MockConfig
defaultBabbage = defaultMockConfig defaultBabbageParams

defaultTransitionConfig :: TransitionConfig
defaultTransitionConfig = L.mkShelleyTransitionConfig L.shelleyGenesisDefaults

-- | Default blockchain config.
defaultMockConfig :: PParams -> MockConfig
defaultMockConfig params =
  MockConfig
    { mockConfigCheckLimits = ErrorLimits
    , mockConfigProtocol = params
    , mockConfigNetworkId = C.Testnet $ C.NetworkMagic 42
    , mockConfigSlotConfig = defaultSlotConfig
    , mockConfigConfig = defaultTransitionConfig
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

-- FIXME: We need TransitionConfig to implement this
keptBlocks :: MockConfig -> Integer
keptBlocks = const 42

-- keptBlocks MockConfig{mockConfigProtocol} =
--   fromIntegral $ sgSecurityParam (_ ^. tcShelleyGenesisL)
