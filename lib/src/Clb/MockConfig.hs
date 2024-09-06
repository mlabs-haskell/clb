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
  keptBlocks,
  paramsFromConfig,
  defaultTransitionConfig,
  defaultConwayTransitionConfig,
) where

import Cardano.Api qualified as C
import Cardano.Api.NetworkId qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Genesis qualified as Alonzo
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.Transition qualified as T
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.Transition qualified as T
import Cardano.Ledger.Conway.Transition qualified as T
import Cardano.Ledger.Mary.Transition qualified as T
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.Transition qualified as T
import Clb.Era (CardanoLedgerEra)
import Clb.Params (
  PParams,
  TransitionConfig,
  defaultAlonzoParams',
  defaultBabbageParams,
  defaultConwayParams,
 )
import Clb.TimeSlot (SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime), nominalDiffTimeToPOSIXTime, utcTimeToPOSIXTime)
import Control.Lens.Getter ((^.))
import PlutusLedgerApi.V3 (POSIXTime (getPOSIXTime))

{- | Config for the blockchain.
TODO: rename to ClbConfig
-}
data MockConfig era = MockConfig
  { mockConfigCheckLimits :: !CheckLimits
  -- ^ limits check mode
  , mockConfigProtocol :: !(PParams era)
  -- ^ Protocol parameters
  , mockConfigNetworkId :: !C.NetworkId
  -- ^ Network id (mainnet / testnet)
  , mockConfigSlotConfig :: !SlotConfig
  -- ^ Slot config
  , mockConfigConfig :: !(TransitionConfig era)
  -- ^ Transition configuration : needed to be able to start in a current era
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
 FIXME: remove rest of `Babbage` naming distinction (not applicable anymore)
-}
defaultBabbage :: MockConfig C.BabbageEra
defaultBabbage = defaultMockConfig defaultBabbageParams defaultTransitionConfig

-- | Default Conwayconfig.
defaultConway :: MockConfig C.ConwayEra
defaultConway = defaultMockConfig defaultConwayParams defaultConwayTransitionConfig

defaultTransitionConfig :: TransitionConfig C.BabbageEra
defaultTransitionConfig =
  T.BabbageTransitionConfig $
    T.AlonzoTransitionConfig (Alonzo.AlonzoGenesisWrapper udefaultAlonzoParams') $
      T.MaryTransitionConfig $
        T.AllegraTransitionConfig $
          T.mkShelleyTransitionConfig C.shelleyGenesisDefaults
  where
    udefaultAlonzoParams' =
      L.UpgradeAlonzoPParams
        { uappCoinsPerUTxOWord = defaultAlonzoParams' ^. Alonzo.ppCoinsPerUTxOWordL
        , uappCostModels = defaultAlonzoParams' ^. Alonzo.ppCostModelsL
        , uappPrices = defaultAlonzoParams' ^. Alonzo.ppPricesL
        , uappMaxTxExUnits = defaultAlonzoParams' ^. Alonzo.ppMaxTxExUnitsL
        , uappMaxBlockExUnits = defaultAlonzoParams' ^. Alonzo.ppMaxBlockExUnitsL
        , uappMaxValSize = defaultAlonzoParams' ^. Alonzo.ppMaxValSizeL
        , uappCollateralPercentage = defaultAlonzoParams' ^. Alonzo.ppCollateralPercentageL
        , uappMaxCollateralInputs = defaultAlonzoParams' ^. Alonzo.ppMaxCollateralInputsL
        }

-- FIXME: base on Babbage
defaultConwayTransitionConfig :: TransitionConfig C.ConwayEra
defaultConwayTransitionConfig =
  -- FIXME: undefined
  T.ConwayTransitionConfig undefined $
    T.BabbageTransitionConfig $
      T.AlonzoTransitionConfig (Alonzo.AlonzoGenesisWrapper udefaultAlonzoParams') $
        T.MaryTransitionConfig $
          T.AllegraTransitionConfig $
            T.mkShelleyTransitionConfig C.shelleyGenesisDefaults
  where
    udefaultAlonzoParams' =
      L.UpgradeAlonzoPParams
        { uappCoinsPerUTxOWord = defaultAlonzoParams' ^. Alonzo.ppCoinsPerUTxOWordL
        , uappCostModels = defaultAlonzoParams' ^. Alonzo.ppCostModelsL
        , uappPrices = defaultAlonzoParams' ^. Alonzo.ppPricesL
        , uappMaxTxExUnits = defaultAlonzoParams' ^. Alonzo.ppMaxTxExUnitsL
        , uappMaxBlockExUnits = defaultAlonzoParams' ^. Alonzo.ppMaxBlockExUnitsL
        , uappMaxValSize = defaultAlonzoParams' ^. Alonzo.ppMaxValSizeL
        , uappCollateralPercentage = defaultAlonzoParams' ^. Alonzo.ppCollateralPercentageL
        , uappMaxCollateralInputs = defaultAlonzoParams' ^. Alonzo.ppMaxCollateralInputsL
        }

-- | Default blockchain config.
defaultMockConfig :: PParams era -> TransitionConfig era -> MockConfig era
defaultMockConfig params config =
  MockConfig
    { mockConfigCheckLimits = ErrorLimits
    , mockConfigProtocol = params
    , mockConfigNetworkId = C.Testnet $ C.NetworkMagic 42
    , mockConfigSlotConfig = defaultSlotConfig
    , mockConfigConfig = config
    }

paramsFromConfig ::
  (T.EraTransition (CardanoLedgerEra era)) =>
  TransitionConfig era ->
  MockConfig era
paramsFromConfig tc =
  MockConfig
    { mockConfigSlotConfig =
        SlotConfig
          { scSlotZeroTime = utcTimeToPOSIXTime $ L.sgSystemStart sg
          , scSlotLength =
              getPOSIXTime $ nominalDiffTimeToPOSIXTime $ L.fromNominalDiffTimeMicro $ L.sgSlotLength sg
          }
    , mockConfigProtocol = tc ^. T.tcInitialPParamsG
    , mockConfigNetworkId = C.fromShelleyNetwork (L.sgNetworkId sg) (C.NetworkMagic $ L.sgNetworkMagic sg)
    , mockConfigCheckLimits = ErrorLimits -- FIXME: soft-code it
    , mockConfigConfig = tc
    }
  where
    sg = tc ^. T.tcShelleyGenesisL

-- | Do not check for limits
skipLimits :: MockConfig era -> MockConfig era
skipLimits cfg = cfg {mockConfigCheckLimits = IgnoreLimits}

-- | Warn on limits
warnLimits :: MockConfig era -> MockConfig era
warnLimits cfg = cfg {mockConfigCheckLimits = WarnLimits}

-- | Error on limits
forceLimits :: MockConfig era -> MockConfig era
forceLimits cfg = cfg {mockConfigCheckLimits = ErrorLimits}

keptBlocks :: (T.EraTransition (CardanoLedgerEra era)) => MockConfig era -> Integer
keptBlocks MockConfig {mockConfigConfig} =
  fromIntegral $ L.sgSecurityParam (mockConfigConfig ^. T.tcShelleyGenesisL)
