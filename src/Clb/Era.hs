{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Clb.Era (IsCardanoLedgerEra, IsCardanoLedgerEra', CardanoLedgerEra, IsMaryBasedEra (maryBasedEra)) where

import Cardano.Api.Eras
import Cardano.Api.Shelley
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Shelley.API
import Data.Default (Default)

-- | Helper class for constraining the 'CardanoLedgerEra' closed type family.
-- Ensures user chosen era (from Cardano.Api.Eras) is valid for usage with CLB.
class ( ledgerEra ~ CardanoLedgerEra era
      , ledgerEra ~ ShelleyLedgerEra era
      , L.EraCrypto ledgerEra ~ L.StandardCrypto
      , L.EraTxOut ledgerEra
      , ApplyTx ledgerEra
      , Default (L.GovState ledgerEra)
      , Show (L.GovState ledgerEra)
      -- The 'IsShelleyBasedEra' constraint may seem redundant with our implementation of 'IsMaryBasedEra', but said constraint
      -- is missing on the upstream 'IsMaryBasedEra' in newer cardano-api. So if we use the upstream class later, we should uncomment
      -- this constraint.
      -- , IsShelleyBasedEra era
      , IsMaryBasedEra era
      )
      => IsCardanoLedgerEra' era ledgerEra | era -> ledgerEra where

type IsCardanoLedgerEra era = IsCardanoLedgerEra' era (CardanoLedgerEra era)

-- The `era` param should be the era to add (Mary onwards)
-- The `ledgerEra` param should be equal to 'CardanoLedgerEra era'. Look up its
-- definition in "Cardano.Api.Eras"
instance IsCardanoLedgerEra' MaryEra    (L.MaryEra    L.StandardCrypto) where
instance IsCardanoLedgerEra' AlonzoEra  (L.AlonzoEra  L.StandardCrypto) where
instance IsCardanoLedgerEra' BabbageEra (L.BabbageEra L.StandardCrypto) where
instance IsCardanoLedgerEra' ConwayEra  (L.ConwayEra  L.StandardCrypto) where

-- NOTE: Add more eras here as hardforks happen.
