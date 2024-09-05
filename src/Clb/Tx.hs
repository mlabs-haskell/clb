module Clb.Tx (
  OnChainTx (..),
) where

import Cardano.Ledger.Core qualified as Core (Tx)
import Cardano.Ledger.Shelley.API qualified as L (Validated)
import Clb.Era (CardanoLedgerEra)

{- | A validated Tx, that made it to the chain.
Might has IsValid = False in which case collaterals will be collected.
-}
newtype OnChainTx era = OnChainTx
  {getOnChainTx :: L.Validated (Core.Tx (CardanoLedgerEra era))}

deriving newtype instance (Show (Core.Tx (CardanoLedgerEra era))) => Show (OnChainTx era)
