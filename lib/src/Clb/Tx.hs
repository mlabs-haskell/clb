module Clb.Tx (
  OnChainTx (..),
  Block,
) where

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as Core (Tx)
import Cardano.Ledger.Shelley.API qualified as L (Validated, extractTx)
import Codec.Serialise (Serialise (..))
import Clb.Era (CardanoLedgerEra)

{- | A validated Tx, that made it to the chain.
Might has IsValid = False in which case collaterals will be collected.
-}
newtype OnChainTx era = OnChainTx
  {getOnChainTx :: L.Validated (Core.Tx (CardanoLedgerEra era))}

deriving newtype instance (Show (Core.Tx (CardanoLedgerEra era)))
  => Show (OnChainTx era)

instance (CBOR.ToCBOR (Core.Tx (CardanoLedgerEra era))) => Serialise (OnChainTx era) where
  encode = CBOR.toCBOR . L.extractTx . getOnChainTx -- For blockID
  decode = fail "Not allowed to use `decode` on `OnChainTx`" -- Unused

{- | A block on the blockchain. This is just a list of transactions
following on from the chain so far.
-}
type Block era = [OnChainTx era]

