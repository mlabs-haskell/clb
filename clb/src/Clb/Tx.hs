module Clb.Tx (
  CardanoTx (..),
  OnChainTx (..),
  Block,
  Blockchain,
) where

import Cardano.Api.Shelley qualified as C

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as Core (Tx)
import Cardano.Ledger.Shelley.API qualified as L (Validated, extractTx)
import Clb.Era (CardanoLedgerEra)
import Codec.Serialise (Serialise (..))

-- | Cardano tx from any Shelley-based era.
data CardanoTx era where
  CardanoTx :: C.Tx era -> C.ShelleyBasedEra era -> CardanoTx era

-- TODO: do we need it? Should it print a tx hash?
instance Show (CardanoTx era) where
  show = const "CardanoTx"

{- | A validated Tx, that made it to the chain.
Might has IsValid = False in which case the collateral will be collected.
-}
newtype OnChainTx era = OnChainTx
  {getOnChainTx :: L.Validated (Core.Tx (CardanoLedgerEra era))}

deriving newtype instance
  (Show (Core.Tx (CardanoLedgerEra era))) =>
  Show (OnChainTx era)

{- | We need this instance to be able to generate block ids, this is why
we only neew 'encode' part.
-}
instance (CBOR.ToCBOR (Core.Tx (CardanoLedgerEra era))) => Serialise (OnChainTx era) where
  encode = CBOR.toCBOR . L.extractTx . getOnChainTx
  decode = fail "Not allowed to use `decode` on `OnChainTx`"

{- | A block on the blockchain. This is just a list of transactions
following on from the chain so far.
-}
type Block era = [OnChainTx era]

-- | A blockchain, which is just a list of blocks, starting with the newest.
type Blockchain era = [Block era]
