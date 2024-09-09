{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Clb.ClbLedgerState where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Clb.Era (CardanoLedgerEra, IsCardanoLedgerEra)
import Clb.Params (PParams)
import Clb.Tx (OnChainTx)
import Control.Lens (makeLenses, over)
import Data.Default (Default, def)

{- | Emulator block (currently we are just keeping one jumbo block
that holds all transactions but this might be changed in the future)
is just a list of validated transactions.
-}
type EmulatorBlock era = [OnChainTx era]

-- | mempool
type TxPool era = [CardanoTx era]

-- | Cardano tx from any era.
data CardanoTx era where
  CardanoTx :: C.Tx era -> C.ShelleyBasedEra era -> CardanoTx era

instance Show (CardanoTx era) where
  show = const "CardanoTx"

getEmulatorEraTx :: CardanoTx era -> C.Tx era
getEmulatorEraTx (CardanoTx tx _) = tx

pattern CardanoEmulatorEraTx :: (C.IsShelleyBasedEra era) => C.Tx era -> CardanoTx era
pattern CardanoEmulatorEraTx tx <- (getEmulatorEraTx -> tx)
  where
    CardanoEmulatorEraTx tx = CardanoTx tx C.shelleyBasedEra

{-# COMPLETE CardanoEmulatorEraTx #-}

-- | State of the ledger with configuration, mempool, and the blockchain.
data EmulatedLedgerState era = EmulatedLedgerState
  { _ledgerEnv :: !(L.MempoolEnv (CardanoLedgerEra era))
  , _memPoolState :: !(L.MempoolState (CardanoLedgerEra era))
  , _currentBlock :: !(EmulatorBlock era)
  }

deriving instance (IsCardanoLedgerEra era, Show (C.Tx (CardanoLedgerEra era))) => Show (EmulatedLedgerState era)

makeLenses ''EmulatedLedgerState

-- | Increase the slot number by one
nextSlot :: EmulatedLedgerState era -> EmulatedLedgerState era
nextSlot = over ledgerEnv f
  where
    f l@L.LedgerEnv {ledgerSlotNo = oldSlot} = l {L.ledgerSlotNo = succ oldSlot}

-- | Set the slot number
setSlot :: SlotNo -> EmulatedLedgerState era -> EmulatedLedgerState era
setSlot sl = over ledgerEnv (\l -> l {L.ledgerSlotNo = sl})

-- | Update the slot number
updateSlot :: (SlotNo -> SlotNo) -> EmulatedLedgerState era -> EmulatedLedgerState era
updateSlot f = over ledgerEnv (\l -> l {L.ledgerSlotNo = f (L.ledgerSlotNo l)})

-- | Get the slot number
getSlot :: (Num a) => EmulatedLedgerState era -> a
getSlot (EmulatedLedgerState L.LedgerEnv {ledgerSlotNo = SlotNo s} _ _) = fromIntegral s

-- | Set the utxo
setUtxo ::
  ( L.EraTxOut (CardanoLedgerEra era)
  , Default (L.GovState (CardanoLedgerEra era))
  ) =>
  L.PParams (CardanoLedgerEra era) ->
  L.UTxO (CardanoLedgerEra era) ->
  EmulatedLedgerState era ->
  EmulatedLedgerState era
setUtxo params utxo els@EmulatedLedgerState {_memPoolState} = els {_memPoolState = newPoolState}
  where
    newPoolState = _memPoolState {L.lsUTxOState = L.smartUTxOState params utxo (L.Coin 0) (L.Coin 0) def (L.Coin 0)}

-- {- | Make a block with all transactions that have been validated in the
-- current block, add the block to the blockchain, and empty the current block.
-- -}
-- makeBlock :: EmulatedLedgerState -> EmulatedLedgerState
-- makeBlock state =
--   state
--     & currentBlock .~ []
--     & over previousBlocks ((reverse $ state ^. currentBlock) :)

-- | Initial ledger state for a distribution
initialState :: (L.EraTxOut (CardanoLedgerEra era), Default (L.GovState (CardanoLedgerEra era))) => PParams era -> EmulatedLedgerState era
initialState params =
  EmulatedLedgerState
    { _ledgerEnv =
        L.LedgerEnv
          { L.ledgerSlotNo = 0
          , L.ledgerIx = minBound
          , L.ledgerPp = params
          , L.ledgerAccount = L.AccountState (L.Coin 0) (L.Coin 0)
          }
    , _memPoolState =
        L.LedgerState
          { lsUTxOState = L.smartUTxOState params mempty (L.Coin 0) (L.Coin 0) def (L.Coin 0)
          , lsCertState = def
          }
    , _currentBlock = []
    }
