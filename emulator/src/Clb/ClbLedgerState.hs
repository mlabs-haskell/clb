{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Clb.ClbLedgerState where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Clb.Era (EmulatorEra)
import Clb.Params (PParams)
import Clb.Tx (OnChainTx)
import Control.Lens (makeLenses, over)
import Data.Default (def)

{- | Emulator block (currently we are just keeping one jumbo block
that holds all transactions but this might be changed in the future)
is just a list of validated transactions.
-}
type EmulatorBlock = [OnChainTx]

-- | mempool
type TxPool = [CardanoTx]

-- | Cardano tx from any era.
data CardanoTx where
  CardanoTx :: C.Tx era -> C.ShelleyBasedEra era -> CardanoTx

instance Show CardanoTx where
  show = const "CardanoTx"

-- getEmulatorEraTx :: CardanoTx -> C.Tx C.ConwayEra
getEmulatorEraTx :: CardanoTx -> C.Tx C.BabbageEra
-- getEmulatorEraTx (CardanoTx tx C.ShelleyBasedEraConway) = tx
getEmulatorEraTx (CardanoTx tx C.ShelleyBasedEraBabbage) = tx
getEmulatorEraTx _ = error "getEmulatorEraTx: Expected a Conway tx"

-- pattern CardanoEmulatorEraTx :: C.Tx C.ConwayEra -> CardanoTx
pattern CardanoEmulatorEraTx :: C.Tx C.BabbageEra -> CardanoTx
pattern CardanoEmulatorEraTx tx <- (getEmulatorEraTx -> tx)
  where
    CardanoEmulatorEraTx tx = CardanoTx tx C.shelleyBasedEra

{-# COMPLETE CardanoEmulatorEraTx #-}

-- | State of the ledger with configuration, mempool, and the blockchain.
data EmulatedLedgerState = EmulatedLedgerState
  { _ledgerEnv :: !(L.MempoolEnv EmulatorEra)
  , _memPoolState :: !(L.MempoolState EmulatorEra)
  , _currentBlock :: !EmulatorBlock
  }
  deriving (Show)

makeLenses ''EmulatedLedgerState

-- | Increase the slot number by one
nextSlot :: EmulatedLedgerState -> EmulatedLedgerState
nextSlot = over ledgerEnv f
  where
    f l@L.LedgerEnv {ledgerSlotNo = oldSlot} = l {L.ledgerSlotNo = succ oldSlot}

-- | Set the slot number
setSlot :: SlotNo -> EmulatedLedgerState -> EmulatedLedgerState
setSlot sl = over ledgerEnv (\l -> l {L.ledgerSlotNo = sl})

-- | Set the slot number
updateSlot :: (SlotNo -> SlotNo) -> EmulatedLedgerState -> EmulatedLedgerState
updateSlot f = over ledgerEnv (\l -> l {L.ledgerSlotNo = f (L.ledgerSlotNo l)})

-- | Get the slot number
getSlot :: (Num a) => EmulatedLedgerState -> a
getSlot (EmulatedLedgerState L.LedgerEnv {ledgerSlotNo = SlotNo s} _ _) = fromIntegral s

-- | Set the utxo
setUtxo :: L.PParams EmulatorEra -> L.UTxO EmulatorEra -> EmulatedLedgerState -> EmulatedLedgerState
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
initialState :: PParams -> EmulatedLedgerState
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
