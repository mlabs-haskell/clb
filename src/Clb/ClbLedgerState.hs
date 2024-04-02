{-# LANGUAGE TemplateHaskell #-}

module Clb.ClbLedgerState where

import Cardano.Ledger.Api qualified as C
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Ledger.Slot (SlotNo)
import Clb.Era (EmulatorEra)
import Clb.Params (PParams)
import Clb.Tx (OnChainTx)
import Control.Lens (makeLenses, over)
import Data.Default (def)

{- | Emulator block (currently we are just keeping one jumbo block
that holds all transactions but this might be changed in the future)
is just a list of validated transactions.
TODO: shall we use 'OnChainTx' here?
-}
type EmulatorBlock = [OnChainTx]

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

-- | Set the utxo
setUtxo :: C.PParams EmulatorEra -> L.UTxO EmulatorEra -> EmulatedLedgerState -> EmulatedLedgerState
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
