{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Clb.ClbLedgerState where

import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Transition (tcInitialStakingL)
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Clb.Era (CardanoLedgerEra, IsCardanoLedgerEra)
import Clb.Params (PParams, TransitionConfig)
import Clb.Tx (CardanoTx (..))
import Control.Lens (makeLenses, over, (^.))
import Data.Default (Default, def)
import Data.ListMap qualified as ListMap
import Data.Map.Strict qualified as Map

{- | mempool is just a list of transactions, ordered by the time at which they
arrived (the head is the latest)
-}
type TxPool era = [CardanoTx era]

getEmulatorEraTx :: CardanoTx era -> C.Tx era
getEmulatorEraTx (CardanoTx tx _) = tx

pattern CardanoEmulatorEraTx :: (C.IsShelleyBasedEra era) => C.Tx era -> CardanoTx era
pattern CardanoEmulatorEraTx tx <- (getEmulatorEraTx -> tx)
  where
    CardanoEmulatorEraTx tx = CardanoTx tx C.shelleyBasedEra

{-# COMPLETE CardanoEmulatorEraTx #-}

{- | State of the ledger - might be used to store ledger state of the whole
blockchain or to maintain a so-called "cached state" that reflect the state
of things for the mempool.
-}
data EmulatedLedgerState era = EmulatedLedgerState
  { _ledgerEnv :: !(L.MempoolEnv (CardanoLedgerEra era))
  , _ledgerState :: !(L.MempoolState (CardanoLedgerEra era))
  }

deriving instance (IsCardanoLedgerEra era) => Show (EmulatedLedgerState era)

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
getSlot (EmulatedLedgerState L.LedgerEnv {ledgerSlotNo = SlotNo s} _) = fromIntegral s

-- | Set the utxo
setUtxo ::
  ( L.EraTxOut (CardanoLedgerEra era)
  , Default (L.GovState (CardanoLedgerEra era))
  ) =>
  L.PParams (CardanoLedgerEra era) ->
  L.UTxO (CardanoLedgerEra era) ->
  EmulatedLedgerState era ->
  EmulatedLedgerState era
setUtxo params utxo els@EmulatedLedgerState {_ledgerState} = els {_ledgerState = newPoolState}
  where
    newPoolState = _ledgerState {L.lsUTxOState = L.smartUTxOState params utxo (L.Coin 0) (L.Coin 0) def (L.Coin 0)}

-- | Initial ledger state for a distribution
initialState ::
  forall era.
  ( L.EraTxOut (CardanoLedgerEra era)
  , L.EraTransition (CardanoLedgerEra era)
  , Default (L.GovState (CardanoLedgerEra era))
  ) =>
  PParams era ->
  TransitionConfig era ->
  EmulatedLedgerState era
initialState params tc =
  let L.ShelleyGenesisStaking {sgsPools} = tc ^. tcInitialStakingL
      -- state = trace (show sgsPools) $ EmulatedLedgerState
      state =
        EmulatedLedgerState
          { _ledgerEnv =
              L.LedgerEnv
                { L.ledgerSlotNo = 0
                , L.ledgerIx = minBound
                , L.ledgerPp = params
                , L.ledgerAccount = L.AccountState (L.Coin 0) (L.Coin 0)
                }
          , _ledgerState =
              L.LedgerState
                { lsUTxOState = L.smartUTxOState params mempty (L.Coin 0) (L.Coin 0) def (L.Coin 0)
                , lsCertState = def {L.certPState = pState'}
                }
                -- , _currentBlock = []
          }
      pState' :: L.PState (CardanoLedgerEra era) =
        L.PState
          (ListMap.toMap sgsPools)
          Map.empty
          Map.empty
          Map.empty
   in state
