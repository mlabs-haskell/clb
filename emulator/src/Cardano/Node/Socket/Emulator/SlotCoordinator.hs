{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Socket.Emulator.SlotCoordinator where

import Cardano.BM.Data.Trace (Trace)
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Clb.TimeSlot (SlotConfig, currentSlot, nominalDiffTimeToPOSIXTime, slotToBeginPOSIXTime)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Freer.Extras (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Units (Millisecond, toMicroseconds)
import Plutus.Monitoring.Util (runLogEffects)

{- | Calls 'addBlock' at the start of every slot, causing pending transactions
  to be validated and added to the chain. If the mempool is empty, no blocks are
  produced.
-}
slotCoordinator ::
  Trace IO String ->
  SlotConfig ->
  Server.ServerHandler ->
  IO a
slotCoordinator trace sc serverHandler = do
  runLogEffects trace $ do
    forever $ do
      -- TODO: for some reason, the systemStart in Shelley genesis tends to be
      -- slightly later (somewhat ~4s) then now (getPOSIXTime). This leads to
      -- negative slot numbers which are misleading (unfortunately types are not
      -- strcit enough to prevent it).
      -- I guess it happens in cardano-testnet, but for now I decided to use a
      -- very makeshift solution here and just wait till we get a positive number.
      slot <- liftIO $ currentSlot sc
      if slot >= 0
        then do
          void $ Server.tryProduceBlock serverHandler
          void $ Server.modifySlot (const slot) serverHandler
          logInfo $ "Chain extended, new tip, slot is: " <> show slot
        else logInfo "Waiting for systemStart to happen (see comments in SlotCoordinator.hs)"
      now <- liftIO Time.getPOSIXTime
      let delay = slotToBeginPOSIXTime sc (slot + 1) - nominalDiffTimeToPOSIXTime now
      liftIO $
        threadDelay $
          fromIntegral $
            toMicroseconds (fromIntegral delay :: Millisecond)
