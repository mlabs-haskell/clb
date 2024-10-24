{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Socket.Emulator.Mock where

import Cardano.Node.Socket.Emulator.Server qualified as Server
import Clb.TimeSlot (SlotConfig, currentSlot, nominalDiffTimeToPOSIXTime, slotToBeginPOSIXTime)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Units (Millisecond, toMicroseconds)

{- | Calls 'addBlock' at the start of every slot, causing pending transactions
  to be validated and added to the chain. If the mempool is empty, no blocks are
  produced.
-}
slotCoordinator ::
  SlotConfig ->
  Server.ServerHandler ->
  IO a
slotCoordinator sc serverHandler = do
  forever $ do
    -- TODO: for some reason, the systemStart in Shelley genesis tends to be
    -- slightly later (somewhat ~4s) then now (getPOSIXTime). This leads to
    -- negative slot numbers which are misleading (unfortunately types are now
    -- strcit enough to prevent it).
    -- I guess it happens in cardano-testnet, but for now I decided to use a
    -- very makeshift solution here and just wait till we get a positive number.
    slot <- currentSlot sc
    if slot >= 0
      then do
        -- TODO: rename
        void $ Server.processBlock serverHandler
        void $ Server.modifySlot (const slot) serverHandler
        putStrLn $ "Chain extended, new tip, slot is: " <> show slot
      else putStrLn "Waiting for systemStart to happen (see comments in SlotCoordinator.hs)"
    now <- Time.getPOSIXTime
    let delay = slotToBeginPOSIXTime sc (slot + 1) - nominalDiffTimeToPOSIXTime now
    liftIO $
      threadDelay $
        fromIntegral $
          toMicroseconds (fromIntegral delay :: Millisecond)
