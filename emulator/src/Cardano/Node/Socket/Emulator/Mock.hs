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
  to be validated and added to the chain.
-}
slotCoordinator ::
  SlotConfig ->
  Server.ServerHandler ->
  IO a
slotCoordinator sc serverHandler = do
  forever $ do
    slot <- currentSlot sc
    print slot
    void $ Server.modifySlot (const slot) serverHandler
    now <- Time.getPOSIXTime
    let delay = slotToBeginPOSIXTime sc (slot + 1) - nominalDiffTimeToPOSIXTime now
    liftIO $
      threadDelay $
        fromIntegral $
          toMicroseconds (fromIntegral delay :: Millisecond)
    void $ Server.processBlock serverHandler
