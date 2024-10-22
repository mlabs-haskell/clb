{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Socket.Emulator.Server (
  ServerHandler,
  runServerNode,
  processBlock,
  modifySlot,
  addTx,
  processChainEffects,
) where

import Control.Concurrent (
  MVar,
  ThreadId,
  forkIO,
  modifyMVar_,
  newMVar,
  readMVar,
 )
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (
  STM,
  TChan,
  TQueue,
  atomically,
  cloneTChan,
  newTQueueIO,
  readTChan,
  readTQueue,
  retry,
  tryReadTChan,
  writeTChan,
  writeTQueue,
 )
import Control.Exception (throwIO)
import Control.Lens (over, (.~), (^.))
import Control.Monad (forever, void)
import Control.Monad.Freer (send)
import Control.Monad.Freer.Extras.Log (LogMsg (LMessage))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (
  MonadReader (ask),
  ReaderT (runReaderT),
  runReader,
 )
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (intersect)
import Data.Maybe (listToMaybe)
import Data.SOP.Strict (NS (S, Z))
import Data.Void (Void)

import Cardano.BM.Data.Trace (Trace)
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))

-- import Ledger (Block, CardanoTx (..), Slot (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CardanoEras, ConwayEra, StandardBabbage, StandardConway)
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto, StandardShelley)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.TypeFamilyWrappers (WrapApplyTxErr (WrapApplyTxErr))
import Ouroboros.Network.Block (Point (..), pointSlot)
import Ouroboros.Network.Block qualified as O
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient (
  NodeToClientProtocols (..),
  nodeToClientCodecCBORTerm,
  nullErrorPolicies,
  versionedNodeToClientProtocols,
 )
import Ouroboros.Network.Point qualified as OP (Block (..))
import Ouroboros.Network.Protocol.ChainSync.Server (
  ChainSyncServer (..),
  ServerStIdle (..),
  ServerStIntersect (..),
  ServerStNext (..),
 )
import Ouroboros.Network.Protocol.ChainSync.Server qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Protocol.LocalStateQuery.Server qualified as Query
import Ouroboros.Network.Protocol.LocalStateQuery.Server qualified as StateQuery
import Ouroboros.Network.Protocol.LocalTxSubmission.Server qualified as TxSubmission
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as TxSubmission
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket
import Plutus.Monitoring.Util qualified as LM

import Cardano.Api qualified as C
import Cardano.Api.InMode qualified as C

import Clb qualified as E
import Clb.ClbLedgerState as E

-- import Cardano.Node.Emulator.API qualified as E
-- import Cardano.Node.Emulator.Internal.API (EmulatorMsg, EmulatorT)
-- import Cardano.Node.Emulator.Internal.API qualified as E
-- import Cardano.Node.Emulator.Internal.Node.Chain qualified as Chain
-- import Cardano.Node.Emulator.Internal.Node.Validation qualified as Validation
import Cardano.Node.Socket.Emulator.Query (HandleQuery (handleQuery))
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  BlockId (BlockId),
  SocketEmulatorState (..),
  Tip,
  blockId,
  chainSyncCodec,
  doNothingResponderProtocol,
  emulatorState,
  getChannel,
  getTip,
  nodeToClientVersion,
  nodeToClientVersionData,
  -- runChainEffects,
  -- setTip,

  runChainEffects,
  setTip,
  socketEmulatorState,
  stateQueryCodec,
  toCardanoBlock,
  txSubmissionCodec,
 )
import Clb (Block, ClbState (ClbState), ClbT, ValidationResult (..), addTxToPool, applyTx, emulatedLedgerState, unwrapClbT, validateTx)
import Clb.TimeSlot (Slot)
import Control.Monad.State (StateT (runStateT))
import Data.Data (Proxy (..))
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

type EmulatorMsg = String
data CommandChannel = CommandChannel
  { ccCommand :: TQueue ServerCommand
  , ccResponse :: TQueue ServerResponse
  }

{- | Clone the original channel for each connected client, then use
     this wrapper to make sure that no data is consumed from the
     original channel.
-}
newtype LocalChannel era = LocalChannel (TChan (Block era))

{- | A handler used to pass around the path to the server
     and channels used for controlling the server.
-}
data ServerHandler = ServerHandler
  { shSocketPath :: FilePath
  , -- The client will send a `ServerCommand` and the server will
    -- respond with a `ServerResponse`.
    shCommandChannel :: CommandChannel
  }

{- | The commands that control the server. This API is not part of the client
     interface, and in order to call them directly you will need access to the
     returned ServerHandler
-}
data ServerCommand where
  ProcessBlock :: ServerCommand
  ModifySlot :: (Slot -> Slot) -> ServerCommand
  AddTx :: (C.Tx C.ConwayEra) -> ServerCommand

instance Show ServerCommand where
  show = \case
    ProcessBlock -> "ProcessBlock"
    ModifySlot _ -> "ModifySlot"
    AddTx t -> "AddTx " <> show t

{- | The response from the server. Can be used for the information
     passed back, or for synchronisation.
-}
data ServerResponse
  = -- A block was added. We are using this for synchronization.
    BlockAdded (Block C.ConwayEra)
  | SlotChanged Slot

-- deriving (Show)

processBlock :: (MonadIO m) => ServerHandler -> m (Block C.ConwayEra)
processBlock ServerHandler {shCommandChannel} = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) ProcessBlock
  -- Wait for the server to finish processing blocks.
  b <-
    liftIO $
      atomically $
        readTQueue (ccResponse shCommandChannel) >>= \case
          BlockAdded block -> do
            pure block
          _ -> retry
  liftIO (putStrLn $ "new block has been produced:" <> show b)
  pure b

modifySlot :: (MonadIO m) => (Slot -> Slot) -> ServerHandler -> m Slot
modifySlot f ServerHandler {shCommandChannel} = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) $ ModifySlot f
  -- Wait for the server to finish changing the slot.
  liftIO $
    atomically $
      readTQueue (ccResponse shCommandChannel) >>= \case
        SlotChanged slot -> pure slot
        _ -> retry

addTx :: (MonadIO m) => ServerHandler -> C.Tx C.ConwayEra -> m ()
addTx ServerHandler {shCommandChannel} tx = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) $ AddTx tx

{- Create a thread that keeps the number of blocks in the channel to the maximum
   limit of K -}
pruneChain :: (MonadIO m) => Integer -> TChan (Block era) -> m ThreadId
pruneChain k original = do
  localChannel <- liftIO $ atomically $ cloneTChan original
  liftIO . forkIO $ go k localChannel
  where
    go :: (MonadIO m) => Integer -> TChan (Block era) -> m ()
    go k' localChannel = do
      -- Wait for data on the channel
      _ <- liftIO $ atomically $ readTChan localChannel
      {- When the counter reaches zero, there are K blocks in the
                original channel and we start to remove the oldest stored
                block by reading it. -}
      if k' == 0
        then do
          liftIO $ atomically (readTChan original) >> go 0 localChannel
        else do
          go (k' - 1) localChannel

processChainEffects ::
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  ClbT C.ConwayEra IO a ->
  IO a
processChainEffects trace stateVar eff = do
  (events, result) <- runChainEffects stateVar eff
  -- TODO: handle logs from clb
  -- LM.runLogEffects trace $ do
  --   traverse_ (send . LMessage) events
  either throwIO pure result

handleCommand ::
  (MonadIO m) =>
  Trace IO EmulatorMsg ->
  CommandChannel ->
  MVar (AppState C.ConwayEra) ->
  m ()
handleCommand trace CommandChannel {ccCommand, ccResponse} mvAppState = do
  mLogs <-
    liftIO $
      atomically (readTQueue ccCommand) >>= \case
        AddTx tx -> do
          liftIO $ putStrLn "----------------------------------->>> AddTx!"
          -- process $ void $ E.sendTx tx
          pure Nothing
        ModifySlot f -> do
          s <- process $ E.modifySlot f
          atomically $
            writeTQueue ccResponse (SlotChanged s)
          pure Nothing
        ProcessBlock -> do
          (block, logs) <- process E.processBlock
          setTip mvAppState block
          ch <- getChannel mvAppState
          atomically $ do
            writeTChan ch block
            writeTQueue ccResponse (BlockAdded block)
          pure $ Just logs
  case mLogs of
    Nothing -> pure ()
    Just logs -> liftIO $ putStrLn logs
  where
    process :: ClbT C.ConwayEra IO a -> IO a
    process = processChainEffects trace mvAppState

{- | Start the server in a new thread, and return a server handler
     used to control the server
-}
runServerNode ::
  (MonadIO m) =>
  Trace IO EmulatorMsg ->
  C.NetworkMagic ->
  FilePath ->
  Integer ->
  AppState C.ConwayEra ->
  m ServerHandler
runServerNode trace magic shSocketPath k initialState = liftIO $ do
  serverState <- newMVar initialState
  shCommandChannel <- CommandChannel <$> newTQueueIO <*> newTQueueIO
  globalChannel <- getChannel serverState
  void $ forkIO . void $ protocolLoop magic shSocketPath serverState
  void $ forkIO . forever $ handleCommand trace shCommandChannel serverState
  void $ pruneChain k globalChannel
  pure $ ServerHandler {shSocketPath, shCommandChannel}

-- * ChainSync protocol

{- A monad for running all code executed when a state
   transition is invoked. It makes the implementation of
   state transitions easier to read. -}

type ChainSyncMonad era = ReaderT (MVar (AppState era)) IO

runChainSync :: MVar (AppState era) -> ChainSyncMonad era a -> IO a
runChainSync = flip runReaderT

{- The initial state of the protocol. You can move into
   requesting the next block or reset state by searching for an
   intersection. -}
idleState ::
  ( MonadReader (MVar (AppState C.ConwayEra)) m
  , MonadIO m
  , block ~ CardanoBlock StandardCrypto
  ) =>
  LocalChannel C.ConwayEra ->
  m (ServerStIdle block (Point block) Tip m ())
idleState channel' = do
  pure
    ServerStIdle
      { recvMsgRequestNext = nextState channel'
      , recvMsgFindIntersect = findIntersect
      , recvMsgDoneClient = return ()
      }

{- Get the next block, either immediately (the Just/Left branch)
   or within a monad (IO, in our case) where you can wait for the
   next block (Nothing/Right branch) -}
nextState ::
  ( MonadReader (MVar (AppState C.ConwayEra)) m
  , MonadIO m
  , block ~ CardanoBlock StandardCrypto
  ) =>
  LocalChannel C.ConwayEra ->
  m
    ( Either
        (ServerStNext block (Point block) Tip m ())
        (m (ServerStNext block (Point block) Tip m ()))
    )
nextState localChannel@(LocalChannel channel') = do
  chainState <- ask
  tip' <- getTip chainState
  (liftIO . atomically $ tryReadTChan channel') >>= \case
    Nothing -> do
      Right . pure <$> do
        nextBlock <- liftIO . atomically $ readTChan channel'
        cBlock <- liftIO $ toCardanoBlock tip' nextBlock
        sendRollForward localChannel tip' cBlock
    Just nextBlock -> do
      cBlock <- liftIO $ toCardanoBlock tip' nextBlock
      Left <$> sendRollForward localChannel tip' cBlock

{- This protocol state will search for a block intersection
   with some client provided blocks. When an intersection is found
   the client state is reset to the new offset (the Just branch)
   or to the genesis block if no intersection was found. -}
findIntersect ::
  ( MonadReader (MVar (AppState C.ConwayEra)) m
  , MonadIO m
  , block ~ CardanoBlock StandardCrypto
  ) =>
  [Point block] ->
  m (ServerStIntersect block (Point block) Tip m ())
findIntersect clientPoints = do
  mvState <- ask
  appState <- liftIO $ readMVar mvState
  let jumboBlock =
        appState
          ^. socketEmulatorState
            . emulatorState
            . E.emulatedLedgerState
            . E.currentBlock
      -- blocks = Chain._chainNewestFirst chainState
      blocks = [jumboBlock]
      slot =
        getSlot $
          appState
            ^. socketEmulatorState
              . emulatorState
              . E.emulatedLedgerState
  serverPoints <- getChainPoints blocks slot
  let point =
        listToMaybe $
          intersect
            serverPoints
            clientPoints
  tip' <- getTip mvState
  pure $ case point of
    Nothing ->
      SendMsgIntersectNotFound
        O.TipGenesis
        -- No intersection found. Resume from origin.
        (ChainSyncServer $ cloneChainFrom 0 >>= idleState)
    Just point' ->
      SendMsgIntersectFound
        point'
        tip'
        -- Resuming from point'.
        (ChainSyncServer $ cloneChainFrom (pointOffset point') >>= idleState)

{- This is a wrapper around the creation of a `ServerStNext` -}
sendRollForward ::
  ( MonadReader (MVar (AppState C.ConwayEra)) m
  , MonadIO m
  , block ~ CardanoBlock StandardCrypto
  ) =>
  LocalChannel C.ConwayEra ->
  Tip -> -- tip
  block -> -- current
  m (ServerStNext block (Point block) Tip m ())
sendRollForward channel' tip' current =
  pure $
    SendMsgRollForward
      current
      tip'
      (ChainSyncServer (idleState channel'))

{- This is the state for a new connection. For now we start with
   slot 0, and in idleState. This will probably change, since it
   makes more sense to start in the `StIntersect` state. -}
chainSyncServer ::
  ( MonadReader (MVar (AppState C.ConwayEra)) m
  , MonadIO m
  , block ~ CardanoBlock StandardCrypto
  ) =>
  ChainSyncServer block (Point block) Tip m ()
chainSyncServer =
  ChainSyncServer (cloneChainFrom 0 >>= idleState)

{- Use a `TChan` to model a broadcast channel of which we
   clone (with potentially varying offsets) for clients. -}
cloneChainFrom ::
  forall m era.
  ( MonadReader (MVar (AppState era)) m
  , MonadIO m
  ) =>
  Integer ->
  m (LocalChannel era)
cloneChainFrom offset = LocalChannel <$> go
  where
    go :: m (TChan (Block era))
    go = do
      globalChannel <- ask >>= getChannel
      liftIO $ atomically $ do
        localChannel <- cloneTChan globalChannel
        consume localChannel offset

    consume :: TChan a -> Integer -> STM (TChan a)
    consume channel' ix | ix == 0 = pure channel'
    consume channel' ix =
      -- We should have all requested blocks available on the
      -- channel, for consumption.
      tryReadTChan channel' >> consume channel' (ix - 1)

-- * Protocol setup

{- The node protocols always run in the IO monad. I wanted to use a
   different monad stack (mainly to be able to pass the internal state
   in a `MonadReader` and future proofing) so I wrote some hoisting
   functions for each of the states which transform the `ChainSyncMonad`
   into IO. -}

hoistChainSync ::
  (MonadReader (MVar (AppState era)) m) =>
  ChainSyncServer block (Point block) Tip (ChainSyncMonad era) a ->
  m (ChainSyncServer block (Point block) Tip IO a)
hoistChainSync machine = do
  internalState <- ask
  pure
    ChainSyncServer
      { {- The basic idea is running the reader monad to remove it,
           leaving only IO, which is what we need. We do the same for all
           other states. -}
        runChainSyncServer =
          runChainSync internalState $
            runChainSyncServer machine >>= hoistStIdle
      }

hoistStIdle ::
  (MonadReader (MVar (AppState era)) m) =>
  ServerStIdle block (Point block) Tip (ChainSyncMonad era) a ->
  m (ServerStIdle block (Point block) Tip IO a)
hoistStIdle (ServerStIdle nextState' findIntersect' done) = do
  internalState <- ask
  pure
    ServerStIdle
      { recvMsgRequestNext =
          runChainSync internalState $
            nextState' >>= \case
              Left stNext -> Left <$> hoistStNext stNext
              Right mNext -> Right . pure <$> (hoistStNext =<< mNext)
      , recvMsgFindIntersect = \points ->
          runChainSync
            internalState
            (findIntersect' points >>= hoistStIntersect)
      , recvMsgDoneClient = runChainSync internalState done
      }

hoistStIntersect ::
  (MonadReader (MVar (AppState era)) m) =>
  ServerStIntersect block (Point block) Tip (ChainSyncMonad era) a ->
  m (ServerStIntersect block (Point block) Tip IO a)
hoistStIntersect (SendMsgIntersectFound point tip' nextState') =
  SendMsgIntersectFound point tip' <$> hoistChainSync nextState'
hoistStIntersect (SendMsgIntersectNotFound tip' nextState') =
  SendMsgIntersectNotFound tip' <$> hoistChainSync nextState'

hoistStNext ::
  (MonadReader (MVar (AppState era)) m) =>
  ServerStNext block (Point block) Tip (ChainSyncMonad era) a ->
  m (ServerStNext block (Point block) Tip IO a)
hoistStNext (SendMsgRollForward header tip' nextState') =
  SendMsgRollForward header tip' <$> hoistChainSync nextState'
hoistStNext (SendMsgRollBackward header tip' nextState') =
  SendMsgRollBackward header tip' <$> hoistChainSync nextState'

{- This is boilerplate code that sets up the node protocols,
   you can find in:
     ouroboros-network/ouroboros-network/demo/chain-sync.hs -}

protocolLoop ::
  (MonadIO m) =>
  C.NetworkMagic ->
  FilePath ->
  MVar (AppState C.ConwayEra) ->
  m Void
protocolLoop magic socketPath internalState = liftIO $ withIOManager $ \iocp -> do
  networkState <- newNetworkMutableState
  _ <- async $ cleanNetworkMutableState networkState
  withServerNode
    (localSnocket iocp)
    makeLocalBearer
    (\_ _ -> pure ())
    nullNetworkServerTracers
    networkState
    (AcceptedConnectionsLimit maxBound maxBound 0)
    (localAddressFromPath socketPath)
    nodeToClientHandshakeCodec
    noTimeLimitsHandshake
    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
    (HandshakeCallbacks acceptableVersion queryVersion)
    ( SomeResponderApplication
        <$> versionedNodeToClientProtocols
          nodeToClientVersion
          (nodeToClientVersionData magic)
          (nodeToClientProtocols internalState)
    )
    nullErrorPolicies
    $ \_ serverAsync -> wait serverAsync

nodeToClientProtocols ::
  MVar (AppState C.ConwayEra) ->
  NodeToClientProtocols 'ResponderMode LocalAddress LBS.ByteString IO Void ()
nodeToClientProtocols internalState =
  NodeToClientProtocols
    { localChainSyncProtocol = chainSync internalState
    , localTxSubmissionProtocol = txSubmission internalState
    , localStateQueryProtocol = stateQuery internalState
    , localTxMonitorProtocol = doNothingResponderProtocol
    }

chainSync ::
  MVar (AppState C.ConwayEra) ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
chainSync mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeer $
      const
        ( nullTracer
        , chainSyncCodec
        , ChainSync.chainSyncServerPeer
            ( runReader
                (hoistChainSync chainSyncServer)
                mvChainState
            )
        )

txSubmission ::
  MVar (AppState C.ConwayEra) ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
txSubmission mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeer $
      const
        ( nullTracer
        , txSubmissionCodec
        , TxSubmission.localTxSubmissionServerPeer
            (pure $ txSubmissionServer mvChainState)
        )

stateQuery ::
  (HandleQuery era) =>
  MVar (AppState era) ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
stateQuery mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeer $
      const
        ( nullTracer
        , stateQueryCodec
        , Query.localStateQueryServerPeer
            (stateQueryServer mvChainState)
        )

-- * Computing intersections

-- Given a `Point` find its offset into the chain.
pointOffset ::
  Point block ->
  Integer
pointOffset pt =
  case pointSlot pt of
    Origin -> 0
    At (SlotNo s) -> fromIntegral s

-- Currently selects all points from the blockchain.
getChainPoints ::
  (MonadIO m, block ~ CardanoBlock StandardCrypto) =>
  [Block C.ConwayEra] ->
  Slot ->
  m [Point block]
getChainPoints chain slot = do
  pure $ zipWith mkPoint [slot, slot - 1 .. 1] chain ++ [Point Origin]
  where
    mkPoint s block =
      Point
        ( At
            ( OP.Block
                (fromIntegral s)
                (coerce $ blockId block)
            )
        )

-- * TxSubmission protocol

{- I did not use the same approach for this protocol as I did
   for the `ChainSync`. This protocol has only one state and
   it is much simpler. -}

txSubmissionServer ::
  (block ~ CardanoBlock StandardCrypto) =>
  MVar (AppState C.ConwayEra) ->
  TxSubmission.LocalTxSubmissionServer (Shelley.GenTx block) (ApplyTxErr block) IO ()
txSubmissionServer state =
  TxSubmission.LocalTxSubmissionServer
    { TxSubmission.recvMsgSubmitTx = fmap (,txSubmissionServer state) . submitTx state
    , TxSubmission.recvMsgDone = ()
    }

submitTx ::
  (block ~ CardanoBlock StandardCrypto) =>
  MVar (AppState C.ConwayEra) ->
  Shelley.GenTx block ->
  IO (TxSubmission.SubmitResult (ApplyTxErr block))
submitTx state tx = case C.fromConsensusGenTx tx of
  C.TxInMode C.ShelleyBasedEraConway shelleyTx -> do
    putStrLn $ "New tx: " ++ show tx
    AppState
      (SocketEmulatorState clbState@(ClbState chainState _ _ _ _ _) _ _)
      _
      _ <-
      readMVar state
    (res, _state) <- runStateT (unwrapClbT $ Clb.validateTx shelleyTx) clbState
    case res of
      Fail _ err ->
        pure $
          TxSubmission.SubmitFail
            ( Consensus.HardForkApplyTxErrFromEra
                (Consensus.OneEraApplyTxErr (S (S (S (S (S (S (Z (WrapApplyTxErr err)))))))))
            )
      Success ls' _tx -> do
        let ctx = CardanoEmulatorEraTx shelleyTx
        modifyMVar_
          state
          ( pure
              . over
                (socketEmulatorState . emulatorState)
                (addTxToPool ctx . (emulatedLedgerState .~ ls'))
          )
        pure TxSubmission.SubmitSuccess
  C.TxInMode era _ -> do
    putStrLn $ "Unsupported era: " <> show era
    pure $
      TxSubmission.SubmitFail
        ( Consensus.HardForkApplyTxErrWrongEra $
            Consensus.MismatchEraInfo $
              Consensus.MS $
                Consensus.MS $
                  Consensus.MS $
                    Consensus.MS $
                      Consensus.MS $
                        Consensus.ML
                          eraInfoBabbage
                          (Z (Consensus.LedgerEraInfo eraInfoConway))
        )
  C.TxInByronSpecial _ -> do
    putStrLn "Unexpected TxInByronSpecial!"
    pure $
      TxSubmission.SubmitFail
        ( Consensus.HardForkApplyTxErrWrongEra $
            Consensus.MismatchEraInfo $
              Consensus.ML
                eraInfoByron
                (S (S (S (S (S (Z (Consensus.LedgerEraInfo eraInfoConway)))))))
        )

-- -- | Applying a Byron thing to a Shelley ledger
-- exampleEraMismatchShelley :: Consensus.MismatchEraInfo (CardanoEras StandardCrypto)
-- exampleEraMismatchShelley =
--     Consensus.MismatchEraInfo $ Consensus.ML eraInfoByron
--       (S ( S (S (S (S (Z (Consensus.LedgerEraInfo eraInfoConway)))))))

eraInfoConway :: Consensus.SingleEraInfo (ShelleyBlock (Praos StandardCrypto) StandardConway)
eraInfoConway = Consensus.singleEraInfo (Proxy @(ShelleyBlock (Praos StandardCrypto) StandardConway))

eraInfoBabbage :: Consensus.SingleEraInfo (ShelleyBlock (Praos StandardCrypto) StandardBabbage)
eraInfoBabbage = Consensus.singleEraInfo (Proxy @(ShelleyBlock (Praos StandardCrypto) StandardBabbage))

eraInfoShelley :: Consensus.SingleEraInfo (ShelleyBlock (TPraos StandardCrypto) StandardShelley)
eraInfoShelley = Consensus.singleEraInfo (Proxy @(ShelleyBlock (TPraos StandardCrypto) StandardShelley))

eraInfoByron :: Consensus.SingleEraInfo ByronBlock
eraInfoByron = Consensus.singleEraInfo (Proxy @ByronBlock)

-- should be SubmitFail HardForkApplyTxErrWrongEra, but the Mismatch type is complicated

stateQueryServer ::
  (block ~ CardanoBlock StandardCrypto, HandleQuery era) =>
  MVar (AppState era) ->
  StateQuery.LocalStateQueryServer block (Point block) (Query block) IO ()
stateQueryServer state = Query.LocalStateQueryServer {Query.runLocalStateQueryServer = pure idle}
  where
    idle =
      Query.ServerStIdle
        { Query.recvMsgAcquire = acquiring
        , Query.recvMsgDone = pure ()
        }
    acquiring _point = pure $ Query.SendMsgAcquired ack
    ack =
      Query.ServerStAcquired
        { Query.recvMsgQuery = \q -> do
            res <- handleQuery state q
            pure $ Query.SendMsgResult res ack
        , Query.recvMsgReAcquire = acquiring
        , Query.recvMsgRelease = pure idle
        }
