{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Socket.Emulator.Server (
  -- * Server runner
  runServerNode,
  ServerHandler,

  -- * Actions
  tryProduceBlock,
  modifySlot,
) where

import Control.Concurrent (
  MVar,
  ThreadId,
  forkIO,
  modifyMVar,
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
import Control.Lens (over, (&), (.~), (^.))
import Control.Monad (forever, void)
import Control.Monad.Class.MonadTimer (MonadDelay (threadDelay), MonadTimer)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (
  MonadReader (ask),
  ReaderT (runReaderT),
  runReader,
 )
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy qualified as LBS

import Cardano.Api qualified as C
import Cardano.Api.InMode qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.BM.Data.Trace (Trace)
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Node.Socket.Emulator.Query (HandleQuery (handleQuery))
import Cardano.Node.Socket.Emulator.Types (
  AppState (..),
  Block,
  BlockId (BlockId),
  EmulatorMsg,
  Tip,
  addTxToPool,
  blockId,
  cachedState,
  chainNewestFirst,
  chainSyncCodec,
  clbState,
  getChannel,
  getTip,
  nodeToClientVersion,
  nodeToClientVersionData,
  runClb,
  setTip',
  socketEmulatorState,
  stateQueryCodec,
  toCardanoBlock,
  txPool,
  txSubmissionCodec,
 )
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Clb (ValidationResult (..))
import Clb qualified as E
import Clb.EmulatedLedgerState qualified as E
import Clb.Era (IsCardanoLedgerEra)
import Clb.TimeSlot (Slot)
import Clb.Tx (pattern CardanoEmulatorEraTx)
import Clb.Tx qualified as E (getEmulatorEraTx)
import Control.Monad.Freer.Extras (logInfo)
import Control.Monad.State (StateT (runStateT), evalStateT, modify)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.List (intersect)
import Data.Map qualified as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.SOP.Strict (NS (S, Z))
import Data.Void (Void)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardBabbage, StandardConway)
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.TypeFamilyWrappers (WrapApplyTxErr (WrapApplyTxErr))
import Ouroboros.Network.Block (Point (..), pointSlot)
import Ouroboros.Network.Block qualified as O
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Mux (
  MiniProtocolCb (MiniProtocolCb),
  MuxMode (ResponderMode),
  RunMiniProtocol (ResponderProtocolOnly),
  RunMiniProtocolWithMinimalCtx,
  mkMiniProtocolCbFromPeer,
 )
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
import Ouroboros.Network.Protocol.Handshake.Codec (
  cborTermVersionDataCodec,
  noTimeLimitsHandshake,
  nodeToClientHandshakeCodec,
 )
import Ouroboros.Network.Protocol.Handshake.Version (
  Acceptable (acceptableVersion),
  Queryable (queryVersion),
 )
import Ouroboros.Network.Protocol.LocalStateQuery.Server qualified as Query
import Ouroboros.Network.Protocol.LocalStateQuery.Server qualified as StateQuery
import Ouroboros.Network.Protocol.LocalTxSubmission.Server qualified as TxSubmission
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as TxSubmission
import Ouroboros.Network.Snocket (
  LocalAddress,
  localAddressFromPath,
  localSnocket,
  makeLocalBearer,
 )
import Ouroboros.Network.Socket (
  AcceptedConnectionsLimit (AcceptedConnectionsLimit),
  HandshakeCallbacks (HandshakeCallbacks),
  SomeResponderApplication (SomeResponderApplication),
  cleanNetworkMutableState,
  newNetworkMutableState,
  nullNetworkServerTracers,
  withServerNode,
 )
import Plutus.Monitoring.Util (runLogEffects)
import PlutusLedgerApi.V1 qualified as PV1

-- -----------------------------------------------------------------------------
-- Server API
-- -----------------------------------------------------------------------------

{- | A handler used to pass around the path to the server
     and channels used for controlling the server.
-}
data ServerHandler = ServerHandler
  { shSocketPath :: FilePath
  , shCommandChannel :: CommandChannel
  }

{- | The client (here client is the code that calls the server)
     will send a `ServerCommand` and the server will
     respond with a `ServerResponse`.
-}
data CommandChannel = CommandChannel
  { ccCommand :: TQueue ServerCommand
  , ccResponse :: TQueue ServerResponse
  }

{- | Clone the original channel for each connected client, then use
     this wrapper to make sure that no data is consumed from the
     original channel.
-}
newtype LocalChannel era = LocalChannel (TChan (Block era))

{- | The commands that control the server. This API is not part of the client
     interface, and in order to call them directly you will need access to the
     returned ServerHandler
-}
data ServerCommand where
  -- | Try to produce a new block.
  TryProduceBlock :: ServerCommand
  -- | Switch to some (usually next) slot.
  ModifySlot :: (Slot -> Slot) -> ServerCommand

instance Show ServerCommand where
  show = \case
    TryProduceBlock -> "TryProduceBlock"
    ModifySlot _ -> "ModifySlot"

{- | The response from the server. Can be used for the information
     passed back, or for synchronisation.
-}
data ServerResponse
  = -- A block was added. We are using this for synchronization.
    BlockAdded (Maybe (Block C.ConwayEra))
  | SlotChanged Slot

{- | Start the server in a new thread, and return a server handler
     used to control the server
-}
runServerNode ::
  (MonadIO m) =>
  Trace IO EmulatorMsg ->
  C.NetworkMagic ->
  FilePath ->
  -- |  Number of blocks to keep in the global channel
  Integer ->
  AppState C.ConwayEra ->
  m ServerHandler
runServerNode trace magic shSocketPath k appState = liftIO $ do
  -- Allocate MVar for the global state and run protocol loop
  serverState <- newMVar appState
  void $ forkIO . void $ protocolLoop trace magic shSocketPath serverState
  -- Run command handle loop
  shCommandChannel <- CommandChannel <$> newTQueueIO <*> newTQueueIO
  void $ forkIO . forever $ handleCommand trace shCommandChannel serverState
  -- Block pruning thread
  globalChannel <- getChannel serverState
  void $ runPruneChainThread k globalChannel
  -- Return the handler to work with the server
  pure $ ServerHandler {shSocketPath, shCommandChannel}

tryProduceBlock :: (MonadIO m) => ServerHandler -> m (Maybe (Block C.ConwayEra))
tryProduceBlock ServerHandler {shCommandChannel} = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) TryProduceBlock
  -- Wait for the server to finish processing blocks.
  liftIO $
    atomically $
      readTQueue (ccResponse shCommandChannel) >>= \case
        BlockAdded block -> do
          pure block
        _ -> retry

modifySlot :: (MonadIO m) => (Slot -> Slot) -> ServerHandler -> m Slot
modifySlot f ServerHandler {shCommandChannel} = do
  liftIO $ atomically $ writeTQueue (ccCommand shCommandChannel) $ ModifySlot f
  -- Wait for the server to finish changing the slot.
  liftIO $
    atomically $
      readTQueue (ccResponse shCommandChannel) >>= \case
        SlotChanged slot -> pure slot
        _ -> retry

-- -----------------------------------------------------------------------------
-- Command handling
-- -----------------------------------------------------------------------------

handleCommand ::
  (MonadIO m) =>
  Trace IO EmulatorMsg ->
  CommandChannel ->
  MVar (AppState C.ConwayEra) ->
  m ()
handleCommand trace CommandChannel {ccCommand, ccResponse} mvAppState = do
  liftIO $
    atomically (readTQueue ccCommand) >>= \case
      ModifySlot f -> do
        s <- runClb trace mvAppState (E.modifySlot f)
        atomically $
          writeTQueue ccResponse (SlotChanged s)
      TryProduceBlock -> do
        mbRet <- processBlock trace mvAppState
        case mbRet of
          Nothing -> do
            atomically $ writeTQueue ccResponse (BlockAdded Nothing)
          Just block -> do
            ch <- getChannel mvAppState
            atomically $ do
              writeTChan ch block
              writeTQueue ccResponse (BlockAdded $ Just block)

{- | Evaluates all Txs in the mempool and adds valid ones to a new block.
     Invalid transaxtions are just get dumped.
-}
processBlock ::
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  IO (Maybe (Block C.ConwayEra))
processBlock trace mvAppState =
  liftIO $ modifyMVar mvAppState $ \appState -> do
    runLogEffects trace $ do
      let poolTxs = appState ^. (socketEmulatorState . txPool)
      case poolTxs of
        [] -> do
          logInfo @EmulatorMsg "Skipping block producing, txPool is empty."
          return (appState, Nothing)
        txs -> do
          logInfo @EmulatorMsg "Producing a new block."
          (newBlock, blockDatums) <-
            liftIO $
              foldl (\(b, ml) (t, mr) -> (t : b, ml `M.union` mr)) ([], mempty)
                . catMaybes
                <$> mapM (processSingleTx . E.getEmulatorEraTx) txs
          logInfo @EmulatorMsg $ "Block is: " <> show newBlock
          logInfo @EmulatorMsg "Updating chain state..."
          let s = appState ^. (socketEmulatorState . clbState)
          (_, s') <-
            runStateT
              ( modify $
                  over E.chainState (const $ appState ^. (socketEmulatorState . cachedState))
                    . over E.knownDatums (`M.union` blockDatums)
              )
              s
          let newAppState =
                appState
                  & over (socketEmulatorState . txPool) (const mempty)
                  & over (socketEmulatorState . chainNewestFirst) (newBlock :)
                  & flip setTip' newBlock
                  & over (socketEmulatorState . clbState) (const s')
          return (newAppState, Just newBlock)

-- -----------------------------------------------------------------------------
-- Protocol Loop
-- -----------------------------------------------------------------------------

{- This is boilerplate code that sets up the node protocols,
   you can find in:
     ouroboros-network/ouroboros-network/demo/chain-sync.hs -}

protocolLoop ::
  (MonadIO m) =>
  Trace IO EmulatorMsg ->
  C.NetworkMagic ->
  FilePath ->
  MVar (AppState C.ConwayEra) ->
  m Void
protocolLoop trace magic socketPath internalState = liftIO $ withIOManager $ \iocp -> do
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
          (nodeToClientProtocols trace internalState)
    )
    nullErrorPolicies
    $ \_ serverAsync -> wait serverAsync

nodeToClientProtocols ::
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  NodeToClientProtocols 'ResponderMode LocalAddress LBS.ByteString IO Void ()
nodeToClientProtocols trace internalState =
  NodeToClientProtocols
    { localChainSyncProtocol = chainSync internalState
    , localTxSubmissionProtocol = txSubmission trace internalState
    , localStateQueryProtocol = stateQuery trace internalState
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
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
txSubmission trace mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeer $
      const
        ( nullTracer
        , txSubmissionCodec
        , TxSubmission.localTxSubmissionServerPeer
            (pure $ txSubmissionServer trace mvChainState)
        )

stateQuery ::
  (HandleQuery era) =>
  Trace IO EmulatorMsg ->
  MVar (AppState era) ->
  RunMiniProtocolWithMinimalCtx 'ResponderMode LocalAddress LBS.ByteString IO Void ()
stateQuery trace mvChainState =
  ResponderProtocolOnly $
    mkMiniProtocolCbFromPeer $
      const
        ( nullTracer
        , stateQueryCodec
        , Query.localStateQueryServerPeer
            (stateQueryServer trace mvChainState)
        )

doNothingResponderProtocol ::
  (MonadTimer m) =>
  RunMiniProtocolWithMinimalCtx
    'ResponderMode
    LocalAddress
    BSL.ByteString
    m
    Void
    a
doNothingResponderProtocol =
  ResponderProtocolOnly $
    MiniProtocolCb $
      \_ _ -> forever $ threadDelay 1_000_000

-- -----------------------------------------------------------------------------
-- ChainSync protocol
-- -----------------------------------------------------------------------------

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
  let blocks =
        appState
          ^. socketEmulatorState
            . chainNewestFirst
      slot =
        E.getSlot $
          appState
            ^. socketEmulatorState
              . clbState
              . E.chainState
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

-- * Calculating intersections

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

-- Given a `Point` find its offset into the chain.
pointOffset ::
  Point block ->
  Integer
pointOffset pt =
  case pointSlot pt of
    Origin -> 0
    At (SlotNo s) -> fromIntegral s

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

-- -----------------------------------------------------------------------------
-- TxSubmission protocol
-- -----------------------------------------------------------------------------

{- I did not use the same approach for this protocol as I did
   for the `ChainSync`. This protocol has only one state and
   it is much simpler. -}

txSubmissionServer ::
  forall block.
  (block ~ CardanoBlock StandardCrypto) =>
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  TxSubmission.LocalTxSubmissionServer (Shelley.GenTx block) (ApplyTxErr block) IO ()
txSubmissionServer trace state =
  TxSubmission.LocalTxSubmissionServer
    { TxSubmission.recvMsgSubmitTx = fmap (,txSubmissionServer trace state) . submitTx trace state
    , TxSubmission.recvMsgDone = ()
    }

submitTx ::
  (block ~ CardanoBlock StandardCrypto) =>
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  Shelley.GenTx block ->
  IO (TxSubmission.SubmitResult (ApplyTxErr block))
submitTx trace state tx = case C.fromConsensusGenTx tx of
  C.TxInMode C.ShelleyBasedEraConway shelleyTx -> do
    runLogEffects trace $ do
      logInfo $ "New tx: " ++ show tx
      appState <- liftIO $ readMVar state
      let cs = appState ^. (socketEmulatorState . clbState)
      let els = appState ^. (socketEmulatorState . cachedState)
      globals <- evalStateT (E.unwrapClbT E.getGlobals) cs
      let res = E.validateTx globals els shelleyTx
      case res of
        Fail _ err ->
          pure $
            TxSubmission.SubmitFail
              ( Consensus.HardForkApplyTxErrFromEra
                  (Consensus.OneEraApplyTxErr (S (S (S (S (S (S (Z (WrapApplyTxErr err)))))))))
              )
        Success ls' _tx -> do
          let ctx = CardanoEmulatorEraTx shelleyTx
          liftIO $
            modifyMVar_
              state
              ( pure
                  . over
                    socketEmulatorState
                    (addTxToPool ctx . (cachedState .~ ls'))
              )
          pure TxSubmission.SubmitSuccess
  -- TODO: handle other possible cases?
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

eraInfoConway :: Consensus.SingleEraInfo (ShelleyBlock (Praos StandardCrypto) StandardConway)
eraInfoConway = Consensus.singleEraInfo (Proxy @(ShelleyBlock (Praos StandardCrypto) StandardConway))

eraInfoBabbage :: Consensus.SingleEraInfo (ShelleyBlock (Praos StandardCrypto) StandardBabbage)
eraInfoBabbage = Consensus.singleEraInfo (Proxy @(ShelleyBlock (Praos StandardCrypto) StandardBabbage))

eraInfoByron :: Consensus.SingleEraInfo ByronBlock
eraInfoByron = Consensus.singleEraInfo (Proxy @ByronBlock)

-- This is called when we move a tx from mempool into a new block.
processSingleTx ::
  (IsCardanoLedgerEra era) =>
  C.Tx era ->
  IO (Maybe (E.OnChainTx era, M.Map PV1.DatumHash PV1.Datum))
processSingleTx tx@(C.ShelleyTx _ stx) = do
  -- let apiTx = C.ShelleyTx shelleyBasedEra (L.extractTx $ getOnChainTx vtx)
  let txBody = C.getTxBody tx
  let txDatums = E.txWitnessDatums txBody `M.union` E.txInlineDatums txBody
  -- There is no real need to re-validate transactions by this stage
  -- since we have guarantees that the slot number is still the same
  -- as it was when a transaction entered the mempool.
  pure $ Just (E.OnChainTx $ L.unsafeMakeValidated stx, txDatums)

-- -----------------------------------------------------------------------------
-- State query protocol, see Query.hs
-- -----------------------------------------------------------------------------

stateQueryServer ::
  (block ~ CardanoBlock StandardCrypto, HandleQuery era) =>
  Trace IO EmulatorMsg ->
  MVar (AppState era) ->
  StateQuery.LocalStateQueryServer block (Point block) (Query block) IO ()
stateQueryServer trace state = Query.LocalStateQueryServer {Query.runLocalStateQueryServer = pure idle}
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
            res <- handleQuery trace state q
            pure $ Query.SendMsgResult res ack
        , Query.recvMsgReAcquire = acquiring
        , Query.recvMsgRelease = pure idle
        }

-- -----------------------------------------------------------------------------
-- Utility: chain pruning
-- -----------------------------------------------------------------------------

{- Create a thread that keeps the number of blocks in the original channel
   to the maximum limit of K -}
runPruneChainThread :: (MonadIO m) => Integer -> TChan (Block era) -> m ThreadId
runPruneChainThread k original = do
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
