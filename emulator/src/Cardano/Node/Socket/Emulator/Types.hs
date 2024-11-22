{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports data types for logging, events and configuration
module Cardano.Node.Socket.Emulator.Types where

import Cardano.Api (ConwayEra, lovelaceToValue)
import Cardano.Api qualified as C
import Cardano.Api.Address (AddressInEra)
import Cardano.BM.Data.Trace (Trace)
import Cardano.Binary qualified as CBOR
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Ledger.Api.Transition (EraTransition)
import Cardano.Ledger.Block qualified as CL
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Era qualified as CL
import Cardano.Ledger.Shelley.API (
  Coin (Coin),
  Nonce (NeutralNonce),
  extractTx,
  unsafeMakeValidated,
 )
import Cardano.Ledger.Shelley.Genesis qualified as SG
import Cardano.Ledger.Shelley.Transition qualified as T
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert (KESPeriod (..))
import Clb (
  CardanoTx,
  ClbConfig (..),
  ClbState,
  ClbT,
  EmulatedLedgerState,
  OnChainTx (..),
  chainState,
 )
import Clb qualified as E
import Clb.EmulatedLedgerState qualified as E (getSlot)
import Clb.Era (CardanoLedgerEra, IsCardanoLedgerEra)
import Clb.TimeSlot (Slot)
import Codec.Serialise (DeserialiseFailure)
import Codec.Serialise qualified as CBOR
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Control.Lens (makeLenses, over, view, (&), (.~), (^.))
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Freer (send)
import Control.Monad.Freer.Extras (
  LogLevel (..),
  LogMessage (LogMessage),
  LogMsg (LMessage),
 )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets, modify)
import Control.Monad.State.Lazy (StateT (runStateT))
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON, ToJSON)
import Data.Base16.Types qualified as B16
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BS
import Data.Coerce (coerce)
import Data.Default (Default, def)
import Data.Foldable (toList, traverse_)
import Data.Functor ((<&>))
import Data.ListMap qualified as LM
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 qualified as F
import Data.Time.Units (Millisecond)
import GHC.Generics (Generic)
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.Byron.Ledger qualified as Byron
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, CodecConfig (..))
import Ouroboros.Consensus.Cardano.Block qualified as OC
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (
  ClientCodecs,
  cChainSyncCodec,
  cStateQueryCodec,
  cTxSubmissionCodec,
  clientCodecs,
 )
import Ouroboros.Consensus.Node.NetworkProtocolVersion (
  BlockNodeToClientVersion,
  supportedNodeToClientVersions,
 )
import Ouroboros.Consensus.Protocol.Praos.Header qualified as Praos
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger qualified as Shelley
import Ouroboros.Network.Block (
  Point,
  pattern BlockPoint,
  pattern GenesisPoint,
 )
import Ouroboros.Network.Block qualified as Ouroboros
import Ouroboros.Network.NodeToClient (
  NodeToClientVersion (..),
  NodeToClientVersionData (..),
 )
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as StateQuery
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as TxSubmission
import Ouroboros.Network.Util.ShowProxy
import Plutus.Monitoring.Util (runLogEffects)
import Prettyprinter (Pretty, pretty, viaShow, (<+>))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Presets (coreNodeKeys)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Protocol.TPraos.Create (mkBlock, mkOCert)

{- | In addition to state handled by CLB emulator as a library ('ClbState')
this data type introduces additional things that are involved when the
emulator is used in "as a cardano-node mode".
-}
data SocketEmulatorState era = SocketEmulatorState
  { _clbState :: ClbState era
  -- ^ the main state from CLB
  , _chainNewestFirst :: !(Blockchain era)
  -- ^ the blockchain
  , _txPool :: !(TxPool era)
  -- ^ transaction mempool
  , _cachedState :: !(EmulatedLedgerState era)
  -- ^ state that reflects all the changes known to the mempool
  -- while in the 'clbState' the state represents the state known
  -- to the blockchain itself
  , _channel :: TChan (Block era)
  , _tip :: Tip
  }
  deriving (Generic)

{- | mempool is just a list of transactions, ordered by the time at which they
arrived (the head is the latest)
-}
type TxPool era = [CardanoTx era]

{- | A block on the blockchain. This is just a list of transactions
following on from the chain so far.
-}
type Block era = [OnChainTx era]

-- | A blockchain, which is just a list of blocks, starting with the newest.
type Blockchain era = [Block era]

type Tip = Ouroboros.Tip (CardanoBlock StandardCrypto)

makeLenses ''SocketEmulatorState

-- | Node server configuration
data NodeServerConfig = NodeServerConfig
  { nscInitialTxWallets :: [Integer]
  -- ^ The wallets that receive money from the initial transaction.
  , nscSocketPath :: FilePath
  -- ^ Path to the socket used to communicate with the server.
  , nscShelleyGenesisPath :: Maybe FilePath
  -- ^ Path to a JSON file containing the Shelley genesis parameters
  , nscAlonzoGenesisPath :: Maybe FilePath
  -- ^ Path to a JSON file containing the Alonzo genesis parameters
  , nscConwayGenesisPath :: Maybe FilePath
  -- ^ Path to a JSON file containing the Conway genesis parameters
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

defaultNodeServerConfig :: NodeServerConfig
defaultNodeServerConfig =
  NodeServerConfig
    { nscInitialTxWallets =
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , -- [ WalletNumber 1
      -- , WalletNumber 2
      -- , WalletNumber 3
      -- , WalletNumber 4
      -- , WalletNumber 5
      -- , WalletNumber 6
      -- , WalletNumber 7
      -- , WalletNumber 8
      -- , WalletNumber 9
      -- , WalletNumber 10
      -- ]
      nscSocketPath = "/tmp/node-server.sock"
    , nscShelleyGenesisPath = Nothing
    , nscAlonzoGenesisPath = Nothing
    , nscConwayGenesisPath = Nothing
    }

instance Default NodeServerConfig where
  def = defaultNodeServerConfig

-- | Application State
newtype AppState era = AppState
  { _socketEmulatorState :: SocketEmulatorState era
  }

-- deriving (Show)

makeLenses 'AppState

type CardanoAddress = AddressInEra ConwayEra

initialChainState ::
  forall m era.
  ( MonadIO m
  , IsCardanoLedgerEra era
  , EraTransition (CardanoLedgerEra era)
  ) =>
  ClbConfig era ->
  m (SocketEmulatorState era)
initialChainState params@ClbConfig {clbConfigConfig} =
  fromEmulatorChainState $
    E.initClb params _dummyTotalNotUsedNow perWallet (Just initialFunds)
  where
    _dummyTotalNotUsedNow = lovelaceToValue $ Coin 1_000_000_000_000
    perWallet = lovelaceToValue $ Coin 1_000_000_000
    initialFunds = LM.toList $ clbConfigConfig ^. (T.tcShelleyGenesisL . SG.sgInitialFundsL)

    fromEmulatorChainState :: ClbState era -> m (SocketEmulatorState era)
    fromEmulatorChainState state = do
      ch <- liftIO $ atomically newTChan
      -- let chainNewestFirst = [view (chainState . currentBlock) state] -- we don't have blocks yet
      -- let currentSlot = view (chainState . ledgerEnv . to ledgerSlotNo) state
      -- void $
      --   liftIO $
      --     mapM_ (atomically . writeTChan ch) chainNewestFirst
      pure $
        SocketEmulatorState
          { _clbState = state
          , _chainNewestFirst = mempty
          , _txPool = mempty
          , _cachedState = state ^. E.chainState
          , _channel = ch
          , -- Is this correct? There is no way to produce a tip if there are no blocks.
            _tip = Ouroboros.TipGenesis
          }

getChannel :: (MonadIO m) => MVar (AppState era) -> m (TChan (Block era))
getChannel mv = liftIO (readMVar mv) <&> view (socketEmulatorState . channel)

-- Get the current tip.
getTip :: (MonadIO m) => MVar (AppState era) -> m Tip
getTip mv = liftIO (readMVar mv) <&> view (socketEmulatorState . tip)

-- Get so-called "chain point" time
getChainPointTime ::
  ( MonadIO m
  , block ~ CardanoBlock StandardCrypto
  , CBOR.ToCBOR (Core.Tx (CardanoLedgerEra era))
  ) =>
  MVar (AppState era) ->
  m (Point block)
getChainPointTime mv = do
  st <- liftIO (readMVar mv)
  case listToMaybe $ st ^. socketEmulatorState . chainNewestFirst of
    Just newest -> do
      let slot = E.getSlot $ st ^. socketEmulatorState . clbState . chainState
      pure $ BlockPoint slot (coerce $ blockId newest)
    Nothing -> pure GenesisPoint

-- Set the new tip
setTip :: (MonadIO m, CBOR.ToCBOR (Core.Tx (CardanoLedgerEra era))) => MVar (AppState era) -> Block era -> m ()
setTip mv block = liftIO $ modifyMVar_ mv $ \oldState -> do
  let slot =
        E.getSlot $
          oldState
            ^. socketEmulatorState
              . clbState
              . E.chainState
  pure $
    oldState
      & socketEmulatorState . tip
        .~ Ouroboros.Tip (fromInteger slot) (coerce $ blockId block) (fromInteger slot)

-- Set the new tip
setTip' :: (CBOR.ToCBOR (Core.Tx (CardanoLedgerEra era))) => AppState era -> Block era -> AppState era
setTip' oldState block =
  let slot =
        E.getSlot $
          oldState
            ^. socketEmulatorState
              . clbState
              . E.chainState
   in oldState
        & socketEmulatorState . tip
          .~ Ouroboros.Tip (fromInteger slot) (coerce $ blockId block) (fromInteger slot)

-- -----------------------------------------------------------------------------
-- CLB utils
-- -----------------------------------------------------------------------------

-- | Run CLB in the IO Monad returning logs and the result or an error.
runClbInIO ::
  MVar (AppState era) ->
  ClbT era IO a ->
  IO (EmulatorLogs, Either EmulatorError a)
runClbInIO stateVar eff =
  modifyMVar stateVar $ \appState -> do
    let s = appState ^. (socketEmulatorState . clbState)
    let action = do
          ret <- eff
          logs <- extractLogs
          mErr <- E.checkErrors
          case mErr of
            Nothing -> pure (logs, Right ret)
            Just err -> pure (logs, Left err)
    (a, s') <- runStateT (E.unwrapClbT action) s
    pure (appState & (socketEmulatorState . clbState) .~ s', a)

runClbInIO' ::
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  ClbT C.ConwayEra IO a ->
  IO (Either EmulatorError a)
runClbInIO' trace stateVar eff = do
  (events, result) <- runClbInIO stateVar eff
  runLogEffects trace $ do
    traverse_ (send . LMessage . convClbLog) (E.fromLog events)
  pure result

runClb ::
  Trace IO EmulatorMsg ->
  MVar (AppState C.ConwayEra) ->
  ClbT C.ConwayEra IO a ->
  IO a
runClb trace mvAppState action = do
  runClbInIO' trace mvAppState action >>= either throwIO pure

-- runClbWithApp ::
--   MVar (AppState C.ConwayEra) ->
--   (AppState C.ConwayEra -> ClbT C.ConwayEra IO (AppState C.ConwayEra, a)) ->
--   IO (AppState C.ConwayEra, a)
-- runClbWithApp mvAppState action =
--   modifyMVar mvAppState $ \appState -> do
--     let s = appState ^. (socketEmulatorState . clbState)
--     ((newAppState, a), s') <- runStateT (unwrapClbT (action appState)) s
--     let newAppState' = newAppState & (socketEmulatorState . clbState) .~ s'
--     pure (newAppState', (newAppState', a))

convClbLog :: (Slot, E.LogEntry) -> LogMessage String
convClbLog (slot, entry) =
  let lvl = case E.leLevel entry of
        E.Debug -> Debug
        E.Info -> Info
        E.Warning -> Warning
        E.Error -> Error
   in LogMessage lvl $ show (pretty slot) <> ": " <> E.leMsg entry

-- Extracts logs from Clb computation, and clears them up.
extractLogs :: (Monad m) => ClbT era m EmulatorLogs
extractLogs = do
  theLog <- gets (^. E.clbLog)
  modify (over E.clbLog (const mempty))
  pure theLog

-- -- Extracts logs from Clb computation, and clears them up.
-- extractLogsAsString :: (Monad m) => ClbT era m String
-- extractLogsAsString = do
--   theLog <- gets (^. E.clbLog)
--   modify (over E.clbLog (const mempty))
--   let logDoc = E.ppLog theLog
--   let options = defaultLayoutOptions {layoutPageWidth = AvailablePerLine 150 1.0}
--   let logString = renderString $ layoutPretty options logDoc
--   let clbLog = "a\nEmulator log :\n--------------\n" <> logString
--   pure clbLog

type EmulatorLogs = E.Log E.LogEntry
type EmulatorError = String
instance Exception EmulatorError

-- -----------------------------------------------------------------------------
-- Logging
-- -----------------------------------------------------------------------------

{- | Top-level logging data type for structural logging
inside the CNSE server.
-}
data CNSEServerLogMsg
  = StartingSlotCoordination UTCTime Millisecond
  | StartingCNSEServer
  | ProcessingEmulatorMsg EmulatorMsg
  deriving (Generic, Show)

type EmulatorMsg = String

instance Pretty CNSEServerLogMsg where
  pretty = \case
    StartingSlotCoordination initialSlotTime slotLength ->
      "Starting slot coordination thread."
        <+> "Initial slot time:"
        <+> pretty (F.iso8601Show initialSlotTime)
        <+> "Slot length:"
        <+> viaShow slotLength
    StartingCNSEServer -> "Starting Cardano Node Emulator"
    ProcessingEmulatorMsg e -> pretty e

-- | The node protocols require a block header type.
newtype BlockId = BlockId {getBlockId :: BS.ShortByteString}
  deriving (Eq, Ord, Generic)
  deriving newtype (CBOR.Serialise)
  deriving (Pretty) via (PrettyShow BlockId)

-- | Newtype wrapper for deriving 'Pretty' via a 'Show' instance
newtype PrettyShow a = PrettyShow {unPrettyShow :: a}

instance (Show a) => Pretty (PrettyShow a) where
  pretty = viaShow . unPrettyShow

instance Show BlockId where
  show = Text.unpack . B16.extractBase16 . B16.encodeBase16 . BS.fromShort . getBlockId

-- | A hash of the block's contents.
blockId :: (CBOR.ToCBOR (Core.Tx (CardanoLedgerEra era))) => Block era -> BlockId
blockId =
  BlockId
    . BS.toShort
    . BA.convert
    . hash @_ @SHA256
    . BSL.toStrict
    . CBOR.serialise

-- | Protocol versions
nodeToClientVersion :: NodeToClientVersion
nodeToClientVersion = NodeToClientV_16

{- | A temporary definition of the protocol version. This will be moved as an
argument to the client connection function in a future PR (the network magic
number matches the one in the test net created by scripts)
-}
nodeToClientVersionData :: C.NetworkMagic -> NodeToClientVersionData
nodeToClientVersionData magic = NodeToClientVersionData {networkMagic = magic, query = False}

-- | Boilerplate codecs used for protocol serialisation.

{- | The number of epochSlots is specific to each blockchain instance. This value
is what the cardano main and testnet uses. Only applies to the Byron era.
-}
epochSlots :: EpochSlots
epochSlots = EpochSlots 21_600

codecVersion :: BlockNodeToClientVersion (CardanoBlock StandardCrypto)
codecVersion = versionMap Map.! nodeToClientVersion
  where
    versionMap =
      supportedNodeToClientVersions
        (Proxy @(CardanoBlock StandardCrypto))

codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
codecConfig =
  CardanoCodecConfig
    (Byron.ByronCodecConfig epochSlots)
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig
    Shelley.ShelleyCodecConfig

nodeToClientCodecs ::
  forall m.
  (MonadST m) =>
  ClientCodecs (CardanoBlock StandardCrypto) m
nodeToClientCodecs =
  clientCodecs codecConfig codecVersion nodeToClientVersion

{- | These codecs are currently used in the mock nodes and will
  probably soon get removed as the mock nodes are phased out.
-}
chainSyncCodec ::
  (block ~ CardanoBlock StandardCrypto) =>
  Codec
    (ChainSync.ChainSync block (Point block) Tip)
    DeserialiseFailure
    IO
    BSL.ByteString
chainSyncCodec = cChainSyncCodec nodeToClientCodecs

txSubmissionCodec ::
  (block ~ CardanoBlock StandardCrypto) =>
  Codec
    (TxSubmission.LocalTxSubmission (Shelley.GenTx block) (ApplyTxErr block))
    DeserialiseFailure
    IO
    BSL.ByteString
txSubmissionCodec = cTxSubmissionCodec nodeToClientCodecs

stateQueryCodec ::
  (block ~ CardanoBlock StandardCrypto) =>
  Codec
    (StateQuery.LocalStateQuery block (Point block) (Query block))
    DeserialiseFailure
    IO
    BSL.ByteString
stateQueryCodec = cStateQueryCodec nodeToClientCodecs

toCardanoBlock ::
  Ouroboros.Tip (CardanoBlock StandardCrypto) -> Block ConwayEra -> IO (CardanoBlock StandardCrypto)
toCardanoBlock Ouroboros.TipGenesis _ = error "toCardanoBlock: TipGenesis not supported"
toCardanoBlock (Ouroboros.Tip curSlotNo _ curBlockNo) block = do
  prevHash <- generate (arbitrary :: Gen (HashHeader (OC.EraCrypto (OC.ConwayEra StandardCrypto))))
  let allPoolKeys = snd $ head $ coreNodeKeys defaultConstants
      kesPeriod = 1
      keyRegKesPeriod = 1
      ocert = mkOCert allPoolKeys 1 (KESPeriod kesPeriod)
      txs = extractTx . getOnChainTx <$> block
      CL.Block hdr1 bdy =
        mkBlock
          prevHash
          allPoolKeys
          txs
          curSlotNo
          curBlockNo
          NeutralNonce
          kesPeriod
          keyRegKesPeriod
          ocert
  let translateHeader (BHeader bhBody bhSig) = Praos.Header hBody hSig
        where
          hBody =
            Praos.HeaderBody
              { Praos.hbBlockNo = bheaderBlockNo bhBody
              , Praos.hbSlotNo = bheaderSlotNo bhBody
              , Praos.hbPrev = bheaderPrev bhBody
              , Praos.hbVk = bheaderVk bhBody
              , Praos.hbVrfVk = bheaderVrfVk bhBody
              , Praos.hbVrfRes = coerce $ bheaderEta bhBody
              , Praos.hbBodySize = bsize bhBody
              , Praos.hbBodyHash = bhash bhBody
              , Praos.hbOCert = bheaderOCert bhBody
              , Praos.hbProtVer = bprotver bhBody
              }
          hSig = coerce bhSig
  pure $ OC.BlockConway $ Shelley.mkShelleyBlock $ CL.Block (translateHeader hdr1) bdy

fromCardanoBlock :: CardanoBlock StandardCrypto -> Block ConwayEra
fromCardanoBlock (OC.BlockConway (Shelley.ShelleyBlock (CL.Block _ txSeq) _)) = map (OnChainTx . unsafeMakeValidated) . toList $ CL.fromTxSeq txSeq
fromCardanoBlock _ = []

addTxToPool :: CardanoTx era -> SocketEmulatorState era -> SocketEmulatorState era
addTxToPool tx = over txPool (tx :)
