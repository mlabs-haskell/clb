module ClbSocket.Core (
  EmulatorConfig (..),
  runEmulator,
) where

import Cardano.Api (Value)
import Clb (ClbState, ClbT (unwrapClbT), MockConfig, initClb, sendTx)
import ClbSocket.Serialise (Response (SendTxResponse))
import ClbSocket.Serialise.Tx (getEmulatorBabbageEraTx)
import ClbSocket.Server (ServerConfig (..))
import ClbSocket.Types (Request (SendTx))
import Control.Concurrent (putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.State (MonadIO, StateT (runStateT), liftIO)

data EmulatorConfig = EmulatorConfig
  { mockConfig :: MockConfig
  , totalValue :: Value
  , walletFunds :: Value
  }

runEmulator :: EmulatorConfig -> ServerConfig -> IO ClbState
runEmulator (EmulatorConfig {..}) serverConfig = do
  let initClbState = initClb mockConfig totalValue walletFunds
  (_, finalClbState) <- runStateT (unwrapClbT $ emulatorT serverConfig) initClbState
  return finalClbState

emulatorT :: (Monad m, MonadIO m) => ServerConfig -> ClbT m ()
emulatorT (ServerConfig {..}) = forever $ do
  request <- liftIO $ takeMVar requestVar
  response <- case request of
    SendTx tx -> SendTxResponse <$> sendTx (getEmulatorBabbageEraTx tx)
    _ -> error "undefined"
  liftIO $ putMVar responseVar response
