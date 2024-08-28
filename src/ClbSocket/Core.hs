module ClbSocket.Core where

import Cardano.Api (Value)
import Clb (ClbState, ClbT (unwrapClbT), MockConfig, initClb, sendTx)
import ClbSocket (getEmulatorBabbageEraTx)
import ClbSocket.Server (ServerConfig (..))
import ClbSocket.Types (Request (SendTx))
import Control.Concurrent (takeMVar)
import Control.Monad (void)
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
emulatorT (ServerConfig {..}) = do
  request <- liftIO $ takeMVar requestVar
  case request of
    SendTx tx -> void (sendTx $ getEmulatorBabbageEraTx tx) -- TODO : Handle output (ValidationResult)
    _ -> error "undefined"
