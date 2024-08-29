module ClbSocket.Server (runServer, ServerConfig (..)) where

import ClbSocket.Parse (parseRequest)
import ClbSocket.Serialise (Response, serializeResponse)
import ClbSocket.Types (EmulatorControl, Request)
import Control.Concurrent (MVar, forkIO, putMVar, takeMVar, threadDelay)
import Control.Exception (IOException, catch, finally)
import Control.Monad (forever)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BSL
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  Socket,
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultProtocol,
  isSupportedSockAddr,
  listen,
  socket,
 )
import Network.Socket.ByteString.Lazy qualified as NBL

data ServerConfig = ServerConfig
  { commSocketPath :: FilePath
  , controlSocketPath :: FilePath
  , requestVar :: MVar Request
  , responseVar :: MVar Response
  , controlVar :: MVar EmulatorControl
  }

-- TODO: Graceful Shutdown, Error Handling, Logging
runServer :: ServerConfig -> IO ()
runServer (ServerConfig {..}) = do
  -- Set up communication socket
  commSocket <-
    setupSocket commSocketPath >>= \case
      Right sock -> pure sock
      Left e -> putStrLn "Failed to setup commSocket!" >> error e

  -- Set up control socket
  controlSocket <-
    setupSocket controlSocketPath >>= \case
      Right sock -> pure sock
      Left e -> putStrLn "Failed to setup controlSocket!" >> error e

  -- Fork a thread to handle communication socket
  _ <- forkIO $ forever $ do
    (conn, _) <- accept commSocket
    handleCommConnection conn `finally` close conn

  -- Fork a thread to handle control socket
  _ <- forkIO $ forever $ do
    (conn, _) <- accept controlSocket
    handleControlConnection conn `finally` close conn

  -- Keep the main thread alive
  forever $ threadDelay maxBound
  where
    handleCommConnection conn = do
      handleRequest conn requestVar
      handleResponse conn responseVar

    handleControlConnection conn = handleEmulatorControl conn controlVar

handleRequest :: Socket -> MVar Request -> IO ()
handleRequest conn requestVar = do
  msg <- recvFull conn
  case parseRequest msg of -- Parse the received data (JSON to Haskell data type)
    Right request -> putMVar requestVar request
    Left e -> putStrLn e

handleResponse :: Socket -> MVar Response -> IO ()
handleResponse conn responseVar = do
  response <- takeMVar responseVar
  NBL.sendAll conn (serializeResponse response) -- Serialize and send the response (Haskell data type to CBOR)

handleEmulatorControl :: Socket -> MVar EmulatorControl -> IO ()
handleEmulatorControl conn controlVar = do
  msg <- recvFull conn
  case decode msg of -- Parse the received data (JSON to Haskell data type)
    Just control -> putMVar controlVar control
    Nothing -> putStrLn "Failed to decode emulator control msg!"

-- Helper Functions

-- | Function to receive a full message, handling cases where the message exceeds the buffer size
recvFull :: Socket -> IO BSL.ByteString
recvFull conn = loop BSL.empty
  where
    loop acc = do
      chunk <- NBL.recv conn 16_384 -- Buffer set to 16,384 bytes to match Cardano's max transaction size, optimizing performance at the expense of memory.
      if BSL.null chunk
        then return acc -- No more data to read
        else loop (acc <> chunk)

-- | Sets up a UNIX domain socket at the given file path, returning either an error message or the socket.
setupSocket :: FilePath -> IO (Either String Socket)
setupSocket socketPath = do
  sock <- socket AF_UNIX Stream defaultProtocol
  let sockAddr = SockAddrUnix socketPath
  if isSupportedSockAddr sockAddr
    then
      catch
        ( do
            bind sock sockAddr
            listen sock 5
            return $ Right sock
        )
        (\e -> return $ Left $ "Failed to bind socket: " <> show @IOException e)
    else return $ Left $ "Unsupported Socket Address: " <> socketPath
