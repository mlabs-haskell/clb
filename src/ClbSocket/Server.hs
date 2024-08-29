module ClbSocket.Server (runServer, ServerConfig (..)) where

import ClbSocket.Parse (parseRequest)
import ClbSocket.Serialise (Response, serializeResponse)
import ClbSocket.Types (Request)
import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (IOException, catch)
import Control.Monad (forever)
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
  }

-- TODO: Concurrency, Error Handling, Logging
-- TODO: implement Control Socket
runServer :: ServerConfig -> IO ()
runServer (ServerConfig {..}) = do
  commSocket <-
    setupSocket commSocketPath >>= \case
      Right sock -> pure sock
      Left e -> error e

  -- Accept connections and handle them
  forever $ do
    (conn, _) <- accept commSocket
    handleRequest conn requestVar
    handleResponse conn responseVar
    close conn

handleRequest :: Socket -> MVar Request -> IO ()
handleRequest conn requestVar = do
  -- Receive data from the client
  msg <- recvFull conn
  case parseRequest msg of -- Parse the received data (JSON to Haskell data type)
    Right request -> putMVar requestVar request
    Left e -> putStrLn e

handleResponse :: Socket -> MVar Response -> IO ()
handleResponse conn responseVar = do
  response <- takeMVar responseVar
  NBL.sendAll conn (serializeResponse response) -- Serialize and send the response (Haskell data type to CBOR)

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
