module ClbSocket.Server (runServer, ServerConfig (..)) where

import ClbSocket.Parse (parseRequest)
import ClbSocket.Serialise (Response, serializeResponse)
import ClbSocket.Types (Request)
import Control.Concurrent (MVar, putMVar, takeMVar)
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
  { socketPath :: FilePath
  , requestVar :: MVar Request
  , responseVar :: MVar Response
  }

-- TODO: Concurrency, Error Handling, Logging
runServer :: ServerConfig -> IO ()
runServer (ServerConfig {..}) = do
  -- Create a UNIX domain socket
  sock <- socket AF_UNIX Stream defaultProtocol

  -- Bind the socket to a file path
  let sockAddr = SockAddrUnix socketPath
  if isSupportedSockAddr sockAddr
    then bind sock sockAddr
    else error "Unsupported Socket Address!"

  -- Start listening for connections
  listen sock 5

  -- Accept connections and handle them
  forever $ do
    (conn, _) <- accept sock
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

-- Function to receive a full message, handling cases where the message exceeds the buffer size
recvFull :: Socket -> IO BSL.ByteString
recvFull conn = loop BSL.empty
  where
    loop acc = do
      chunk <- NBL.recv conn 16_384 -- Buffer set to 16,384 bytes to match Cardano's max transaction size, optimizing performance at the expense of memory.
      if BSL.null chunk
        then return acc -- No more data to read
        else loop (acc <> chunk)
