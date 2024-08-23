module ClbSocket.Server (runServer) where

import ClbSocket.Parse (Request, parseData)
import ClbSocket.Serialise (Response, serializeData)
import Control.Monad (forever)
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
import qualified Network.Socket.ByteString.Lazy as NBL

-- TODO: Concurrency, Error Handling, Logging
runServer :: FilePath -> IO ()
runServer socketPath = do
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
    handleConnection conn
    close conn

handleConnection :: Socket -> IO ()
handleConnection conn = do
  -- Receive data from the client
  msg <- NBL.recv conn 1024 -- Adjust buffer size as needed
  case parseData msg of -- Parse the received data (JSON to Haskell data type)
    Just parsedData -> do
      let response = processRequest parsedData -- Process the request
      NBL.sendAll conn (serializeData response) -- Serialize and send the response (Haskell data type to JSON)
    Nothing -> putStrLn "Failed to parse data"
  where
    processRequest :: Request -> Response Int
    processRequest = undefined
