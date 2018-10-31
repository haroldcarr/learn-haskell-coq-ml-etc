module Echo where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Network.Socket
import System.Environment
import System.IO

echo :: IO ()
echo = withSocketsDo $ do
  -- port <- toEnum . read . head <$> getArgs
  let port = 3000
  newSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption newSocket ReuseAddr 1
  bindSocket newSocket $ SockAddrInet port iNADDR_ANY
  listen newSocket 2
  runServer id newSocket

runServer :: (String -> String) -> Socket -> IO()
runServer f s = forever $ do
  (usableSocket,_) <- accept s
  forkIO $ interactWithSocket f usableSocket

interactWithSocket :: (String -> String) -> Socket -> IO()
interactWithSocket f s = do
  handle <- socketToHandle s ReadWriteMode
  forever $ f <$> hGetLine handle >>= hPutStrLn handle 
