module Main where
{-
repl  : server 8888
shell : telnet 127.0.0.1 8888

or

shell : sudo `stack exec which debug`
shell : telnet 127.0.0.1 79
shell : finger carr@localhost
-}

import           Control.Monad             (forever)
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import           System.Environment        (getArgs)

-- | Sets up server.
-- Arg is Socket that listens for new client connections.
-- forever causes listening socket to remain open indefinitely.
-- accept blocks until a client connects to the server.
-- `soc` is connection for communicating with the client.
-- receive up to 1024 bytes of text from clent connection
-- print received
-- echo back on client socket
-- close connection to client
-- Note: `recv` may return fewer than the maximum bytes specified if client sends less.
logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
    (soc, _) <- accept sock
    printAndKickback soc
    close soc
  where
    printAndKickback conn = do
        msg <- recv conn 1024
        print msg
        sendAll conn msg

server :: String -> IO ()
server portnum = withSocketsDo $ do
    addrinfos <- getAddrInfo
                     -- preferred socket type or protocol
                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                     -- host name to lookup
                     Nothing
                     -- service name to lookup
                     (Just portnum)
    let serveraddr = head addrinfos -- unsafe
    sock <- socket -- Family
                   (addrFamily serveraddr)
                   -- SocketType
                   Stream
                   -- ProtocolNumber
                   defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1 -- 1 is maximum number of queued connections
    logAndEcho sock
    close sock

main :: IO ()
main = do
    args <- getArgs
    let portnum = case args of [] -> "79"; (x:_) -> x
    server portnum
