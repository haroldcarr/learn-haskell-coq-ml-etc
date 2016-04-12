{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.ByteString        as B
import           Data.ByteString.Char8  as BC (putStrLn)
import           Pipes
import qualified Pipes.ByteString       as PB
import           Pipes.Network.TCP
import           Pipes.Prelude          as PP (take)

connect :: MonadIO m => m (Socket, SockAddr)
connect = connectSock "127.0.0.1" "3000"

close :: MonadIO m => Socket -> m ()
close = closeSock

mkP :: MonadIO m => Socket -> Producer' B.ByteString m ()
mkP s = fromSocket s 4096

mkC :: MonadIO m => Socket -> Consumer' B.ByteString m r
mkC = toSocket

doit :: IO ()
doit = do
    c <- Lib.connect
    let s = fst c
    sendIt s
    recIt s
    close s

sendIt :: MonadIO m => Socket -> m ()
sendIt soc = do
    let c = mkC soc
    runEffect $ PB.stdin >-> PP.take 1 >-> c

recIt :: Socket -> IO ()
recIt soc = do
    let p = mkP soc
    runEffect $ doRecv p

-- doRecv :: Proxy x' x () ByteString IO a' -> Proxy x' x c' c IO a'
doRecv :: Producer' B.ByteString IO () -> Effect IO ()
doRecv p = for p $ \b -> lift $ BC.putStrLn b

{-
c <- connect
let s = fst c
let p = mkP s
doRecv p
close s
-}
