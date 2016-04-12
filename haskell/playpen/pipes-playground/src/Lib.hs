{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.ByteString        as B
import           Data.ByteString.Char8  as BC (putStrLn)
import           Data.String            (IsString)
import           Pipes
import           Pipes.Network.TCP

connect :: MonadIO m => m (Socket, SockAddr)
connect = connectSock "127.0.0.1" "3000"

close :: MonadIO m => Socket -> m ()
close = closeSock

mkP :: MonadIO m => Socket -> Producer' B.ByteString m ()
mkP s = fromSocket s 4096

mkC :: MonadIO m => Socket -> Consumer' B.ByteString m r
mkC = toSocket

doRecv p = for p $ \b -> lift $ BC.putStrLn b

doit = do
    c <- Lib.connect
    let s = fst c
--     sendIt s
    recIt s
    close s

sendIt soc = do
    let c = mkC soc
    runEffect $ "foo\n" >~ c

recIt soc = do
    let p = mkP soc
    runEffect $ doRecv p

{-
c <- connect
let s = fst c
let p = mkP s
doRecv p
close s
-}
