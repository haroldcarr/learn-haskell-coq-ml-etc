{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.ByteString        as B
import           Data.ByteString.Char8  as BC (putStrLn)
import           Data.ByteString.Lazy   as BL
import           Pipes
import           Pipes.Network.TCP
import           Pipes.Prelude          (stdoutLn)
import           Prelude                as P

someFunc :: IO ()
someFunc = P.putStrLn "someFunc"

connect :: MonadIO m => m (Socket, SockAddr)
connect = connectSock "127.0.0.1" "3000"

close :: MonadIO m => Socket -> m ()
close = closeSock

mkP :: MonadIO m => Socket -> Producer' B.ByteString m ()
mkP s = fromSocket s 4096

doRecv p = for p $ \b -> lift $ BC.putStrLn b

doit = do
    c <- Lib.connect
    let s = fst c
        p = mkP s
    send s "foo\n"
    runEffect $ doRecv p
    close s
    return ()

{-
c <- connect
let s = fst c
let p = mkP s
doRecv p
close s
-}
