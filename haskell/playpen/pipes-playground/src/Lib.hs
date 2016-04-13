{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.ByteString        as B
import           Data.ByteString.Char8  as BC (putStrLn)
import           Pipes
import qualified Pipes.ByteString       as PB
import           Pipes.HTTP
import           Pipes.Network.TCP
import           Pipes.Prelude          as PP
import           Pipes.Safe
import           Prelude                as P hiding (readFile)
import qualified System.IO              as IO

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

dox :: IO ()
dox = do
    r <- parseUrl "http://127.0.0.1:3000/validator/validate"
    let req = r { method = "POST"
                , requestHeaders = [ ("Accept"      , "application/json")
                                   , ("Content-Type", "application/swagger+json; version=2.0")
                                   ]
                , requestBody = stream (PB.stdin >-> PP.take 1)
                }
    m <- newManager defaultManagerSettings
    withHTTP req m $ \resp ->
        runEffect $ responseBody resp >-> PB.stdout

readFile :: FilePath -> Producer' ByteString (SafeT IO) ()
readFile file = bracket
    (do h <- IO.openFile file IO.ReadMode
        P.putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        IO.hClose h
        P.putStrLn $ "{" ++ file ++ " closed}" )
    PB.fromHandle

test = runSafeT $ runEffect $ Lib.readFile "TAGS" >-> PP.take 4 >-> PB.stdout
