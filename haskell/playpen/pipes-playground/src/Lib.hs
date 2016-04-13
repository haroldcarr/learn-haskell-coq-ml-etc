{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Lib where

import qualified Data.ByteString       as B (ByteString)
import qualified Data.ByteString.Char8 as BC (putStrLn)
import qualified Pipes                 as P (Consumer', Effect, MonadIO,
                                             Producer', for, lift, runEffect,
                                             (>->))
import qualified Pipes.ByteString      as PB (fromHandle, stdin, stdout)
import qualified Pipes.HTTP            as PH (defaultManagerSettings, method,
                                              newManager, parseUrl, requestBody,
                                              requestHeaders, responseBody,
                                              stream, withHTTP)
import qualified Pipes.Network.TCP     as PN (SockAddr, Socket, closeSock,
                                              connectSock, fromSocket, toSocket)
import qualified Pipes.Prelude         as PP (take)
import qualified Pipes.Safe            as PS (SafeT, bracket, runSafeT)
import           Prelude               as PL (FilePath, IO, fst, putStrLn,
                                              return, ($), (++))
import qualified System.IO             as IO (IOMode (ReadMode), hClose,
                                              openFile, withFile)

connect :: P.MonadIO m => m (PN.Socket, PN.SockAddr)
connect = PN.connectSock "127.0.0.1" "2999"

close :: P.MonadIO m => PN.Socket -> m ()
close = PN.closeSock

mkP :: P.MonadIO m => PN.Socket -> P.Producer' B.ByteString m ()
mkP s = PN.fromSocket s 4096

mkC :: P.MonadIO m => PN.Socket -> P.Consumer' B.ByteString m r
mkC = PN.toSocket

doit :: IO ()
doit = do
    c <- Lib.connect
    let s = fst c
    sendIt s
    recIt s
    close s

sendIt :: P.MonadIO m => PN.Socket -> m ()
sendIt soc = do
    let c = mkC soc
    P.runEffect $ PB.stdin P.>-> PP.take 1 P.>-> c

recIt :: PN.Socket -> IO ()
recIt soc = do
    let p = mkP soc
    P.runEffect $ doRecv p

-- doRecv :: Proxy x' x () ByteString IO a' -> Proxy x' x c' c IO a'
doRecv :: P.Producer' B.ByteString IO () -> P.Effect IO ()
doRecv p = P.for p $ \b -> P.lift $ BC.putStrLn b

------------------------------------------------------------------------------

fi :: FilePath
fi = "TAGS"

dox :: FilePath -> IO ()
dox filename =
    IO.withFile filename IO.ReadMode $ \hIn -> do
        r <- PH.parseUrl "http://127.0.0.1:2999/validator/validate"
        let req = r { PH.method = "POST"
                    , PH.requestHeaders = [ ("Accept"      , "application/json")
                                          , ("Content-Type", "application/swagger+json; version=2.0")
                                          ]
                    , PH.requestBody = PH.stream (PB.fromHandle hIn)
                    }
        m <- PH.newManager PH.defaultManagerSettings
        PH.withHTTP req m $ \resp ->
            P.runEffect $ PH.responseBody resp P.>-> PB.stdout
        return ()

------------------------------------------------------------------------------

-- TODO : make a 'readFile' with this signature:
-- readFile :: FilePath -> P.Producer' B.ByteString IO ()
readFile :: FilePath -> P.Producer' B.ByteString (PS.SafeT IO) ()
readFile file = PS.bracket
    (do h <- IO.openFile file IO.ReadMode
        PL.putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        IO.hClose h
        PL.putStrLn $ "{" ++ file ++ " closed}" )
    PB.fromHandle

test :: IO ()
test = PS.runSafeT $ P.runEffect $ readFile "TAGS" P.>-> PP.take 4 P.>-> PB.stdout

{-
java -cp ~/.m2/repository/ws-commons/tcpmon/1.0/tcpmon-1.0.jar  org.apache.ws.commons.tcpmon.TCPMon 2999 127.0.0.1 3000 &

stream                   :: Pipes.Core.Producer ByteString IO () -> RequestBody
(PB.stdin >-> PP.take 1) :: Control.Monad.IO.Class.MonadIO m =>
                             Pipes.Internal.Proxy                a' a () ByteString m ()
type Pipes.Core.Producer b = Pipes.Internal.Proxy Pipes.Internal.X () () b

-}
