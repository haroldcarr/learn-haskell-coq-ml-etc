{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Control.Concurrent                   as CC
import qualified Control.Concurrent.Async             as Async
import qualified Control.Monad.Logger                 as L
import qualified Data.ByteString.Builder              as BSB
import qualified Data.ByteString.Char8                as BSC
import qualified Data.IORef                           as IOR
import           Data.Monoid                          ((<>))
import qualified Data.Sequence                        as Seq
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Thyme                           as Time
import qualified Network                              as N
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified System.IO                            as SIO
import qualified System.Random                        as Random

host :: N.HostName
host = "localhost"

txPort :: N.PortID
txPort = N.PortNumber 44444

httpPort :: Wai.Port
httpPort = 3000

numClients :: Int
numClients = 8

lInfo :: T.Text -> IO ()
lInfo = L.runStdoutLoggingT . L.logInfoN

lInfoP :: String -> IO ()
lInfoP = lInfo . T.pack

runServerAndClients :: IO ()
runServerAndClients = server `Async.concurrently_` clients

server :: IO ()
server = N.withSocketsDo $ do
  sock <- N.listenOn txPort
  lInfoP ("Listening for TXs on port " <> show txPort)
  ior <- IOR.newIORef Seq.empty
  httpServer ior `Async.concurrently_` txServer ior sock

httpServer :: Show a => IOR.IORef a -> IO ()
httpServer ior = Wai.run httpPort $ Wai.logStdoutDev $
  \_req send -> do
    ledger <- IOR.readIORef ior
    send $ Wai.responseBuilder HTTP.status200 [] (BSB.byteString (BSC.pack (show ledger)))

txServer :: IOR.IORef (Seq.Seq T.Text) -> N.Socket -> IO CC.ThreadId
txServer ior sock = do
  (handle, h, p) <- N.accept sock
  lInfoP ("Accepted TX connection from " <> h <> " " <> show p)
  _ <- CC.forkFinally (serve ior handle) (const (SIO.hClose handle))
  txServer ior sock

serve :: IOR.IORef (Seq.Seq T.Text) -> SIO.Handle -> IO ()
serve ior h = do
  SIO.hSetBuffering h SIO.LineBuffering
  loop
 where
  loop = do
    line <- T.hGetLine h
    _ <- IOR.atomicModifyIORef' ior (\old -> (old Seq.|> line, old))
    lInfo $ "serve got TX: " <> line
    SIO.hPrint h line
    loop

clients :: IO ()
clients =
  Async.replicateConcurrently_  numClients client
 where
  client = do
    h <- N.connectTo host txPort
    SIO.hSetBuffering h SIO.LineBuffering
    loop h
   where
    loop h = do
      d <- Random.randomRIO (1,10)
      CC.threadDelay (d * 1000000)
      t <- Time.getCurrentTime
      lInfoP ("client sending TX: " <> show t)
      SIO.hPrint h t
      r <- SIO.hGetLine h
      lInfoP ("client received TX: " <> r)
      loop h
