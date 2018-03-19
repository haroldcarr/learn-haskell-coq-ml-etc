{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X01_LockedLedger where

import qualified Control.Concurrent                   as CC
import qualified Control.Concurrent.Async             as Async
import qualified Control.Exception.Safe               as S
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Builder              as BSB
import qualified Data.ByteString.Char8                as BSC
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Thyme                           as Time
import qualified Network                              as N
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           RIO
import qualified System.Exit                          as SE
import qualified System.IO                            as SIO
import qualified System.Posix.Process                 as SPP
import qualified System.Random                        as Random
------------------------------------------------------------------------------
import           Config
import           LedgerLockedImpl

runServerAndClients
  :: IO ()
runServerAndClients = do
  lo <- logOptionsHandle stderr False
  let logOptions = setLogUseColor False (setLogUseTime False lo)
  withLogFunc logOptions $ \lf -> do
    let c = defaultConfig lf
    runRIO c server `Async.concurrently_` runRIO c clients

server
  :: (HasLogFunc env, HasConfig env)
  => RIO env ()
server = do
  env <- ask
  liftIO $ N.withSocketsDo $ do
    let txp = cTxPort (getConfig env)
    sock <- N.listenOn txp
    runRIO env $ logInfo (displayShow ("Listening for TXs on port " <> show txp))
    ledger <- initLedger
    liftIO (runRIO env (httpServer ledger)
            `Async.concurrently_`
            runRIO env (txServer ledger sock))
      `S.onException` do
          runRIO env $ logInfo (displayShow ("Closing listen port " <> show txp))
          N.sClose sock

httpServer
  :: (HasLogFunc env, HasConfig env, Show a)
  => Ledger a
  -> RIO env ()
httpServer ledger = do
  env <- ask
  let httpPort = cHttpPort (getConfig env)
  logInfo (displayShow ("starting httpServer on port " <> show httpPort))
  liftIO $ Wai.run httpPort $ Wai.logStdoutDev $
    \req send -> do
      runRIO env $ logInfo (displayShow ("httpServer received request " <> show req))
      case Wai.rawPathInfo req of
        "/contents" -> do
          contents <- ledgerContents ledger
          let r = BSB.byteString (BSC.pack (show contents))
          -- runRIO env $ logInfo (displayShow (show contents))
          send $ Wai.responseBuilder HTTP.status200 [] r
        "/quit" -> do
          runRIO env $ logInfo "httpServer received QUIT"
          SPP.exitImmediately (SE.ExitFailure 1)
          -- never happens -- just for type checking
          send $ Wai.responseBuilder HTTP.status500 [] ""
        x -> do
          runRIO env $ logInfo (displayShow ("httpServer received unknown " <> x))
          send $ Wai.responseBuilder HTTP.status400 [] ""

txServer
  :: (HasLogFunc env, HasConfig env)
  => Ledger T.Text
  -> N.Socket
  -> RIO env CC.ThreadId
txServer ledger sock = do
  env <- ask
  liftIO $ do
    (h, hst, prt) <- N.accept sock
    runRIO env $ logInfo (displayShow ("Accepted TX connection from " <> hst <> " " <> show prt))
    CC.forkFinally (liftIO (runRIO env (serve ledger h))) (const (SIO.hClose h))
  txServer ledger sock

serve
  :: (HasLogFunc env, HasConfig env)
  => Ledger T.Text
  -> SIO.Handle
  -> RIO env ()
serve ledger h = do
  env <- ask
  liftIO $ do
    SIO.hSetBuffering h SIO.LineBuffering
    loop env
 where
  loop e = do
    line <- T.hGetLine h
    commitToLedger e ledger line
    runRIO e $ logInfo (displayShow ("serve got TX: " <> line))
    SIO.hPrint h line
    loop e

clients
  :: (HasLogFunc env, HasConfig env)
  => RIO env ()
clients = do
  cfg <- asks getConfig
  liftIO $ Async.replicateConcurrently_ (cNumClients cfg) (client cfg)
 where
  client c = do
    h <- N.connectTo (cHost c) (cTxPort c)
    SIO.hSetBuffering h SIO.LineBuffering
    loop h
   where
    loop h = do
      d <- Random.randomRIO (1,10)
      CC.threadDelay (d * 1000000)
      t <- Time.getCurrentTime
      runRIO c $ logInfo (displayShow ("client sending TX: " <> show t))
      SIO.hPrint h t
      r <- SIO.hGetLine h
      runRIO c $ logInfo (displayShow ("client received TX: " <> r))
      loop h
