{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X00_Base where

import qualified Control.Concurrent                   as CC
import qualified Control.Concurrent.Async             as Async
import qualified Control.Exception.Safe               as S
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Builder              as BSB
import qualified Data.ByteString.Char8                as BSC8
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.IO                         as TIO
import qualified Data.Thyme                           as Time
import qualified Network                              as N
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Prelude
import           RIO
import qualified System.Exit                          as SE
import qualified System.IO                            as SIO
import qualified System.Log.Logger                    as Log
import qualified System.Posix.Process                 as SPP
import qualified System.Random                        as Random
------------------------------------------------------------------------------
import           Config
import           Ledger
import           Logging

runServerAndClients
  :: Ledgerable a
  => Config
  -> Ledger a Config
  -> (a -> IO ())
  -> IO ()
runServerAndClients env ledger txHandler = do
  setLogLevels
  liftIO (runRIO env (server ledger (txConnectionAcceptor txHandler))
          `Async.concurrently_`
          runRIO env clients)

server
  :: (Env env, Ledgerable a)
  => Ledger a env
  -> (Ledger a env -> RIO env ())
  -> RIO env ()
server ledger txServer = do
  env <- ask
  liftIO (runRIO env (httpServer ledger)
          `Async.concurrently_`
          runRIO env (txServer ledger))

txConnectionAcceptor
  :: (Env env, Ledgerable a)
  => (a -> IO ())
  -> Ledger a env
  -> RIO env ()
txConnectionAcceptor txHandler ledger = do
  env <- ask
  liftIO $ N.withSocketsDo $ do
    let txp = cTxPort (getConfig env)
    Log.infoM lBASE ("Listening for TXs on port " <> show txp)
    sock <- N.listenOn txp
    loop env sock
 where
   loop e s = liftIO $ do
     (h, hst, prt) <- N.accept s
     Log.infoM lBASE ("Accepted TX connection from " <> hst <> " " <> show prt)
     CC.forkFinally (liftIO (runRIO e (txConnectionHandler h ledger txHandler)))
                    (const (SIO.hClose h))
     loop e s
     `S.onException` do
       Log.infoM lBASE "Closing listen port"
       N.sClose s

txConnectionHandler
  :: (Env env, Ledgerable a)
  => SIO.Handle
  -> Ledger a env
  -> (a -> IO ())
  -> RIO env ()
txConnectionHandler h l f = do
  env <- ask
  liftIO $ do
    SIO.hSetBuffering h SIO.LineBuffering
    loop env
 where
  loop e = do
    line <- TIO.hGetLine h
    Log.infoM lBASE ("txConnectionHandler got TX: " <> T.unpack line)
    f (fromText l line)
    SIO.hPrint h line
    loop e

httpServer
  :: (Env env, Ledgerable a)
  => Ledger a env
  -> RIO env ()
httpServer ledger = do
  env <- ask
  let httpPort = cHttpPort (getConfig env)
  liftIO $ Log.infoM lBASE ("starting httpServer on port " <> show httpPort)
  liftIO $ Wai.run httpPort $ Wai.logStdoutDev $
    \req s -> do
      Log.infoM lBASE ("httpServer received request " <> show req)
      case Wai.rawPathInfo req of
        "/contents" -> do
          r <- contentsAsBS ledger
          -- runRIO env $ logInfo (displayShow (show contents))
          send s HTTP.status200 r
        "/modify" -> do
          let q = Wai.queryString req
          case q of
            [(k,Just v)] -> do
              let k' = Prelude.read (BSC8.unpack k) :: Int -- TODO
                  v' = TE.decodeUtf8 v
              Log.infoM lBASE ("httpServer modify " <> show k' <> " " <> show v')
              lModify ledger k' (fromText ledger v')
              r <- contentsAsBS ledger
              send s HTTP.status200 r
            _ -> do
              Log.infoM lBASE ("httpServer modify with bad query " <> show q)
              send s HTTP.status400 ""
        "/quit" -> do
          Log.infoM lBASE "httpServer received QUIT"
          SPP.exitImmediately (SE.ExitFailure 1)
          -- never happens -- just for type checking
          send s HTTP.status500 ""
        x -> do
          Log.infoM lBASE ("httpServer received unknown " <> BSC8.unpack x)
          send s HTTP.status400 ""
 where
  send s sc r = s $ Wai.responseBuilder sc [] r
  contentsAsBS l = do
    contents <- lContents l
    return $ BSB.byteString (BSC8.pack (show contents))

------------------------------------------------------------------------------

clients
  :: Env env
  => RIO env ()
clients = do
  cfg <- asks getConfig
  liftIO $ Async.replicateConcurrently_ (cNumClients cfg) (client cfg)
 where
  client c = do
    let host = cHost c
        port = cTxPort c
    Log.infoM lBASE ("client connecting to : " <> host <> " " <> show port)
    h <- N.connectTo host port
    Log.infoM lBASE ("client connected to : " <> host <> " " <> show port)
    SIO.hSetBuffering h SIO.LineBuffering
    loop h
   where
    loop h = do
      d <- Random.randomRIO (1,10)
      CC.threadDelay (d * 1000000)
      t <- Time.getCurrentTime
      Log.infoM lBASE ("client sending TX: " <> show t)
      SIO.hPrint h t
      r <- SIO.hGetLine h
      Log.infoM lBASE ("client received TX: " <> r)
      loop h
