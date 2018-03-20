{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X00_Base where

import qualified Control.Concurrent                   as CC
import qualified Control.Concurrent.Async             as Async
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Builder              as BSB
import qualified Data.ByteString.Char8                as BSC8
import           Data.Monoid                          ((<>))
import qualified Data.Text.IO                         as TIO
import qualified Data.Text.Encoding                   as TE
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
import qualified System.Posix.Process                 as SPP
import qualified System.Random                        as Random
------------------------------------------------------------------------------
import           Config
import           Ledger

runServerAndClients
  :: Ledgerable a
  => Ledger a Config
  -> (Ledger a Config -> RIO Config ())
  -> IO ()
runServerAndClients ledger txServer = do
  lo <- logOptionsHandle stderr False
  let logOptions = setLogUseColor False (setLogUseTime False lo)
  withLogFunc logOptions $ \lf -> do
    let c = defaultConfig lf
    runRIO c (server ledger txServer) `Async.concurrently_` runRIO c clients

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

httpServer
  :: (Env env, Ledgerable a)
  => Ledger a env
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
          r <- contentsAsBS ledger
          -- runRIO env $ logInfo (displayShow (show contents))
          send $ Wai.responseBuilder HTTP.status200 [] r
        "/modify" -> do
          let q = Wai.queryString req
          case q of
            [(k,Just v)] -> do
              let k' = Prelude.read (BSC8.unpack k) :: Int -- TODO
                  v' = TE.decodeUtf8 v
              runRIO env $ logInfo (displayShow ("httpServer modify " <> show k' <> " " <> show v'))
              lModify ledger k' (fromText ledger v')
              runRIO env $ logInfo "httpServer after modify"
              r <- contentsAsBS ledger
              runRIO env $ logInfo "httpServer after contentsAsBS"
              send $ Wai.responseBuilder HTTP.status200 [] r
            _ -> do
              runRIO env $ logInfo (displayShow ("httpServer modify with bad query " <> show q))
              send $ Wai.responseBuilder HTTP.status400 [] ""
        "/quit" -> do
          runRIO env $ logInfo "httpServer received QUIT"
          SPP.exitImmediately (SE.ExitFailure 1)
          -- never happens -- just for type checking
          send $ Wai.responseBuilder HTTP.status500 [] ""
        x -> do
          runRIO env $ logInfo (displayShow ("httpServer received unknown " <> x))
          send $ Wai.responseBuilder HTTP.status400 [] ""
 where
  contentsAsBS l = do
    contents <- lContents l
    return $ BSB.byteString (BSC8.pack (show contents))

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
    f (fromText l line)
    runRIO e $ logInfo (displayShow ("txConnectionHandler got TX: " <> line))
    SIO.hPrint h line
    loop e

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
    runRIO c $ logInfo (displayShow ("client connecting to : " <> host <> " " <> show port))
    h <- N.connectTo host port
    runRIO c $ logInfo (displayShow ("client connected to : " <> host <> " " <> show port))
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
