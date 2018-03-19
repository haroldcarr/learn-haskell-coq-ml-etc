{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X02_CASLedger where

import qualified Control.Concurrent.Async             as Async
import qualified Control.Concurrent                   as CC
import qualified Control.Exception.Safe               as S
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.Concurrent.Queue.MichaelScott   as Q
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Network                              as N
import           RIO
import qualified System.IO                            as SIO
------------------------------------------------------------------------------
import           Config
import           LedgerLockedImpl
import           X00_Base

runCASLedger :: IO ()
runCASLedger = do
  q <- Q.newQ
  runServerAndClients (poolAndMinerServer q)

poolAndMinerServer
  :: (HasLogFunc env, HasConfig env)
  => Q.LinkedQueue T.Text
  -> Ledger T.Text
  -> RIO env ()
poolAndMinerServer q l = do
  env <- ask
  liftIO (runRIO env (poolServer q) `Async.concurrently_` runRIO env (minerServer q l))

minerServer
  :: (HasLogFunc env, HasConfig env)
  => Q.LinkedQueue T.Text
  -> Ledger T.Text
  -> RIO env ()
minerServer q l = do
  env <- ask
  liftIO $ Async.replicateConcurrently_ (cNumMiners (getConfig env)) (miner env)
 where
  miner e = do
    ma <- Q.tryPopR q
    case ma of
      Nothing -> miner e
      Just a  -> do
        commitToLedger e l a
        runRIO e $ logInfo (displayShow ("miner committed TX: " <> a))
        miner e

poolServer
  :: (HasLogFunc env, HasConfig env)
  => Q.LinkedQueue T.Text
  -> RIO env ()
poolServer q = do
  env <- ask
  liftIO $ N.withSocketsDo $ do
    let txp = cTxPort (getConfig env)
    runRIO env $ logInfo (displayShow ("Listening for TXs on port " <> show txp))
    sock <- N.listenOn txp
    loop env sock
 where
   loop e s = liftIO $ do
     (h, hst, prt) <- N.accept s
     runRIO e $ logInfo (displayShow ("Accepted TX connection from " <> hst <> " " <> show prt))
     CC.forkFinally (liftIO (runRIO e (txConnectionHandler q h))) (const (SIO.hClose h))
     loop e s
     `S.onException` do
       runRIO e $ logInfo "Closing listen port"
       N.sClose s

txConnectionHandler
  :: (HasLogFunc env, HasConfig env)
  => Q.LinkedQueue T.Text
  -> SIO.Handle
  -> RIO env ()
txConnectionHandler q h = do
  env <- ask
  liftIO $ do
    SIO.hSetBuffering h SIO.LineBuffering
    loop env
 where
  loop e = do
    line <- T.hGetLine h
    Q.pushL q line
    runRIO e $ logInfo (displayShow ("txConnectionHandler got TX: " <> line))
    SIO.hPrint h line
    loop e
