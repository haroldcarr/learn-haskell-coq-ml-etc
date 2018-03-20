{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module X02_CASLedger where

import qualified Control.Concurrent                 as CC
import qualified Control.Concurrent.Async           as Async
import qualified Control.Exception.Safe             as S
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Concurrent.Queue.MichaelScott as Q
import           Data.Monoid                        ((<>))
import qualified Network                            as N
import           RIO
import qualified System.IO                          as SIO
------------------------------------------------------------------------------
import           Config
import           Ledger
import           X00_Base

runPoolLedger :: IO ()
runPoolLedger = do
  l <- createLedgerCAS id
  q <- Q.newQ
  runServerAndClients l (poolAndMinerServer q)

poolAndMinerServer
  :: (Env env, Ledgerable a)
  => Q.LinkedQueue a
  -> Ledger a env
  -> RIO env ()
poolAndMinerServer q l = do
  env <- ask
  liftIO (runRIO env (poolServer q l) `Async.concurrently_` runRIO env (minerServer q l))

minerServer
  :: (Env env, Ledgerable a)
  => Q.LinkedQueue a
  -> Ledger a env
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
        lCommit l e a
        runRIO e $ logInfo (displayShow ("miner COMMITTED TX: " <> show a))
        miner e

poolServer
  :: (Env env, Ledgerable a)
  => Q.LinkedQueue a
  -> Ledger a env
  -> RIO env ()
poolServer q l = do
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
     let f = Q.pushL q -- NOTE : specific
     CC.forkFinally (liftIO (runRIO e (txConnectionHandler h l f))) (const (SIO.hClose h))
     loop e s
     `S.onException` do
       runRIO e $ logInfo "Closing listen port"
       N.sClose s

