{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module X02_LedgerWithPool where

import qualified Control.Concurrent                 as CC
import qualified Control.Concurrent.Async           as Async
import qualified Data.Concurrent.Queue.MichaelScott as Q
import           Data.Monoid                        ((<>))
import           RIO
import qualified System.Log.Logger                  as Log
------------------------------------------------------------------------------
import           Config
import           Ledger
import           Logging
import           X00_Base

runLedgerWithPool :: IO ()
runLedgerWithPool = do
  l <- createLedgerCAS id
  q <- Q.newQ
  let e = defaultConfig
      txHandler tx = do
        Log.infoM lMINER ("POOLING: " <> show tx)
        Q.pushL q tx
  Async.async $
    Async.replicateConcurrently_ (cNumMiners (getConfig e)) (miner e q l)
  runServerAndClients e l txHandler

miner
  :: (Env env, Ledgerable a)
  => env
  -> Q.LinkedQueue a
  -> Ledger a env
  -> IO ()
miner env q l = do
  CC.threadDelay 10000
  ma <- Q.tryPopR q
  case ma of
    Nothing ->
      miner env q l
    Just a  -> do
      lCommit l env a
      Log.infoM lMINER ("miner COMMITTED TX: " <> show a)
      miner env q l
