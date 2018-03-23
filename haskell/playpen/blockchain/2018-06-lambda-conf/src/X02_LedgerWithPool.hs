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
  l <- createLedgerCAS (return Nothing) id
  q <- Q.newQ
  let e = defaultConfig
      txHandler tx = do
        Q.pushL q tx
        Log.infoM lMINER ("POOLED: " <> show tx)
      committer = lCommit l e
  Async.replicateConcurrently_ (cNumMiners (getConfig e)) (miner q committer)
   `Async.concurrently_`
   runServerAndClients e l txHandler

miner
  :: Ledgerable a
  => Q.LinkedQueue a
  -> (a -> IO ())
  -> IO ()
miner q committer = do
  CC.threadDelay 10000
  ma <- Q.tryPopR q
  case ma of
    Nothing ->
      miner q committer
    Just a  -> do
      committer a
      Log.infoM lMINER ("miner COMMITTED TX: " <> show a)
      miner q committer
