{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module X01_LockedLedger where

import qualified Control.Concurrent     as CC
import qualified Control.Exception.Safe as S
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            ((<>))
import qualified Network                as N
import           RIO
import qualified System.IO              as SIO
------------------------------------------------------------------------------
import           Config
import           Ledger
import           X00_Base

runDirectLedger :: IO ()
runDirectLedger = do
  l <- createLedgerLocked id
  runServerAndClients l txServer

txServer
  :: (Env env, Ledgerable a)
  => Ledger a env
  -> RIO env ()
txServer ledger = do
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
     let f = lCommit ledger e
     CC.forkFinally (liftIO (runRIO e (txConnectionHandler h ledger f))) (const (SIO.hClose h))
     loop e s
     `S.onException` do
       runRIO e $ logInfo "Closing listen port"
       N.sClose s

