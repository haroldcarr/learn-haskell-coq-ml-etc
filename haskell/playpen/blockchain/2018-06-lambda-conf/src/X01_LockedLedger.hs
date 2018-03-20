{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module X01_LockedLedger where

import qualified Control.Concurrent                   as CC
import qualified Control.Exception.Safe               as S
import           Control.Monad.IO.Class               (liftIO)
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Network                              as N
import           RIO
import qualified System.IO                            as SIO
------------------------------------------------------------------------------
import           Config
import           Ledger
import           X00_Base

runDirectLedger :: IO ()
runDirectLedger = runServerAndClients txServer

txServer
  :: (HasLogFunc env, HasConfig env)
  => Ledger T.Text
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
     CC.forkFinally (liftIO (runRIO e (txConnectionHandler ledger h))) (const (SIO.hClose h))
     loop e s
     `S.onException` do
       runRIO e $ logInfo "Closing listen port"
       N.sClose s

txConnectionHandler
  :: (HasLogFunc env, HasConfig env)
  => Ledger T.Text
  -> SIO.Handle
  -> RIO env ()
txConnectionHandler ledger h = do
  env <- ask
  liftIO $ do
    SIO.hSetBuffering h SIO.LineBuffering
    loop env
 where
  loop e = do
    line <- T.hGetLine h
    commitToLedger e ledger line
    runRIO e $ logInfo (displayShow ("txConnectionHandler COMMITED TX: " <> line))
    SIO.hPrint h line
    loop e
