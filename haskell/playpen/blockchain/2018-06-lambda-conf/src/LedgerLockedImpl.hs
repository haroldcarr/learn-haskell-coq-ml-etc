{-# OPTIONS_GHC -Wno-unused-do-bind    #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module LedgerLockedImpl
  ( Ledger
  , initLedger
  , ledgerContents
  , commitToLedger
  )
  where

import qualified Control.Concurrent                   as CC
import qualified Control.Concurrent.MVar              as MV
import qualified Control.Monad                        as CM
import qualified Data.Sequence                        as Seq
import           RIO
import qualified System.Random                        as Random
------------------------------------------------------------------------------
import           Config

newtype Ledger a = Ledger { _ledgerRef :: MV.MVar (Seq.Seq a) }

initLedger
  :: IO (Ledger a)
initLedger = do
  r <- MV.newMVar Seq.empty
  return $ Ledger r

ledgerContents
  :: Ledger a
  -> IO (Seq.Seq a)
ledgerContents = MV.readMVar . _ledgerRef

commitToLedger
  :: (HasConfig env, HasLogFunc env)
  => env
  -> Ledger a
  -> a
  -> IO ()
commitToLedger e (Ledger m) a = do
  s <- MV.takeMVar m
  CM.when (cDOSEnabled (getConfig e)) $ do
    d <- Random.randomRIO (1,10::Int)
    CM.when (d == 10) $ do
      runRIO e $ logInfo "BEGIN commitToLedger DOS"
      CC.threadDelay (1000000 * cDOSDelay (getConfig e))
      runRIO e $ logInfo "END commitToLedger DOS"
  MV.putMVar m (s Seq.|> a)
