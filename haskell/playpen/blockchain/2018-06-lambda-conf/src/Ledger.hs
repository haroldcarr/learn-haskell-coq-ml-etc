{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Ledger where

import qualified Control.Concurrent      as CC
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad           as CM
import qualified Data.Atomics            as A
import qualified Data.IORef              as IOR
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import           RIO
import qualified System.Random           as Random
------------------------------------------------------------------------------
import           Config

type Ledgerable a = (Show a, Monoid a)

data Ledger a env = Ledger
  { lContents
      :: IO (Seq.Seq a)
  , lCommit
      :: env
      -> a
      -> IO ()
  , lModify
      :: Int
      -> a
      -> IO ()
  , fromText
      :: T.Text
      -> a
  }

createLedgerCAS
  :: (Env env, Ledgerable a)
  => (T.Text -> a)
  -> IO (Ledger a env)
createLedgerCAS ft = do
  r <- IOR.newIORef Seq.empty
  return Ledger
    { lContents = IOR.readIORef r
    , lCommit = \_ a -> A.atomicModifyIORefCAS_ r $ \existing -> existing Seq.|> a
    , lModify = \i a -> A.atomicModifyIORefCAS_ r $ \existing -> Seq.update i a existing
    , fromText = ft
    }

createLedgerLocked
  :: (Env env, Ledgerable a)
  => (T.Text -> a)
  -> IO (Ledger a env)
createLedgerLocked ft = do
  mv <- MV.newMVar Seq.empty
  return Ledger
    { lContents = MV.readMVar mv
    , lCommit = \e a -> do
        s <- MV.takeMVar mv
        CM.when (cDOSEnabled (getConfig e)) $ do
          d <- Random.randomRIO (cDOSRandomRange (getConfig e))
          CM.when (d == cDOSRandomHit (getConfig e)) $ do
            runRIO e $ logInfo "BEGIN commitToLedger DOS"
            CC.threadDelay (1000000 * cDOSDelay (getConfig e))
            runRIO e $ logInfo "END commitToLedger DOS"
        MV.putMVar mv (s Seq.|> a)
    , lModify = \i a -> do
        s <- MV.takeMVar mv
        MV.putMVar mv (Seq.update i a s)
    , fromText = ft
    }
