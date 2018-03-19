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
import qualified Control.Monad                        as CM
import qualified Data.IORef                           as IOR
import qualified Data.Sequence                        as Seq
import           RIO
import qualified System.IO.Unsafe                     as SIOU
------------------------------------------------------------------------------
import           Config

data Ledger a env = Ledger
  { _ledgerRef      :: IOR.IORef (Seq.Seq a)
  , _ledgerContents :: IOR.IORef (Seq.Seq a) -> IO (Seq.Seq a)
  , _commitToLedger :: (HasLogFunc env, HasConfig env)
                    => env
                    -> Int
                    -> Ledger a env
                    -> a
                    -> IO (Ledger a env)
  }

initLedger
  :: IO (Ledger a env)
initLedger = do
  r <- IOR.newIORef Seq.empty
  return Ledger
    { _ledgerRef      = r
    , _ledgerContents = IOR.readIORef
    , _commitToLedger = \e d l@(Ledger i _ _) a -> IOR.atomicModifyIORef' i $ \existing
       ->
         SIOU.unsafePerformIO $ do
           CM.when (cDOSEnabled (getConfig e) && d == 10) $ do
             runRIO e $ logInfo "BEGIN commitToLedger DOS"
             CC.threadDelay (1000000 * 60)
             runRIO e $ logInfo "END commitToLedger DOS"
           return (existing Seq.|> a, l)
    }

ledgerContents
  :: Ledger a env
  -> IO (Seq.Seq a)
ledgerContents (Ledger i lc _) = lc i

commitToLedger
  :: (HasConfig env, HasLogFunc env)
  => env
  -> Int
  -> Ledger a env
  -> a
  -> IO (Ledger a env)
commitToLedger e d l@(Ledger _ _ cl) = cl e d l
