{-# OPTIONS_GHC -Wno-unused-do-bind    #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE RankNTypes                #-}

module LedgerCASImpl
  ( Ledger
  , initLedger
  , ledgerContents
  , commitToLedger
  )
  where

import qualified Data.Atomics                         as A
import qualified Data.IORef                           as IOR
import qualified Data.Sequence                        as Seq
import           RIO
------------------------------------------------------------------------------
import           Config

newtype Ledger a = Ledger { _ledgerRef :: IOR.IORef (Seq.Seq a) }

initLedger
  :: IO (Ledger a)
initLedger = do
  r <- IOR.newIORef Seq.empty
  return $ Ledger r

ledgerContents
  :: Ledger a
  -> IO (Seq.Seq a)
ledgerContents = IOR.readIORef . _ledgerRef

commitToLedger
  :: (HasConfig env, HasLogFunc env)
  => env
  -> Ledger a
  -> a
  -> IO ()
commitToLedger _ (Ledger i) a =
  A.atomicModifyIORefCAS_ i $ \existing ->
    existing Seq.|> a
