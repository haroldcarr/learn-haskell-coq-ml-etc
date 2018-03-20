{-# OPTIONS_GHC -Wno-unused-do-bind    #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module LedgerCASImpl where

import qualified Data.Atomics  as A
import qualified Data.IORef    as IOR
import qualified Data.Sequence as Seq
import           RIO
------------------------------------------------------------------------------
import           Ledger

createLedger
  :: Show a
  => IO (Ledger a env)
createLedger = do
  r <- IOR.newIORef Seq.empty
  return Ledger
    { lContents = IOR.readIORef r
    , lCommit = \_ a -> A.atomicModifyIORefCAS_ r $ \existing -> existing Seq.|> a
    , lModify = \i a -> A.atomicModifyIORefCAS_ r $ \existing -> Seq.update i a existing
    }
