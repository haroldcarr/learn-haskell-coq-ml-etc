{-# OPTIONS_GHC -Wno-unused-do-bind    #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module LedgerCASImpl where

import qualified Data.Atomics  as A
import qualified Data.IORef    as IOR
import qualified Data.Sequence as Seq
import qualified Data.Text     as T
import           RIO
------------------------------------------------------------------------------
import           Config
import           Ledger

createLedger
  :: (Env env, Ledgerable a)
  => (T.Text -> a)
  -> IO (Ledger a env)
createLedger ft = do
  r <- IOR.newIORef Seq.empty
  return Ledger
    { lContents = IOR.readIORef r
    , lCommit = \_ a -> A.atomicModifyIORefCAS_ r $ \existing -> existing Seq.|> a
    , lModify = \i a -> A.atomicModifyIORefCAS_ r $ \existing -> Seq.update i a existing
    , fromText = ft
    }
