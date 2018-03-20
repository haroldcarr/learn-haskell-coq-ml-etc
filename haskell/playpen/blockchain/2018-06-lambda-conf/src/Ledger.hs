{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Ledger where

import Data.Sequence as Seq
import RIO
------------------------------------------------------------------------------
import           Config

data Ledger a env = Ledger
  { lContents
      :: IO (Seq.Seq a)
  , lCommit
      :: (HasConfig env, HasLogFunc env)
      => env
      -> a
      -> IO ()
  }


