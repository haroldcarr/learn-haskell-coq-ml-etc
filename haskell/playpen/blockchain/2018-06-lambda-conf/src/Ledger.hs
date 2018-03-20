{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Ledger where

import Data.Sequence as Seq
import RIO
------------------------------------------------------------------------------
import           Config

data Ledger a env = Ledger
  { lContents
      :: Show a
      => IO (Seq.Seq a)
  , lCommit
      :: (HasConfig env, HasLogFunc env, Show a)
      => env
      -> a
      -> IO ()
  , lModify
      :: Show a
      => Int
      -> a
      -> IO ()
  }


