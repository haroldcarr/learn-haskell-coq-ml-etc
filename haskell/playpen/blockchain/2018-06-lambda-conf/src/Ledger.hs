{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger where

import           Data.Sequence as Seq
import           Data.Text     as T
import           RIO

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


