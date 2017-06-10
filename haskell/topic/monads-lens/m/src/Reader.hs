{-# LANGUAGE FlexibleContexts #-}

module Reader where

import           Control.Monad.Reader
import           Test.HUnit           as T (Test (TestList), runTestTT)
import           Test.HUnit.Util      as U

------------------------------------------------------------------------------

w :: Reader [String] [String]
w = do
  a <- ask
  b <- asks (["XXX"]++)
  return (a++b)

dow = runReader w ["initial"]

tdow = U.t "tdow"
    dow
    ["initial","XXX","initial"]

------------------------------------------------------------------------------

testReader =
  runTestTT $ TestList {- $ -} tdow
