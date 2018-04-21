{-# LANGUAGE FlexibleContexts #-}

module Reader where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Test.HUnit           as T (Test (TestList), runTestTT)
import           Test.HUnit.Util      as U

{-# ANN module "HLint: ignore Reduce duplication" #-}

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

wt :: ReaderT [String] Identity [String]
wt = do
  a <- ask
  b <- asks (["XXX"]++)
  return (a++b)

dowt  = runReaderT w ["initial"]

tdowt = U.t "tdowt"
    dowt
    (Identity ["initial","XXX","initial"])

dowt' = runIdentity dowt

tdowt' = U.t "tdowt'"
    dowt'
    ["initial","XXX","initial"]

------------------------------------------------------------------------------

testReader =
  runTestTT $ TestList $ tdow ++ tdowt ++ tdowt'
