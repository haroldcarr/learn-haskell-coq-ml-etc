{-# LANGUAGE FlexibleContexts #-}

module Reader where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Test.HUnit           as T (Test (TestList), runTestTT)
import           Test.HUnit.Util      as U

{-# ANN module "HLint: ignore Reduce duplication" #-}

------------------------------------------------------------------------------

w     :: Reader      [String]               [String]
wt    :: ReaderT     [String] Identity      [String]
wmr   :: MonadReader [String] m        => m [String]
w      = do a <- ask; b <- asks (["W"]  ++); return (a++b)
wt     = do a <- ask; b <- asks (["WT"] ++); return (a++b)
wmr    = do a <- ask; b <- asks (["WMR"]++); return (a++b)
dow    = runReader  w   ["initial"]
dowt   = runReaderT wt  ["initial"]
dowmr  = runReaderT wmr ["initial"]     :: Identity  [String]
tdow   = U.t "tdow"                dow               ["initial","W","initial"]
tdowt  = U.t "tdowt"               dowt   (Identity  ["initial","WT","initial"])
tdowt' = U.t "tdowt'" (runIdentity dowt)             ["initial","WT","initial"]
tdowmr = U.t "tdowmr"              dowmr   (Identity ["initial","WMR","initial"])

------------------------------------------------------------------------------

testReader = runTestTT $ TestList $ tdow ++ tdowt ++ tdowt' ++ tdowmr
tr = testReader
