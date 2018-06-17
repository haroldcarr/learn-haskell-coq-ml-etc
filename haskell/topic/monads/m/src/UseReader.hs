{-# LANGUAGE FlexibleContexts #-}

module UseReader where

import           Control.Monad.Identity
import           MonadReader
import           Test.HUnit           as T (Test (TestList), runTestTT)
import           Test.HUnit.Util      as U

{-# ANN module "HLint: ignore Reduce duplication" #-}

------------------------------------------------------------------------------

r     :: Reader      [String]               [String]
rt    :: ReaderT     [String] Identity      [String]
rmr   :: MonadReader [String] m        => m [String]
r      = do a <- ask; b <- asks (["R"]  ++); l <- local (["L"]++) ask; return (a++b++l)
rt     = do a <- ask; b <- asks (["RT"] ++); l <- local (["L"]++) ask; return (a++b++l)
rmr    = do a <- ask; b <- asks (["RMR"]++); l <- local (["L"]++) ask; return (a++b++l)
dor    = runReader  r   ["initial"]
dort   = runReaderT rt  ["initial"]
dormr  = runReaderT rmr ["initial"]     :: Identity [String]
tdor   = U.t "tdor"                dor              ["initial","R"  ,"initial","L","initial"]
tdort  = U.t "tdort"               dort   (Identity ["initial","RT" ,"initial","L","initial"])
tdort' = U.t "tdort'" (runIdentity dort)            ["initial","RT" ,"initial","L","initial"]
tdormr = U.t "tdormr"              dormr  (Identity ["initial","RMR","initial","L","initial"])

------------------------------------------------------------------------------

testReader = runTestTT $ TestList $ tdor ++ tdort ++ tdort' ++ tdormr
tr = testReader
