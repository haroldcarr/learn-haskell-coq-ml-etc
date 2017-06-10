{-# LANGUAGE FlexibleContexts #-}

module State where

import           Control.Monad.State
import           Data.Map            as M
import           Test.HUnit          as T (Test (TestList), runTestTT)
import           Test.HUnit.Util     as U

------------------------------------------------------------------------------

s :: (String, Map String Int)
s = runState f M.empty

f :: State (Map String Int) String
f = do
  m <- get
  put (M.insert "H" 6 m)
  return "C"

ts = U.t "ts"
    s
    ("C",fromList [("H",6)])

------------------------------------------------------------------------------

testState =
  runTestTT $ TestList {- $ -} ts
