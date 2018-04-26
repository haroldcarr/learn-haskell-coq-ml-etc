{-# LANGUAGE FlexibleContexts #-}

module State where

import           Control.Monad.State
import           Data.Map            as M
import           Test.HUnit          as T (Test (TestList), runTestTT)
import           Test.HUnit.Util     as U

------------------------------------------------------------------------------

s :: State (Map String Int) String
s = do
  m <- get
  put (M.insert "H" 6 m)
  return "C"

rs :: (String, Map String Int)
rs = runState s M.empty

trs = U.t "trs"
    rs
    ("C",fromList [("H",6)])

------------------------------------------------------------------------------

testState = runTestTT $ TestList {- $ -} trs
ts = testState
