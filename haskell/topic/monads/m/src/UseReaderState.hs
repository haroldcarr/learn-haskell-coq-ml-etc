{-# LANGUAGE FlexibleContexts #-}

module UseReaderState where

import           Test.HUnit           as T (Test (TestList), runTestTT)
import           Test.HUnit.Util      as U

import MonadReader
import MonadState

foo :: (MonadReader [String] m, MonadState [String] m)
    => m Int
foo = do
  a <- asks head
  let a' = read a
  let aa = a' + a'
  put [show aa]
  return a'

runFooSR = runState (runReaderT foo ["1"]) [   ]
runFooRS = runReader (runStateT foo [   ]) ["1"]

------------------------------------------------------------------------------

trf = U.tt "runFoo*" [ runFooSR, runFooRS ] (1, ["2"])

testReaderState = runTestTT $ TestList {- $ -} trf
trs = testReaderState
