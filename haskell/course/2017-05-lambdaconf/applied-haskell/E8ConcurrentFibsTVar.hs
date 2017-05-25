#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

module E8ConcurrentFibsTVar where

import           Control.Concurrent.Async
import           Control.Concurrent.STM

e8 = main

main :: IO ()
main = do
  v1 <- newTVarIO (1 :: Int)
  v2 <- newTVarIO (2 :: Int)
  replicateConcurrently_  7 (doit v1 v2)
  readTVarIO v2 >>= print
 where
  doit v1 v2 = atomically $ do
    v1' <- readTVar v1
    v2' <- readTVar v2
    writeTVar v1 v2'
    writeTVar v2 (v1' + v2')


