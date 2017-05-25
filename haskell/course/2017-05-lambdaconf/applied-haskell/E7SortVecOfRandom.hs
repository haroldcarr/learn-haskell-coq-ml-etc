#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module E7SortVecOfRandom where

import           Data.Foldable                    (forM_)
import qualified Data.Vector                      as V
import           Data.Vector.Algorithms.Insertion (sort)
import qualified Data.Vector.Mutable              as VM
import           System.Random                    (getStdGen, randomR,
                                                   randomRIO)

e7 = main

main = do
  m1
  m2
  m3

m1 :: IO ()
m1 = do
  v <- V.replicateM 100 $ randomRIO (1, 10000 :: Int)
  print $ V.modify sort v

m2 :: IO ()
m2 = do
  gen0 <- getStdGen
  print $ V.create $ do
    mv <- VM.new 100
    let loop gen idx
          | idx >= 100 = return ()
          | otherwise = do
              let (x, gen') = randomR (1, 10000) gen
              VM.write mv idx (x :: Int)
              loop gen' (idx + 1)
    loop gen0 0
    sort mv
    return mv

m3 :: IO ()
m3 = do
  mv <- VM.new 100
  forM_ [0..99] $ \idx -> do
    x <- randomRIO (1, 10000)
    VM.write mv idx (x :: Int)
  sort mv
  v <- V.unsafeFreeze mv
  print v


