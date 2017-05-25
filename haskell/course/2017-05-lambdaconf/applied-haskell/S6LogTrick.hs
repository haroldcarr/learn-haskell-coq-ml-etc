#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module S6LogTrick where

import Data.Functor.Identity

add1 :: Monad m => (String -> m ()) -> Int -> Int -> m Int
add1 logFunc x y = do
  logFunc $ "inside add1 with " ++ show (x, y)
  return $ x + y

s6m1 = m1

m1 :: IO ()
m1 = do
  x <- add1 putStrLn 5 6
  print x

add2 :: Monad m => (String -> m ()) -> Int -> Int -> m Int
add2 logFunc x y = do
  logFunc $ "inside add2 with " ++ show (x, y)
  return $ x + y
{-# SPECIALIZE add2 :: (String -> Identity ()) -> Int -> Int -> Identity Int #-}
{-# INLINE add2 #-}

s6m2 = m2

m2 :: IO ()
m2 = do
  x <- add2 putStrLn 5 6
  print x
  print $ runIdentity $ add2 (const $ return ()) 5 6

main = do
  _ <- m1
  _ <- m2
  return ()
