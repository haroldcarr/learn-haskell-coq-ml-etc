#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# LANGUAGE OverloadedStrings #-}

module S_MonadReader where

import Control.Monad.Reader

smr :: IO ()
smr = main

main :: IO ()
main = runReaderT inner "Michael"

inner :: ReaderT String IO ()
inner = do
  name0 <- ask
  lift $ putStrLn $ "Name0: " ++ name0
  local (++ "!") $ do
    name1 <- ask
    liftIO $ putStrLn $ "Name1: " ++ name1

smr2 :: IO ()
smr2 = runReaderT i [1,2::Int]
 where
  i = do
    xs0 <- ask
    lift $ print xs0
    local (map (*2)) $ do
      xs1 <- ask
      liftIO $ print xs1
