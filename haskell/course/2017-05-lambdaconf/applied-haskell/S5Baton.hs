#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module S5Baton where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.Function            (fix)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

s5 = main

app :: Application
app _req send = send $ responseBuilder status200 [] "Hello World"

main :: IO ()
main = do
    baton <- newEmptyMVar
    race_ (warp baton) (prompt baton)
  where
    warp baton = runSettings
      (setBeforeMainLoop (putMVar baton ()) defaultSettings)
      app
    prompt baton = do
      putStrLn "Waiting for Warp to be ready..."
      _ <- takeMVar baton
      putStrLn "Warp is now ready, type 'quit' to exit"
      fix $ \loop -> do
        line <- getLine
        if line == "quit"
          then putStrLn "Goodbye!"
          else putStrLn "I didn't get that, try again" >> loop
