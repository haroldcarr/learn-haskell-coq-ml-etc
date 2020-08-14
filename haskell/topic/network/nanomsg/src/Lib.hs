{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forever, mapM_)
import qualified Data.ByteString.Char8 as C
import           Nanomsg
import           Protolude

------------------------------------------------------------------------------

pubServer :: IO ()
pubServer  =
  withSocket Pub $ \s -> do
    _ <- bind s "tcp://*:5560"
    mapM_ (sendNumber s) (cycle [1..1000000 :: Int])
 where
  sendNumber s number = do
    threadDelay 1000
    send s (C.pack (show number))

subClient :: Bool -> IO ()
subClient shouldBlock =
  withSocket Sub $ \s -> do
    _ <- connect s "tcp://localhost:5560"
    subscribe s $ C.pack ""
    forever $
      if shouldBlock
      then do
        msg <- recv s
        C.putStrLn msg
      else do
        threadDelay 700
        msg <- recv' s
        C.putStrLn $ fromMaybe (C.pack "No message") msg

------------------------------------------------------------------------------

