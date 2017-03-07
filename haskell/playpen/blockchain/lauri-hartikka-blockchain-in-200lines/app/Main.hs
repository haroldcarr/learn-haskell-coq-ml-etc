module Main where

import           Consensus             (runClient, runServer)
import           Http                  (site)

import           Control.Concurrent    (MVar, newEmptyMVar)
import           Control.Concurrent    (forkIO)
import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)

host = "0.0.0.0"
port = 9160

main :: IO ()
main = do
  mvar <- newEmptyMVar
  forkIO $ forever (runServer host port)
  forkIO $ forever (runClient mvar host port)
  site mvar
