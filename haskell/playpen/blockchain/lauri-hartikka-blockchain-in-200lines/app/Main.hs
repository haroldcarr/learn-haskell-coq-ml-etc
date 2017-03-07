module Main where

import           Consensus          (runClient, runServer)
import           Http               (site)

import           Control.Concurrent (forkIO)
import           Control.Monad      (forever)

main :: IO ()
main = do
  forkIO $ forever (runServer 0)
  forkIO $ forever runClient
  site
