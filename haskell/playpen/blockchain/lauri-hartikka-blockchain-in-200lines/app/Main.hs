module Main where

import           Http                  (site)
import           Logging
import           TransportUDP
import           Util

import           Control.Concurrent    (MVar, forkIO, newEmptyMVar, takeMVar,
                                        threadDelay)
import           Control.Monad         (forM_, forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.Monoid           ((<>))
import           Network.Socket        (PortNumber)
import           System.Environment    (getArgs)
import           System.Log.Logger     (infoM)

defaultHost = "224.0.0.99"
defaultPort = 9160

main = do
  xs <- getArgs
  case xs of
    []    -> doIt defaultHost defaultPort
    [h,p] -> doIt h (read p :: PortNumber)

doIt host port = do
  configureLogging
  httpToConsensus <- newEmptyMVar
  startNodeComm httpToConsensus host port
  site httpToConsensus "0.0.0.0" 9160
