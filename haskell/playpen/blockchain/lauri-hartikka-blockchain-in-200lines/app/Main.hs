module Main where

import           Http                  (site)
import           Logging               (configureLogging)
import           TransportUDP          (startNodeComm)

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

{-
stack exec bc -- 9160 224.0.0.99 9160 &
stack exec bc -- 9161 224.0.0.99 9160
-}

main = do
  xs <- getArgs
  case xs of
    []             -> doIt defaultPort            defaultHost (read (show defaultPort) :: PortNumber)
    [httpPort,h,p] -> doIt (read httpPort :: Int) h           (read p :: PortNumber)
    xs             -> error (show xs)

doIt httpPort host port = do
  configureLogging
  httpToConsensus <- newEmptyMVar
  startNodeComm httpToConsensus host port
  site httpToConsensus "0.0.0.0" httpPort
