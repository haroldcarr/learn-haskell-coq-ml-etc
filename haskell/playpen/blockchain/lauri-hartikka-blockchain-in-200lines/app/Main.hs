module Main where

import           Consensus             (runAcceptConnections,
                                        runInitiateConnection)
import           Http                  (site)
import           Logging
import           Util

import           Control.Concurrent    (MVar, forkIO, newEmptyMVar)
import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.Monoid           ((<>))
import           System.Environment    (getArgs)
import           System.Log.Logger     (infoM)

host = "0.0.0.0"
port = 9160

main = do
  xs <- getArgs
  case xs of
    [] -> doIt [((host,port), [(host,port)])] -- for testing
    xs -> do
      if not (even (length xs))
        then error "Usage"
        else doIt (foo (mkHostPortPairs xs))

doIt all@((leader@(host,port), followers):_) = do
  configureLogging
  initializePeers (leader:followers)
  httpToConsensus <- connectPeers all
  site httpToConsensus (10000 + port)

initializePeers = mapM_ (\(host, port) -> forkIO $ forever (runAcceptConnections host port))

connectPeers xs = do
  infoM mainProgram ("connectPeers ENTER: " <> show xs)
  httpToConsensus <- newEmptyMVar
  mapM (\((host,port),followers) -> forkIO $ do
    infoM mainProgram ("connectPeers " <> host <> " " <> show port)
    forever (runInitiateConnection httpToConsensus host port)) xs
  return httpToConsensus

