module Main where

import           Consensus             (runAcceptConnections,
                                        runInitiateConnection)
import           Http                  (site)
import           Logging
import           Util

import           Control.Concurrent    (MVar, forkIO, newEmptyMVar, takeMVar,
                                        threadDelay)
import           Control.Monad         (forM_, forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.Monoid           ((<>))
import           System.Environment    (getArgs)
import           System.Log.Logger     (infoM)

host = "0.0.0.0"
port = 9160

-- stack exec bc -- 0.0.0.0 9160 0.0.0.0 9161 0.0.0.0 9162

main = do
  xs <- getArgs
  case xs of
    [] -> doIt [((host,port), [(host,port+1)])] -- for testing : this causes address already in use - can be ignored
    xs -> do
      if not (even (length xs))
        then error "Usage [ host port ... ]"
        else doIt (mkInitiatorList (mkHostPortPairs xs))

doIt all@((leader@(host,port), followers):_) = do
  configureLogging
  initializePeers (leader:followers)
  connectPeers all
  forever $ threadDelay 10000000

initializePeers = mapM_ (\(host, port) -> forkIO $ forever (runAcceptConnections host port))

connectPeers xs = do
  infoM mainProgram ("connectPeers ENTER: " <> show xs)
  httpToConsensus <- newEmptyMVar
  forM_ xs $ \((host,port),targets) -> do
    infoM mainProgram ("connectPeers " <> host <> " " <> show port <> " connecting to " <> show targets)
    forkIO $ forever (runInitiateConnection httpToConsensus host port targets)
    forkIO $ site httpToConsensus host (10000 + port)
