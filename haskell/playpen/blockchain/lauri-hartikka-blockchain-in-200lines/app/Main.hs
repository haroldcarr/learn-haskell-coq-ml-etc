module Main where

import           Consensus             (consensusFollower, consensusLeader,
                                        runAcceptConnections,
                                        runInitiateConnection)
import           Http                  (site)
import           Util

import           Control.Concurrent    (MVar, forkIO, newEmptyMVar)
import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.Monoid           ((<>))
import           System.Environment    (getArgs)
import           System.Log.Logger

mainProgram = "main"
type Host = String
type Port = Int
host = "0.0.0.0"
port = 9160

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [] -> doIt [((host,port), [(host,port)])] -- for testing
    xs -> do
      if not (even (length xs))
        then error "Usage"
        else doIt (foo (mkHostPortPairs xs))

-- doIt :: [((Host, Port), [(Host, Port)])] -> IO ()
doIt all@((leader@(host,port), followers):_) = do
  configureLogging
  initializePeers (leader:followers)
  httpToConsensus <- connectPeers all
  site httpToConsensus

initializePeers = mapM_ (\(host, port) -> forkIO $ forever (runAcceptConnections host port))

connectPeers xs = do
  infoM mainProgram ("connectPeers: " <> show xs)
  httpToConsensus <- newEmptyMVar
  mapM (\((host,port),followers) -> forkIO $ forever (runInitiateConnection httpToConsensus host port)) xs
  return httpToConsensus

configureLogging = do
  updateGlobalLogger mainProgram       (setLevel INFO)
  updateGlobalLogger consensusFollower (setLevel INFO)
  updateGlobalLogger consensusLeader   (setLevel INFO)
