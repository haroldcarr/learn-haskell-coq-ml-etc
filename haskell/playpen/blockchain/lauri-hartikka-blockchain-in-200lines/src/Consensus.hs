{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Consensus where

import           Control.Concurrent            (MVar, forkIO, modifyMVar,
                                                modifyMVar_, newEmptyMVar,
                                                newMVar, readMVar, takeMVar,
                                                threadDelay)
import           Control.Exception             (finally)
import           Control.Monad                 (forM_, forever, unless)
import           Control.Monad.Trans           (liftIO)
import           Data.ByteString               (ByteString)
import           Data.ByteString               as BS
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Network.Socket                (withSocketsDo)
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import           Prelude                       as P
import           System.Log.Logger

type PeerId = Int -- scope is this server
type Peer   = (PeerId, WS.Connection)
type Peers  = [Peer]

consensusFollower = "Consensus.Follower"
consensusLeader   = "Consensus.Leader"

------------------------------------------------------------------------------
-- server

runFollower :: String -> Int -> IO ()
runFollower host port = do
  peers      <- newMVar []
  nextPeerId <- newMVar (-1)
  WS.runServer host port $ consensus peers nextPeerId

consensus :: MVar Peers -> MVar PeerId -> WS.ServerApp
consensus peers nextPeerId pending = do
  c <- WS.acceptRequest pending
  WS.forkPingThread c 30
  peerId <- modifyMVar nextPeerId $ \i -> return (i+1,i+1)
  let peer = (peerId, c)
  flip finally (disconnect peer) $ do
    modifyMVar_ peers $ return . addPeer peer
    recS peer peers
 where
  disconnect peer@(pid,_) = do
    modifyMVar_ peers $ return . rmPeer peer
    infoM consensusFollower ("WS S disconnect: " <> show pid <> " disconnected")

broadcast :: Text -> Peers -> IO ()
broadcast msg ps = do
  infoM consensusFollower ("WS S broadcast: " <> show msg)
  forM_ ps $ \(_,c) -> WS.sendTextData c msg

recS :: Peer -> MVar Peers -> IO ()
recS (_, c) peers = forever $ do
  msg <- WS.receiveData c :: IO ByteString
  infoM consensusFollower ("WS S recS: " <> show msg)
  if | BS.isPrefixOf "{\"appendEntry\":" msg -> infoM consensusFollower "APPENDENTRY"
     | otherwise                             -> infoM consensusFollower "NO"

addPeer :: Peer -> Peers -> Peers
addPeer p ps = p:ps

rmPeer :: Peer -> Peers -> Peers
rmPeer p = P.filter ((/= fst p) . fst)

--------------------------------------------------------------------------------
-- client

-- runLeader :: MVar ByteString -> String -> Int -> IO ()
runLeader httpToConsensus host port = do
  infoM consensusLeader "----------------------------------------------"
  infoM consensusLeader ("WS C runLeader: " <> host <> " " <> show port)
  followerConnections <- newMVar []
  forkIO . withSocketsDo $ WS.runClient host port "/" (app followerConnections)
  waitUntilAllConnected followerConnections
  infoM consensusLeader "WS C runLeader: after waitUntilAllConnected"
  fc <- readMVar followerConnections
  infoM consensusLeader "WS C runLeader: before sendC"
  sendC httpToConsensus fc
  infoM consensusLeader "WS C runLeader: Bye!"

waitUntilAllConnected followerConnections = do
  infoM consensusLeader "WS C waitUntilAllConnected before MVAR"
  threadDelay 100000
  fc <- readMVar followerConnections
  infoM consensusLeader "WS C waitUntilAllConnected after MVAR"
  unless (P.length fc == 1) $ waitUntilAllConnected followerConnections

-- app :: MVar ByteString -> WS.ClientApp ()
app followerConnections conn = do
  infoM consensusLeader "WS C app: Connected!"
  modifyMVar_ followerConnections $ return . (conn :)
  recC conn

-- write anything received to stdout
recC conn = forever $ do
  liftIO $ infoM consensusLeader "WS C recC: waiting"
  msg <- WS.receiveData conn :: IO ByteString
  liftIO $ infoM consensusLeader ("WS C recC: rec: " <> show msg)

-- Read from httpToConsensus and write to WS
-- sendC :: MVar ByteString -> WS.Connection -> IO ()
sendC httpToConsensus followerConnections = do
  infoM consensusLeader "WS C sendC: waiting"
  msg <- takeMVar httpToConsensus
  infoM consensusLeader ("WS C sendC: sending: " ++ show msg)
  forM_ followerConnections $ \c -> do
    infoM consensusLeader ("WS C sendC: really sending: " ++ show msg)
    WS.sendBinaryData c msg >> sendC httpToConsensus followerConnections
