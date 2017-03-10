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

runAcceptConnections :: String -> Int -> IO ()
runAcceptConnections host port = do
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

broadcastS :: Text -> Peers -> IO ()
broadcastS msg ps = do
  infoM consensusFollower ("WS S broadcastS: " <> show msg)
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

-- runInitiateConnection :: MVar ByteString -> String -> Int -> IO ()
runInitiateConnection httpToConsensus host port = do
  infoM consensusLeader "----------------------------------------------"
  infoM consensusLeader ("WS C runInitiateConnection: " <> host <> " " <> show port)
  peers <- newMVar []
  forkIO . withSocketsDo $ WS.runClient host port "/" (app peers)
  waitUntilAllConnected peers
  infoM consensusLeader "WS C runInitiateConnection: after waitUntilAllConnected"
  fc <- readMVar peers
  infoM consensusLeader "WS C runInitiateConnection: before sendC"
  sendC httpToConsensus fc
  infoM consensusLeader "WS C runInitiateConnection: Bye!"

waitUntilAllConnected peers = do
  infoM consensusLeader "WS C waitUntilAllConnected before MVAR"
  threadDelay 100000
  fc <- readMVar peers
  infoM consensusLeader "WS C waitUntilAllConnected after MVAR"
  unless (P.length fc == 1) $ waitUntilAllConnected peers

-- app :: MVar ByteString -> WS.ClientApp ()
app peers conn = do
  infoM consensusLeader "WS C app: Connected!"
  modifyMVar_ peers $ return . (conn :)
  recC conn

-- write anything received to stdout
recC conn = forever $ do
  liftIO $ infoM consensusLeader "WS C recC: waiting"
  msg <- WS.receiveData conn :: IO ByteString
  liftIO $ infoM consensusLeader ("WS C recC: rec: " <> show msg)

-- Read from httpToConsensus and write to WS
-- sendC :: MVar ByteString -> WS.Connection -> IO ()
sendC httpToConsensus peers = do
  infoM consensusLeader "WS C sendC: waiting"
  msg <- takeMVar httpToConsensus
  infoM consensusLeader ("WS C sendC: sending: " ++ show msg)
  broadcastC msg peers
  sendC httpToConsensus peers

broadcastC msg peers = do
  infoM consensusLeader ("WS C broadcastC: really sending: " ++ show msg)
  forM_ peers (\c -> WS.sendBinaryData c msg)
