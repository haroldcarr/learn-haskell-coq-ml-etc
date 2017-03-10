{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Consensus where

import           Logging
import           Util

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
import           System.Log.Logger             (infoM)

type PeerId = Int -- scope is this server
type Peer   = (PeerId, WS.Connection)
type Peers  = [Peer]

------------------------------------------------------------------------------
-- server

runAcceptConnections :: String -> Int -> IO ()
runAcceptConnections host port = do
  peers      <- newMVar []
  nextPeerId <- newMVar (-1)
  WS.runServer host port $ consensus host port peers nextPeerId

consensus :: Host -> Port -> MVar Peers -> MVar PeerId -> WS.ServerApp
consensus host port peers nextPeerId pending = do
  c <- WS.acceptRequest pending
  WS.forkPingThread c 30
  peerId <- modifyMVar nextPeerId $ \i -> return (i+1,i+1)
  let peer = (peerId, c)
  flip finally (disconnect peer) $ do
    modifyMVar_ peers $ return . addPeer peer
    recS host port peer peers
 where
  disconnect peer@(pid,_) = do
    modifyMVar_ peers $ return . rmPeer peer
    infoS host port ("disconnect: " <> show pid <> " disconnected")

broadcastS :: Host -> Port -> Text -> Peers -> IO ()
broadcastS host port msg ps = do
  infoS host port ("broadcastS: " <> show msg)
  forM_ ps $ \(_,c) -> WS.sendTextData c msg

recS :: Host -> Port -> Peer -> MVar Peers -> IO ()
recS host port (_, c) peers = forever $ do
  msg <- WS.receiveData c :: IO ByteString
  infoS host port ("recS: " <> show msg)
  if | BS.isPrefixOf "{\"appendEntry\":" msg -> infoS host port "APPENDENTRY"
     | otherwise                             -> infoS host port "NO"

addPeer :: Peer -> Peers -> Peers
addPeer p ps = p:ps

rmPeer :: Peer -> Peers -> Peers
rmPeer p = P.filter ((/= fst p) . fst)

wsS h p = "WS S " <> h <> ":" <> show p <> " "
infoS h p msg = infoM consensusFollower ((wsS h p) <> msg)

--------------------------------------------------------------------------------
-- client

-- runInitiateConnection :: MVar ByteString -> String -> Int -> IO ()
runInitiateConnection httpToConsensus host port = do
  infoC host port "----------------------------------------------"
  infoC host port "runInitiateConnection: ENTER"
  peers <- newMVar []
  forkIO . withSocketsDo $ WS.runClient host port "/" (app host port peers)
  waitUntilAllConnected host port peers
  infoC host port "runInitiateConnection: after waitUntilAllConnected"
  fc <- readMVar peers
  infoC host port "runInitiateConnection: before sendC"
  sendC host port httpToConsensus fc
  infoC host port "runInitiateConnection: EXIT"

waitUntilAllConnected host port peers = do
  infoS host port "waitUntilAllConnected before MVAR"
  threadDelay 100000
  fc <- readMVar peers
  infoS host port "waitUntilAllConnected after MVAR"
  unless (P.length fc == 1) $ waitUntilAllConnected host port peers

-- app :: MVar ByteString -> WS.ClientApp ()
app host port peers conn = do
  infoC host port "app: Connected"
  modifyMVar_ peers $ return . (conn :)
  recC host port conn

-- write anything received to stdout
recC host port conn = forever $ do
  liftIO $ infoC host port "recC: waiting"
  msg <- WS.receiveData conn :: IO ByteString
  liftIO $ infoC host port  ("recC: rec: " <> show msg)

-- Read from httpToConsensus and write to WS
-- sendC :: MVar ByteString -> WS.Connection -> IO ()
sendC host port httpToConsensus peers = do
  infoC host port "sendC: waiting"
  msg <- takeMVar httpToConsensus
  infoC host port ("sendC: sending: " ++ show msg)
  broadcastC host port msg peers
  sendC host port httpToConsensus peers

broadcastC host port msg peers = do
  infoC host port ("broadcastC: really sending: " ++ show msg)
  forM_ peers (\c -> WS.sendBinaryData c msg)

wsC h p = "WS C " <> h <> ":" <> show p <> " "
infoC h p msg = infoM consensusLeader ((wsC h p) <> msg)

