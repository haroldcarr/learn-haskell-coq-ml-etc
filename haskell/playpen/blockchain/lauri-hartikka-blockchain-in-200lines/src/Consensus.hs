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

type PeerId = Int -- scope is this server
type Peer   = (PeerId, WS.Connection)
type Peers  = [Peer]

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
    s <- modifyMVar peers $ \s -> let s' = rmPeer peer s in return (s', s')
    T.putStrLn ("WS S disconnect: " <> T.pack (show pid) <> " disconnected")

broadcast :: Text -> Peers -> IO ()
broadcast msg ps = do
  T.putStrLn ("WS S broadcast: " <> msg)
  forM_ ps $ \(_,c) -> WS.sendTextData c msg

recS :: Peer -> MVar Peers -> IO ()
recS (_, c) peers = forever $ do
  msg <- WS.receiveData c
  T.putStrLn ("WS S recS: " <> msg)

addPeer :: Peer -> Peers -> Peers
addPeer p ps = p:ps

rmPeer :: Peer -> Peers -> Peers
rmPeer p = P.filter ((/= fst p) . fst)

--------------------------------------------------------------------------------
-- client

-- runLeader :: MVar ByteString -> String -> Int -> IO ()
runLeader httpToConsensus host port = do
  P.putStrLn "----------------------------------------------"
  P.putStrLn ("WS C runLeader: " <> host <> " " <> show port)
  followerConnections <- newMVar []
  forkIO . withSocketsDo $ WS.runClient host port "/" (app followerConnections)
  waitUntilAllConnected followerConnections
  P.putStrLn "WS C runLeader: after waitUntilAllConnected"
  fc <- readMVar followerConnections
  P.putStrLn "WS C runLeader: before sendC"
  sendC httpToConsensus fc
  P.putStrLn "WS C runLeader: Bye!"

waitUntilAllConnected followerConnections = do
  P.putStrLn "WS C waitUntilAllConnected before MVAR"
  threadDelay 100000
  fc <- readMVar followerConnections
  P.putStrLn "WS C waitUntilAllConnected after MVAR"
  if P.length fc == 1 then return () else waitUntilAllConnected followerConnections

-- app :: MVar ByteString -> WS.ClientApp ()
app followerConnections conn = do
  P.putStrLn "WS C app: Connected!"
  modifyMVar_ followerConnections $ return . (conn :)
  recC conn

-- write anything received to stdout
recC conn = forever $ do
  liftIO $ T.putStrLn ("WS C recC: waiting")
  msg <- WS.receiveData conn
  liftIO $ T.putStrLn ("WS C recC: rec: " <> msg)

-- Read from httpToConsensus and write to WS
-- sendC :: MVar ByteString -> WS.Connection -> IO ()
sendC httpToConsensus followerConnections = do
  P.putStrLn ("WS C sendC: waiting")
  msg <- takeMVar httpToConsensus
  P.putStrLn ("WS C sendC: sending: " ++ show msg)
  forM_ followerConnections $ \c -> do
    P.putStrLn ("WS C sendC: really sending: " ++ show msg)
    WS.sendBinaryData c msg >> sendC httpToConsensus followerConnections
