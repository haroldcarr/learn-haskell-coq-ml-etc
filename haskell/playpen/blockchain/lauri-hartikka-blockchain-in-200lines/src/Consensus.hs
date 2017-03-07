{-# LANGUAGE OverloadedStrings #-}

module Consensus where

import           Control.Concurrent  (forkIO)
import           Control.Concurrent  (MVar, modifyMVar, modifyMVar_, newMVar,
                                      readMVar)
import           Control.Exception   (finally)
import           Control.Monad       (forM_, forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Network.Socket      (withSocketsDo)
import qualified Network.WebSockets  as WS

type Peer  = (Text, WS.Connection)
type Peers = [Peer]

runServer :: Int -> IO ()
runServer _ = do
  state <- newMVar []
  WS.runServer "0.0.0.0" 9160 $ consensus state

consensus :: MVar Peers -> WS.ServerApp
consensus state pending = do
  c <- WS.acceptRequest pending
  WS.forkPingThread c 30
  msg <- WS.receiveData c
  let client = (msg, c)
  flip finally (disconnect client) $ do
    modifyMVar_ state $ \s -> do
      let s' = addPeer client s
      broadcast (fst client `mappend` " joined") s'
      return s'
    talk c state client
 where
  disconnect client = do
    s <- modifyMVar state $ \s ->
      let s' = rmPeer client s in return (s', s')
    broadcast (fst client `mappend` " disconnected") s

broadcast :: Text -> Peers -> IO ()
broadcast msg ps = do
  T.putStrLn msg
  forM_ ps $ \(_, c) -> WS.sendTextData c msg

talk :: WS.Connection -> MVar Peers -> Peer -> IO ()
talk c state (user, _) = forever $ do
  msg <- WS.receiveData c
  readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)

addPeer :: Peer -> Peers -> Peers
addPeer p ps = p:ps

rmPeer :: Peer -> Peers -> Peers
rmPeer p = filter ((/= fst p) . fst)

--------------------------------------------------------------------------------
-- client

runClient :: IO ()
runClient = withSocketsDo $ WS.runClient "0.0.0.0" 9160 "/" app

app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected!"
  -- RECEIVE: Fork a thread that writes anything received to stdout
  forkIO $ forever $ do
    msg <- WS.receiveData conn
    liftIO $ T.putStrLn msg
  loop conn
  WS.sendClose conn ("Bye!" :: Text)

-- SEND: Read from stdin and write to WS
loop :: WS.Connection -> IO ()
loop conn = do
  line <- T.getLine
  unless (T.null line) $ WS.sendTextData conn line >> loop conn
