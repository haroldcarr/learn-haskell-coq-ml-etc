{-# LANGUAGE OverloadedStrings #-}

module Consensus where

import           Control.Concurrent  (forkIO)
import           Control.Concurrent  (MVar, modifyMVar, modifyMVar_, newMVar,
                                      readMVar, takeMVar)
import           Control.Exception   (finally)
import           Control.Monad       (forM_, forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString     (ByteString)
import           Data.ByteString     as BS
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Network.Socket      (withSocketsDo)
import qualified Network.WebSockets  as WS
import           Prelude             as P

type Peer  = (Text, WS.Connection)
type Peers = [Peer]

runServer :: String -> Int -> IO ()
runServer host port = do
  state <- newMVar []
  WS.runServer host port $ consensus state

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
  -- readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)
  T.putStrLn msg

addPeer :: Peer -> Peers -> Peers
addPeer p ps = p:ps

rmPeer :: Peer -> Peers -> Peers
rmPeer p = P.filter ((/= fst p) . fst)

--------------------------------------------------------------------------------
-- client

runClient :: MVar ByteString -> String -> Int -> IO ()
runClient mvar host port = withSocketsDo $ WS.runClient host port "/" (app mvar)

app :: MVar ByteString -> WS.ClientApp ()
app mvar conn = do
  P.putStrLn "Connected!"
  -- RECEIVE: Fork a thread that writes anything received to stdout
  forkIO $ forever $ do
    msg <- WS.receiveData conn
    liftIO $ T.putStrLn msg
  loop mvar conn
  WS.sendClose conn ("Bye!" :: Text)

-- SEND: Read from stdin and write to WS
loop :: MVar ByteString -> WS.Connection -> IO ()
loop mvar conn = do
  msg <- takeMVar mvar
  P.putStrLn ("LOOP: " ++ show msg)
  WS.sendBinaryData conn msg >> loop mvar conn
