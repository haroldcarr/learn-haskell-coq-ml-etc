{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module TransportUDP
  (startNodeComm)
where

import           Blockchain                (Block)
import           CommandDispatcher
import           Consensus
import           Json                      ()
import           Logging                   (consensusFollower)

import           Control.Concurrent        (MVar, forkIO, takeMVar)
import           Data.Aeson                (decodeStrict, encode)
import           Data.ByteString           as BS (ByteString, isInfixOf,
                                                  isPrefixOf)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Monoid               ((<>))
import           Network.Multicast         as NM (multicastReceiver,
                                                  multicastSender)
import           Network.Socket            as N (PortNumber, SockAddr, Socket)
import           Network.Socket.ByteString as N (recvFrom, sendTo)
import           System.Log.Logger         (infoM)

startNodeComm :: CommandDispatcher -> String -> PortNumber -> IO ()
startNodeComm (CommandDispatcher sendToConsensusNodes0 _ _ isValid0) host port = do
  _ <- infoN host port "startNodeComm: ENTER"
  (sendSock, sendAddr) <- multicastSender host port
  recSock <- multicastReceiver host port
  forkIO $ send sendToConsensusNodes0 host port sendSock sendAddr
  forkIO $ rec host port recSock sendSock sendAddr isValid0
  infoN host port "startNodeComm: EXIT"
  return ()

rec :: String -> PortNumber -> Socket -> Socket -> SockAddr -> (Block -> IO (Maybe t)) -> IO ()
rec host port recSock sendSock sendAddr isValid0 = do
  infoN host port "rec: waiting"
  (msg,addr) <- N.recvFrom recSock 1024
  infoN host port  ("rec: from: " <> show addr <> " " <> show msg)
  if | BS.isPrefixOf "{\"appendEntry\":" msg -> do
         infoN host port "APPENDENTRY"
         case decodeStrict msg of
           Just (AppendEntry blk) -> do
             v <- isValid0 blk
             case v of
               Nothing -> sendTo sendSock (toStrict (encode (AppendEntryResponse True  (Just blk)))) sendAddr
               _       -> sendTo sendSock (toStrict (encode (AppendEntryResponse False (Just blk)))) sendAddr
           Nothing ->     sendTo sendSock (toStrict (encode (AppendEntryResponse False Nothing)))    sendAddr
     | BS.isInfixOf "\"appendEntryResponse\":" msg -> do
         infoN host port "APPENDENTRYRESPONSE"
         case decodeStrict msg of
           Just aer@(AppendEntryResponse _ _) -> infoN host port (show aer)
           Nothing                            -> infoN host port "AER NOT OK"
     | otherwise -> infoN host port "NO"
  rec host port recSock sendSock sendAddr isValid0

-- Read from sendToConsensusNodes and broadcast
send :: MVar ByteString -> String -> PortNumber -> Socket -> SockAddr -> IO ()
send sendToConsensusNodes0 host port sock addr = do
  infoN host port "send: waiting"
  msg <- takeMVar sendToConsensusNodes0
  infoN host port ("send: " ++ show msg)
  sendTo sock msg addr
  send sendToConsensusNodes0 host port sock addr

infoN :: String -> PortNumber -> String -> IO Int
infoN h p msg = do
  infoM consensusFollower ("T " <> h <> ":" <> show p <> " " <> msg)
  return 1 -- to match sendTo

