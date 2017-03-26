{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module TransportUDP
  (startNodeComm)
where

import           Consensus
import           Json
import           Logging                   (consensusFollower)

import           Control.Concurrent        (forkIO, takeMVar)
import           Data.Aeson                (encode)
import           Data.ByteString           as BS (isPrefixOf)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Monoid               ((<>))
import           Network.Multicast         as NM (multicastReceiver,
                                                  multicastSender)
import           Network.Socket            as N (PortNumber, Socket,
                                                 withSocketsDo)
import           Network.Socket.ByteString as N (recvFrom, sendTo)
import           System.Log.Logger         (infoM)

startNodeComm httpToConsensus host port = do
  infoN host port "startNodeComm: ENTER"
  startSndRcvComm httpToConsensus host port
  infoN host port "startNodeComm: EXIT"

startSndRcvComm httpToConsensus host port = withSocketsDo $ do
  infoN host port "startSndRcvComm: ENTER"
  (sendSock, sendAddr) <- multicastSender host port
  recSock <- multicastReceiver host port
  forkIO $ send httpToConsensus host port sendSock sendAddr
  forkIO $ rec host port recSock sendSock sendAddr
  infoN host port "startSndRcvComm: EXIT"

rec host port recSock sendSock sendAddr = do
  infoN host port "rec: waiting"
  (msg,addr) <- N.recvFrom recSock 1024
  infoN host port  ("rec: from: " <> show addr <> " " <> show msg)
  if | BS.isPrefixOf "{\"appendEntry\":" msg -> do
         infoN host port "APPENDENTRY"
         sendTo sendSock (toStrict (encode (AppendEntryResponse True))) sendAddr
     | BS.isPrefixOf "{\"appendEntryResponse\":" msg -> do
         infoN host port "APPENDENTRYRESPONSE"
     | otherwise                             -> infoN host port "NO"
  rec host port recSock sendSock sendAddr

-- Read from httpToConsensus and broadcast
send httpToConsensus host port sock addr = do
  infoN host port "send: waiting"
  msg <- takeMVar httpToConsensus
  infoN host port ("send: " ++ show msg)
  sendTo sock msg addr
  send httpToConsensus host port sock addr

infoN h p msg = do
  infoM consensusFollower ("N " <> h <> ":" <> show p <> " " <> msg)
  return 1 -- TODO - get rid of this

