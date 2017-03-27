{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module TransportUDP
  (startNodeComm)
where

import           Consensus
import           Json
import           Logging                   (consensusFollower)

import           Control.Concurrent        (forkIO, takeMVar)
import           Data.Aeson                (decodeStrict, encode)
import           Data.ByteString           as BS (isPrefixOf)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Monoid               ((<>))
import           Network.Multicast         as NM (multicastReceiver,
                                                  multicastSender)
import           Network.Socket            as N (PortNumber, Socket,
                                                 withSocketsDo)
import           Network.Socket.ByteString as N (recvFrom, sendTo)
import           System.Log.Logger         (infoM)

startNodeComm sendToConsensusNodes host port = do
  infoN host port "startNodeComm: ENTER"
  (sendSock, sendAddr) <- multicastSender host port
  recSock <- multicastReceiver host port
  forkIO $ send sendToConsensusNodes host port sendSock sendAddr
  forkIO $ rec host port recSock sendSock sendAddr
  infoN host port "startNodeComm: EXIT"

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

-- Read from sendToConsensusNodes and broadcast
send sendToConsensusNodes host port sock addr = do
  infoN host port "send: waiting"
  msg <- takeMVar sendToConsensusNodes
  infoN host port ("send: " ++ show msg)
  sendTo sock msg addr
  send sendToConsensusNodes host port sock addr

infoN h p msg = do
  infoM consensusFollower ("T " <> h <> ":" <> show p <> " " <> msg)
  return 1 -- TODO - get rid of this

