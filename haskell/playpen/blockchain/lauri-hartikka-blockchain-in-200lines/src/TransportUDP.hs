{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module TransportUDP
  (startNodeComm)
where

import           CommandDispatcher
import           Consensus
import           Json
import           Logging                   (consensusFollower)

import           Control.Concurrent        (forkIO, takeMVar)
import           Data.Aeson                (decodeStrict, encode)
import           Data.ByteString           as BS (isInfixOf, isPrefixOf)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Monoid               ((<>))
import           Network.Multicast         as NM (multicastReceiver,
                                                  multicastSender)
import           Network.Socket            as N (PortNumber, Socket,
                                                 withSocketsDo)
import           Network.Socket.ByteString as N (recvFrom, sendTo)
import           System.Log.Logger         (infoM)

startNodeComm (CommandDispatcher sendToConsensusNodes _ _ isValid) host port = do
  infoN host port "startNodeComm: ENTER"
  (sendSock, sendAddr) <- multicastSender host port
  recSock <- multicastReceiver host port
  forkIO $ send sendToConsensusNodes host port sendSock sendAddr
  forkIO $ rec host port recSock sendSock sendAddr isValid
  infoN host port "startNodeComm: EXIT"

rec host port recSock sendSock sendAddr isValid = do
  infoN host port "rec: waiting"
  (msg,addr) <- N.recvFrom recSock 1024
  infoN host port  ("rec: from: " <> show addr <> " " <> show msg)
  if | BS.isPrefixOf "{\"appendEntry\":" msg -> do
         infoN host port "APPENDENTRY"
         case decodeStrict msg of
           Just (AppendEntry block) -> do
             v <- isValid block
             case v of
               Nothing -> sendTo sendSock (toStrict (encode (AppendEntryResponse True (Just block)))) sendAddr
               _       -> sendTo sendSock (toStrict (encode (AppendEntryResponse False Nothing)))     sendAddr
     | BS.isInfixOf "\"appendEntryResponse\":" msg -> do
         infoN host port "APPENDENTRYRESPONSE"
         case decodeStrict msg of
           Just aer@(AppendEntryResponse b mBlock) -> infoN host port (show aer)
           Nothing                                 -> infoN host port "AER NOT OK"
     | otherwise -> infoN host port "NO"
  rec host port recSock sendSock sendAddr isValid

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

