{-# LANGUAGE OverloadedStrings #-}

module TransportUDP
  (startNodeComm)
where

import           Blockchain                (Block, BlockData)
import           CommandDispatcher
import           Logging                   (consensusFollower)

import           Control.Concurrent        (forkIO)
import           Data.Monoid               ((<>))
import           Network.Multicast         as NM (multicastReceiver,
                                                  multicastSender)
import           Network.Socket            as N (HostName, PortNumber, SockAddr,
                                                 Socket)
import           Network.Socket.ByteString as N (recvFrom, sendTo)
import           System.Log.Logger         (infoM)

startNodeComm :: CommandDispatcher -> HostName -> PortNumber -> IO ()
startNodeComm (CommandDispatcher handleConsensusMessage getMsgsToSendToConsensusNodes sendToConsensusNodes _ _ isValid) host port = do
  _ <- infoN host port "startNodeComm: ENTER"
  (sendSock, sendAddr) <- multicastSender host port
  recSock <- multicastReceiver host port
  forkIO $ send host port sendSock sendAddr getMsgsToSendToConsensusNodes
  forkIO $ rec host port recSock sendSock sendAddr handleConsensusMessage sendToConsensusNodes isValid
  infoN host port "startNodeComm: EXIT"
  return ()

rec :: HostName -> PortNumber -> Socket -> Socket -> SockAddr
    -> HandleConsensusMessage
    -> (BlockData -> IO ())
    -> (Block -> IO (Maybe String))
    -> IO ()
rec host port recSock sendSock sendAddr handleConsensusMessage sendToConsensusNodes isValid = do
  infoN host port "rec: waiting"
  (msg,addr) <- N.recvFrom recSock 1024
  infoN host port  ("rec: from: " <> show addr <> " " <> show msg)
  handleConsensusMessage host port sendToConsensusNodes isValid msg
  rec host port recSock sendSock sendAddr handleConsensusMessage sendToConsensusNodes isValid

-- Read from sendToConsensusNodes and broadcast
send :: HostName -> PortNumber -> Socket -> SockAddr -> IO BlockData -> IO () -- TODO ByteString
send host port sock addr getMsgsToSendToConsensusNodes = do
  infoN host port "send: waiting"
  msg <- getMsgsToSendToConsensusNodes
  infoN host port ("send: " ++ show msg)
  sendTo sock msg addr
  send host port sock addr getMsgsToSendToConsensusNodes

infoN :: HostName -> PortNumber -> String -> IO Int
infoN h p msg = do
  infoM consensusFollower ("T " <> h <> ":" <> show p <> " " <> msg)
  return 1 -- to match sendTo

