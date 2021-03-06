{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Blockchain           as BC (Block, BlockData, Blockchain,
                                             addBlock, generateNextBlock,
                                             genesisBlock, isValidChain)
import           BlockchainState      (initialBlockchainState)
import           CommandDispatcher
import           Consensus
import           Http                 (commandReceiver)
import           Logging              (configureLogging)
import           TransportUDP         (startNodeComm)

import           Control.Concurrent   (MVar, newEmptyMVar, putMVar, takeMVar,
                                       withMVar)
import           Control.Lens         (element, (^?))
import           Data.Aeson           (encode)
import           Data.ByteString.Lazy (toStrict)
import           Network.Socket       (HostName, PortNumber)
import           System.Environment   (getArgs)

defaultHost :: HostName
defaultHost  = "224.0.0.99"
defaultPort :: PortNumber
defaultPort  = 9160

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    []             -> doIt defaultPort                   defaultHost (read (show defaultPort) :: PortNumber)
    [httpPort,h,p] -> doIt (read httpPort :: PortNumber) h           (read p                  :: PortNumber)
    xss            -> error (show xss)

doIt :: PortNumber -> HostName -> PortNumber -> IO ()
doIt httpPort host port = do
  configureLogging
  commandDispatcher <- initializeCommandDispatcher
  startNodeComm commandDispatcher host port
  commandReceiver commandDispatcher "0.0.0.0" httpPort

initializeCommandDispatcher :: IO CommandDispatcher
initializeCommandDispatcher = do
  blockchainState <- initialBlockchainState
  mv <- newEmptyMVar
  return (CommandDispatcher
          Consensus.handleConsensusMessage
          (getMsgsToSendToConsensusNodes mv)
          (sendToConsensusNodes mv)
          (Main.listBlocks blockchainState)
          (Main.addBlock mv)
          (Main.isValid blockchainState))

getMsgsToSendToConsensusNodes :: MVar BlockData -> IO BlockData
getMsgsToSendToConsensusNodes  = takeMVar

sendToConsensusNodes :: MVar BlockData -> BlockData -> IO ()
sendToConsensusNodes  = putMVar

listBlocks :: MVar Blockchain -> Maybe Int -> IO (Maybe Blockchain)
listBlocks blockchain i =
  case i of
    -- return all entries
    Nothing -> withMVar blockchain $ return . Just
    -- return the single entry (as a one-element list)
    Just i' -> withMVar blockchain $ \bc -> case bc ^? element i' of
                                              Nothing -> return Nothing
                                              Just el -> return (Just [el])

addBlock :: MVar BlockData -> BlockData -> IO Block
addBlock sendToConsensusNodesMV blockdata = do
  let newBlock = generateNextBlock genesisBlock "fake timestamp" blockdata
  -- send block to verifiers
  putMVar sendToConsensusNodesMV (toStrict (encode (AppendEntry newBlock)))
  -- return block to caller
  return newBlock

isValid :: MVar Blockchain -> Block -> IO (Maybe String)
isValid blockchain blk =
  withMVar blockchain $ \bc -> return $ isValidChain (BC.addBlock blk bc)

