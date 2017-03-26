{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Blockchain            (Blockchain, generateNextBlock,
                                        genesisBlock)
import           BlockchainState       (initialBlockchainState)
import           CommandDispatcher
import           Consensus
import           Http                  (site)
import           Logging               (configureLogging)
import           TransportUDP          (startNodeComm)

import           Control.Concurrent    (MVar, forkIO, newEmptyMVar, putMVar,
                                        takeMVar, threadDelay, withMVar)
import           Control.Lens          (element, (^?))
import           Control.Monad         (forM_, forever)
import           Data.Aeson            (encode)
import           Data.ByteString       (ByteString)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Network.Socket        (PortNumber)
import           System.Environment    (getArgs)
import           System.Log.Logger     (infoM)

defaultHost = "224.0.0.99"
defaultPort = 9160

main = do
  xs <- getArgs
  case xs of
    []             -> doIt defaultPort            defaultHost (read (show defaultPort) :: PortNumber)
    [httpPort,h,p] -> doIt (read httpPort :: Int) h           (read p :: PortNumber)
    xs             -> error (show xs)

doIt httpPort host port = do
  configureLogging
  httpToConsensus <- newEmptyMVar
  startNodeComm httpToConsensus host port
  commandDispatcher <- initializeCommandDispatcher httpToConsensus
  site commandDispatcher "0.0.0.0" httpPort

initializeCommandDispatcher httpToConsensus = do
  blockchainState <- initialBlockchainState
  return (CommandDispatcher
          (Main.listBlocks blockchainState)
          (Main.addBlock blockchainState httpToConsensus))

listBlocks :: MVar Blockchain -> Maybe Int -> IO (Maybe Blockchain)
listBlocks blockchain i =
  case i of
    Nothing -> withMVar blockchain $ return . Just
    Just i' -> withMVar blockchain $ \x -> case x ^? element i' of
                                             Nothing -> return Nothing
                                             Just el -> return (Just [el])

addBlock blockchain httpToConsensus blockdata = do
  let newBlock = generateNextBlock genesisBlock "fake timestamp" blockdata
  -- send block to verifiers
  sendAppendEntries httpToConsensus newBlock
  -- return block to caller
  return newBlock

sendAppendEntries httpToConsensus block =
  putMVar httpToConsensus (toStrict (encode (AppendEntry block)))
