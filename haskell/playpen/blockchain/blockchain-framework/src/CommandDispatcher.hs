{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher where

import           Blockchain              (Block, BlockData, Blockchain)

import           Control.Concurrent.MVar

data CommandDispatcher =
  CommandDispatcher {
    sendToConsensusNodes :: MVar BlockData
  , listBlocks           :: Maybe Int -> IO (Maybe Blockchain) -- Nothing: return all; Just i: return block at index i
  , addBlock             :: BlockData -> IO Block
  , isValid              :: Block     -> IO (Maybe String)
  }
