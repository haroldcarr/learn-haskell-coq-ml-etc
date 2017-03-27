{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher where

import           Blockchain (Block, BlockData, Blockchain, Timestamp)

data CommandDispatcher =
  CommandDispatcher {
    listBlocks :: Maybe Int -> IO (Maybe Blockchain)     -- Nothing: return all; Just i: return block at index i
  , addBlock   :: BlockData -> IO Block
  }