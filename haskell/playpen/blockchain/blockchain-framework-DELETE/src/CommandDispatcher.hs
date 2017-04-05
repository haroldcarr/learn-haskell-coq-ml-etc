{-# LANGUAGE OverloadedStrings #-}

module CommandDispatcher
  ( HandleConsensusMessage
  , CommandDispatcher (CommandDispatcher)
  )
where

import           Blockchain (Block, BlockData, Blockchain)
import           Consensus  (HandleConsensusMessage)

data CommandDispatcher =
  CommandDispatcher
  -- CONSENSUS
  {-  handleConsensusMessage       :: -} HandleConsensusMessage
  {-, getMsgToSendToConsensusNodes :: -} (IO BlockData)
  {-, sendToConsensusNodes         :: -} (BlockData -> IO ())
  -- BLOCKCHAIN
    -- Nothing: return all; Just i: return block at index i
  {-, listBlocks                   :: -} (Maybe Int -> IO (Maybe Blockchain))
  {-, addBlock                     :: -} (BlockData -> IO Block)  -- TODO : split into Blockchain and Consensus ops
  {-, isValid                      :: -} (Block     -> IO (Maybe String))
