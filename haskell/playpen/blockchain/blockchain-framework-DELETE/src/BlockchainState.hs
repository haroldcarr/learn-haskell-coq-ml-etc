module BlockchainState
  ( initialBlockchainState
  )
where

import           Blockchain              (Blockchain, genesisBlock)

import           Control.Concurrent.MVar (MVar, newMVar)

initialBlockchainState :: IO (MVar Blockchain)
initialBlockchainState = newMVar [genesisBlock]
