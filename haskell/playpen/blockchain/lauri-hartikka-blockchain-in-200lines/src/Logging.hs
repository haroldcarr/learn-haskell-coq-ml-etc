module Logging where

import           System.Log.Logger

mainProgram       = "main"
consensusFollower = "Consensus.Follower"
consensusLeader   = "Consensus.Leader"

configureLogging = do
  updateGlobalLogger mainProgram       (setLevel INFO)
  updateGlobalLogger consensusFollower (setLevel INFO)
  updateGlobalLogger consensusLeader   (setLevel INFO)
