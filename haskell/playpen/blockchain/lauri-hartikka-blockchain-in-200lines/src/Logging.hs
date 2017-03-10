module Logging where

import           System.Log.Logger

mainProgram       = "MAIN"
http              = "HTTP"
consensusFollower = "Consensus.Follower"
consensusLeader   = "Consensus.Leader"

configureLogging = do
  updateGlobalLogger mainProgram       (setLevel INFO)
  updateGlobalLogger http              (setLevel INFO)
  updateGlobalLogger consensusFollower (setLevel INFO)
  updateGlobalLogger consensusLeader   (setLevel INFO)
