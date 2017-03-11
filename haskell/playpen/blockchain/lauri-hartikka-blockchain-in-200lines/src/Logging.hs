module Logging
  ( mainProgram
  , http
  , consensusFollower
  , consensusLeader
  , configureLogging
  )
where

import           System.Log.Logger (Priority (INFO), setLevel,
                                    updateGlobalLogger)

mainProgram       = "MAIN"
http              = "HTTP"
consensusFollower = "Consensus.Follower"
consensusLeader   = "Consensus.Leader"

configureLogging = do
  updateGlobalLogger mainProgram       (setLevel INFO)
  updateGlobalLogger http              (setLevel INFO)
  updateGlobalLogger consensusFollower (setLevel INFO)
  updateGlobalLogger consensusLeader   (setLevel INFO)
