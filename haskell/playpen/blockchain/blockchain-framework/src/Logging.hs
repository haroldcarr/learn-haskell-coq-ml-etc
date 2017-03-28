module Logging
  ( mainProgram
  , http
  , consensus
  , consensusFollower
  , consensusLeader
  , configureLogging
  )
where

import           System.Log.Logger (Priority (INFO), setLevel,
                                    updateGlobalLogger)

mainProgram, http, consensus, consensusFollower, consensusLeader :: String
mainProgram       = "MAIN"
http              = "HTTP"
consensus         = "Consensus"
consensusFollower = "Consensus.Follower"
consensusLeader   = "Consensus.Leader"

configureLogging :: IO ()
configureLogging  = do
  updateGlobalLogger mainProgram       (setLevel INFO)
  updateGlobalLogger http              (setLevel INFO)
  updateGlobalLogger consensus         (setLevel INFO)
  updateGlobalLogger consensusFollower (setLevel INFO)
  updateGlobalLogger consensusLeader   (setLevel INFO)
