{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module XTestUtils where

import           X0
------------------------------------------------------------------------------
import qualified Data.Set  as Set
import           Protolude

node0, node1, node2 :: NodeId
node0 = "node0"
node1 = "node1"
node2 = "node2"

nodeIds :: NodeIds
nodeIds = Set.fromList [node0, node1, node2]

testConfigs :: [NodeConfig]
testConfigs = [testConfig0, testConfig1, testConfig2]

msToMicroS :: Num n => n -> n
msToMicroS = (1000 *)

pairMsToMicroS :: Num n => (n, n) -> (n, n)
pairMsToMicroS = bimap msToMicroS msToMicroS

testConfig0, testConfig1, testConfig2 :: NodeConfig
testConfig0 = NodeConfig
  { configNodeId           = node0
  , configNodeIds          = nodeIds
  , configHeartbeatTimeout = msToMicroS 50
  }
testConfig1 = NodeConfig
  { configNodeId           = node1
  , configNodeIds          = nodeIds
  , configHeartbeatTimeout = msToMicroS 50
  }
testConfig2 = NodeConfig
  { configNodeId           = node2
  , configNodeIds          = nodeIds
  , configHeartbeatTimeout = msToMicroS 50
  }
