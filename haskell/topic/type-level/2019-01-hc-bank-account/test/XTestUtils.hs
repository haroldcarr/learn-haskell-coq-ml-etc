{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module XTestUtils where

import           X0
------------------------------------------------------------------------------
import qualified Data.Map.Merge.Lazy as Merge
import qualified Data.Set            as Set
import qualified Prelude
import           Protolude

isXLoggedIn :: XNodeState v -> Bool
isXLoggedIn (XNodeState rns) = isLoggedIn rns

isXCandidate :: XNodeState v -> Bool
isXCandidate (XNodeState rns) = isCandidate rns

isXLoggedOut :: XNodeState v -> Bool
isXLoggedOut (XNodeState rns) = isLoggedOut rns

node0, node1, node2 :: NodeId
node0 = "node0"
node1 = "node1"
node2 = "node2"

client0 :: ClientId
client0 = ClientId "client0"

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

-- | Zip maps using function. Throws away items left and right
zipMapWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
zipMapWith f = Merge.merge Merge.dropMissing Merge.dropMissing (Merge.zipWithMatched (const f))

-- | Perform an inner join on maps (hence throws away items left and right)
combine :: Ord a => Map a b -> Map a c -> Map a (b, c)
combine = zipMapWith (,)

printIfNode :: (Show nId, Eq nId) => nId -> nId -> Prelude.String -> IO ()
printIfNode nId nId' msg =
  when (nId == nId') $
    print $ show nId ++ " " ++ msg

printIfNodes :: (Show nId, Eq nId) => [nId] -> nId -> Prelude.String -> IO ()
printIfNodes nIds nId' msg =
  when (nId' `elem` nIds) $
    print $ show nId' ++ " " ++ msg
