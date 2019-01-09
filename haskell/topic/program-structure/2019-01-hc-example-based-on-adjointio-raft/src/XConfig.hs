{-# LANGUAGE NoImplicitPrelude #-}

module XConfig where

import           XTypes
------------------------------------------------------------------------------
import           Numeric.Natural (Natural)
import           Protolude

data NodeConfig = NodeConfig
  { configNodeId           :: NodeId  -- dummy
  , configNodeIds          :: NodeIds
  , configHeartbeatTimeout :: Natural
  } deriving (Show)

