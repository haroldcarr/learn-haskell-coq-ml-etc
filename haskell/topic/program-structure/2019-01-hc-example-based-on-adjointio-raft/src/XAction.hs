{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module XAction where

import           XClient
import           XEvent
import           XTypes
------------------------------------------------------------------------------
import           Protolude

data Action sm v
  = SendRPC           NodeId    (SendRPCAction     v) -- outgoing internode messages
  | SendToClient      ClientId  (ClientResponse sm v)
  | ResetTimeoutTimer Timeout
  deriving (Eq, Show)

data SendRPCAction v
  deriving (Eq, Show)
