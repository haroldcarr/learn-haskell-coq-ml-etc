{-# LANGUAGE DeriveGeneric      #-}

module XEvent where

import           XRPC
------------------------------------------------------------------------------
import           Protolude

-- | Events a node can send and receive.
data Event v
  = MessageEvent (MessageEvent v)
  | TimeoutEvent Timeout
  deriving (Show)

data Timeout
  = HeartbeatTimeout
  deriving (Show)

newtype MessageEvent v
  = RPCMessageEvent (RPCMessage v)       -- ^ Incoming event from a peer
  deriving (Show, Generic)
