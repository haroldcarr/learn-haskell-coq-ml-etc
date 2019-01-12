{-# LANGUAGE NoImplicitPrelude #-}

module XEvent where

import           XClient
import           XRPC
------------------------------------------------------------------------------
import           Protolude

data Event v
  = MessageEvent (MessageEvent v)
  | TimeoutEvent Timeout
  deriving Show

data Timeout
  = HeartbeatTimeout
  deriving (Eq, Show)

data MessageEvent v
  = RPCMessageEvent    (RPCMessage    v) -- incoming internode messages
  | ClientRequestEvent (ClientRequest v)
  deriving Show
