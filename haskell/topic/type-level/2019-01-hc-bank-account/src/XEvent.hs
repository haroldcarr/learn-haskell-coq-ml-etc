{-# LANGUAGE NoImplicitPrelude #-}

module XEvent where

import           XRPC
------------------------------------------------------------------------------
import           Protolude

data Event v
  = MessageEvent (MessageEvent v)
  | TimeoutEvent Timeout
  deriving Show

data Timeout
  = HeartbeatTimeout
  deriving Show

newtype MessageEvent v
  = RPCMessageEvent (RPCMessage v)
  deriving Show
