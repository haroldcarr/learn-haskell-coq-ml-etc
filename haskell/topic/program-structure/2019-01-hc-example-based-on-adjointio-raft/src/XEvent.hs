{-# LANGUAGE NoImplicitPrelude #-}

module XEvent where

import           XClient
------------------------------------------------------------------------------
import           Protolude

data Event v
  = MessageEvent (MessageEvent v)
  | TimeoutEvent Timeout
  deriving Show

data Timeout
  = HeartbeatTimeout
  deriving (Eq, Show)

newtype MessageEvent v
  = ClientRequestEvent (ClientRequest v)
  deriving Show
