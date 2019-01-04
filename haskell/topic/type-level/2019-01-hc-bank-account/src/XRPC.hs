{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module XRPC where

import           Protolude

import           XTypes

-- | Interface for nodes to send messages to one another.
-- E.g. Control.Concurrent.Chan, Network.Socket, etc.
class XSendRPC m v where
  sendRPC :: NodeId -> RPCMessage v -> m ()

-- | Interface for nodes to receive messages from one another.
class Show (XRecvRPCError m v) => XRecvRPC m v where
  type XRecvRPCError m v
  receiveRPC :: m (Either (XRecvRPCError m v) (RPCMessage v))

-- | Representation of a message sent between nodes.
data RPCMessage v = RPCMessage
  { sender :: NodeId
  , rpc    :: RPC v
  } deriving Show

newtype RPC v
  = UsernamePasswordRPC (UsernamePassword v)
  deriving Show

class RPCType a v where
  toRPC :: a -> RPC v

instance RPCType (UsernamePassword v) v where
  toRPC = UsernamePasswordRPC

newtype UsernamePassword v = UsernamePassword
  { upUP :: v
  }
  deriving (Show)
