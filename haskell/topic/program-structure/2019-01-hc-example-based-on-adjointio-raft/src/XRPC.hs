{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module XRPC where

import           XTypes
------------------------------------------------------------------------------
import           Protolude

-- NOT USED.

-- | For nodes to send messages to one another.
-- E.g. Control.Concurrent.Chan, Network.Socket, etc.
class XSendRPC m v where
  sendRPC :: NodeId -> RPCMessage v -> m ()

-- | For nodes to receive messages from one another.
class Show (XRecvRPCError m v) => XRecvRPC m v where
  type XRecvRPCError m v
  receiveRPC :: m (Either (XRecvRPCError m v) (RPCMessage v))

-- | Representation of a message sent between nodes.
data RPCMessage v = RPCMessage
  { sender :: NodeId
  , rpc    :: RPC v
  } deriving Show

data RPC v
  deriving Show

class RPCType a v where
  toRPC :: a -> RPC v
