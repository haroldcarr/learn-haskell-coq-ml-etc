{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module XRPC where

import           Protolude

import           XTypes

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
  = EnterUsernamePasswordRPC   EnterUsernamePassword
  | UsernamePasswordRPC        UsernamePassword
  | InvalidUserNamePasswordRPC InvalidUsernamePassword
  | EnterPinRPC                EnterPin
  | PinRPC                     Pin
  | InvalidPinRPC              InvalidPin
  | EnterAcctNumOrQuitRPC      EnterAcctNumOrQuit
  | AcctNumOrQuitRPC           AcctNumOrQuit
  | InvalidAcctNumRPC          InvalidAcctNum
  | AcctBalanceRPC             (AcctBalance v)
  | QuitRPC                    Quit
  deriving Show

class RPCType a v where
  toRPC :: a -> RPC v

instance RPCType EnterUsernamePassword   v where toRPC = EnterUsernamePasswordRPC
instance RPCType UsernamePassword        v where toRPC = UsernamePasswordRPC
instance RPCType InvalidUsernamePassword v where toRPC = InvalidUserNamePasswordRPC
instance RPCType EnterPin                v where toRPC = EnterPinRPC
instance RPCType Pin                     v where toRPC = PinRPC
instance RPCType InvalidPin              v where toRPC = InvalidPinRPC
instance RPCType EnterAcctNumOrQuit      v where toRPC = EnterAcctNumOrQuitRPC
instance RPCType AcctNumOrQuit           v where toRPC = AcctNumOrQuitRPC
instance RPCType InvalidAcctNum          v where toRPC = InvalidAcctNumRPC
instance RPCType (AcctBalance v)         v where toRPC = AcctBalanceRPC
instance RPCType Quit                    v where toRPC = QuitRPC

data EnterUsernamePassword = EnterUsernamePassword
  deriving Show

data UsernamePassword = UsernamePassword
  { upUsername :: Text
  , upPassword :: Text
  } deriving Show

data InvalidUsernamePassword = InvalidUsernamePassword
  deriving Show

data EnterPin = EnterPin
  deriving Show

newtype Pin = Pin
  { pPin :: Text
  } deriving Show

data InvalidPin = InvalidPin
  deriving Show

data EnterAcctNumOrQuit = EnterAcctNumOrQuit
  deriving Show

newtype AcctNumOrQuit = AcctNumOrQuit
  { anoqAcctNum :: Text
  } deriving Show

data InvalidAcctNum = InvalidAcctNum
  deriving Show

data AcctBalance v = AcctBalance
  { abBalance :: Int
  , abV       :: v
  } deriving Show

data Quit = Quit
  deriving Show

