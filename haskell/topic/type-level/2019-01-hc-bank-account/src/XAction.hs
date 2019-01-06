{-# LANGUAGE NoImplicitPrelude #-}

module XAction where

import XEvent
import XRPC
import XTypes
------------------------------------------------------------------------------
import Protolude

data Action sm v
  = SendRPC NodeId (SendRPCAction v)
  | ResetTimeoutTimer Timeout
  deriving Show

data SendRPCAction v
  = SendEnterUsernamePassword   EnterUsernamePassword
  | SendInvalidUsernamePassword InvalidUsernamePassword
  | SendEnterPin                EnterPin
  | SendInvalidPin              InvalidPin
  | SendEnterAcctNumOrQuit      EnterAcctNumOrQuit
  | SendInvalidAccountNum       InvalidAcctNum
  | SendAccountBalance          (AcctBalance v)
  deriving Show
