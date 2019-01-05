{-# LANGUAGE NoImplicitPrelude #-}

module XAction where

import XEvent
import XRPC
------------------------------------------------------------------------------
import Protolude

data Action sm v
  = SendRPC (SendRPCAction v)
  | ResetTimeoutTimer Timeout
  deriving Show

data SendRPCAction v
  = SendEnterUsernamePassword    EnterUsernamePassword
  | SendInvalidUsernamePassword  InvalidUsernamePassword
  | SendEnterPin                 EnterPin
  | SendInvalidPin               InvalidPin
  | SendEnterAccountNumberOrQuit EnterAcctNumOrQuit
  | SendInvalidAccountNumber     InvalidAcctNum
  | SendAccountBalance           (AcctBalance v)
  deriving Show
