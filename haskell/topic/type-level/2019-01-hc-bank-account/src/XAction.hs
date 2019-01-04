{-# LANGUAGE NoImplicitPrelude #-}

module XAction where

import XEvent
import XTypes
------------------------------------------------------------------------------
import Protolude

data Action sm v
  = SendRPC NodeId (SendRPCAction v)
  | ResetTimeoutTimer Timeout
  deriving Show

newtype SendRPCAction v
  = StartTwoFactorAuthN v
  deriving Show
