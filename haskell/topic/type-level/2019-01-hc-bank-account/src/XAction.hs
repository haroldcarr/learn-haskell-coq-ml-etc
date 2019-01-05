{-# LANGUAGE NoImplicitPrelude #-}

module XAction where

import XEvent
------------------------------------------------------------------------------
import Protolude

data Action sm v
  = ClientAction (ClientAction v)
  | ResetTimeoutTimer Timeout
  deriving Show

data ClientAction v
  = EnterUsernamePassword
  | InvalidUsernamePassword
  | EnterPin
  | InvalidPin
  | EnterAccountNumberOrQuit
  | InvalidAccountNumber
  | AccountBalance Int
  deriving Show
