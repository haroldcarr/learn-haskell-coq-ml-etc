{-# LANGUAGE NoImplicitPrelude #-}

module XAction where

import           XClient
import           XEvent
import           XTypes
------------------------------------------------------------------------------
import           Protolude

data Action sm v
  = SendToClient      ClientId  (ClientResponse sm v)
  | ResetTimeoutTimer Timeout
  deriving (Eq, Show)
