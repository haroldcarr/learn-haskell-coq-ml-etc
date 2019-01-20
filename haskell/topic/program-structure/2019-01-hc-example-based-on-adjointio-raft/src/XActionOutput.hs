{-# LANGUAGE NoImplicitPrelude #-}

module XActionOutput where

import           XClient
import           XEventInput
import           XTypes
------------------------------------------------------------------------------
import           Protolude

data Action sm v
  = SendToClient      ClientId  (ClientResponse sm v)
  | ResetTimeoutTimer Timeout
  deriving (Eq, Show)
