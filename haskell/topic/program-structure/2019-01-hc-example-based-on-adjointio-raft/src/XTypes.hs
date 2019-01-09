{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}

module XTypes where

import           Protolude

type NodeId  = ByteString
type NodeIds = Set NodeId

newtype ClientId = ClientId NodeId
  deriving (Show, Eq, Ord)
