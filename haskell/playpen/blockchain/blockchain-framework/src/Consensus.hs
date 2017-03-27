{-# LANGUAGE DeriveGeneric #-}

module Consensus
  ( AppendEntry (..)
  , AppendEntryResponse (..)
  )
where

import           Blockchain   (Block)

import           Data.Aeson
import           GHC.Generics

data AppendEntry =
  AppendEntry { appendEntry :: Block
              }
  deriving (Eq, Generic, Show)

data AppendEntryResponse =
  AppendEntryResponse { appendEntryResponse :: Bool
                      , block               :: Maybe Block
                      }
  deriving (Eq, Generic, Show)

instance ToJSON   AppendEntry
instance FromJSON AppendEntry
instance ToJSON   AppendEntryResponse
instance FromJSON AppendEntryResponse

