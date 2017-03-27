{-# LANGUAGE DeriveGeneric #-}

module Consensus
  ( AppendEntry (..)
  , AppendEntryResponse (..)
  )
where

import           Blockchain   (Block (Block))

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
