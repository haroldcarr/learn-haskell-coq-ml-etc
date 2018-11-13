{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Control.Lens
import qualified Data.Aeson   as JS
import           GHC.Generics

type ShardId = Int

newtype X = X { foo :: Int } deriving (Eq,Ord,Read,Show,Generic)
instance JS.ToJSON   X
instance JS.FromJSON X

data NodeID = NodeID { _host :: !String, _port :: !Int, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Generic)
$(makeLenses ''NodeID)
instance Show NodeID where
  show = ("NodeID " ++) . _fullAddr
instance JS.ToJSON NodeID where
  toJSON = JS.genericToJSON JS.defaultOptions { JS.fieldLabelModifier = drop 1 }
instance JS.ToJSONKey NodeID
instance JS.FromJSON NodeID where
  parseJSON = JS.genericParseJSON JS.defaultOptions { JS.fieldLabelModifier = drop 1 }
instance JS.FromJSONKey NodeID

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord, Generic)
instance JS.ToJSON   Term
instance JS.FromJSON Term

startTerm :: Term
startTerm = Term (-1)

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic)
instance JS.ToJSON LogIndex
instance JS.FromJSON LogIndex

newtype Nonce = Nonce Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic)
instance JS.ToJSON   Nonce
instance JS.FromJSON Nonce

data RequestId = RequestId
  { _ridNonce :: Nonce
  , _ridShard :: ShardId
  } deriving (Read, Eq, Ord, Generic)
instance Show RequestId where
  show (RequestId (Nonce n) sid) =
    concat ["RqId=", "s:", show sid, ",", "n:", show n]
instance JS.ToJSON   RequestId
instance JS.FromJSON RequestId
