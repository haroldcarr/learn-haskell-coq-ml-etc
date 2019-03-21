{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Lib where

import           Control.Concurrent
import           Control.Lens
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Serialize     (Serialize)
import qualified Data.Serialize     as S
import           Data.Word          (Word64)
import           GHC.Generics
import           GHC.Int            (Int64)

newtype Being = Being
  { _age :: Double }

data Person = Person
  { _personBeing :: Being
  , _name        :: String }

data Worker = Worker
  { _workerPerson :: Person
  , _job          :: String  }

makeClassy ''Being
makeClassy ''Person
makeClassy ''Worker

instance HasBeing Person where being = personBeing
instance HasPerson Worker where person = workerPerson

instance HasBeing Worker where being = person.being

------------------------------------------------------------------------------
type ShardId = Int64
type MsgNum  = Int
type Port    = Word64

data Role
  = Follower
  | Candidate
  | Leader
  | Inactive
  deriving (Show, Generic, Eq)

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord, Generic, Serialize)

data NodeId = NodeId { _host :: !String, _port :: !Port, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Generic,Show)

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic, Serialize)

data RaftState a = RaftState
  { _nodeRole         :: !Role
  , _term             :: !Term
  , _currentLeader    :: !(Maybe NodeId)
  , _ignoreLeader     :: !Bool
  , _commitIndex      :: !LogIndex
  , _timerThread      :: !(Maybe ThreadId)
  , _timeSinceLastAER :: !Int
  , _lNextIndex       :: !(Map NodeId LogIndex)
  , _lMatchIndex      :: !(Map NodeId LogIndex)
  , _lRATS            :: !a
  , _shardLeader      :: !(Map ShardId NodeId)
  , _numTimeouts      :: !Int
  , _nextMsgNum       :: !MsgNum
  , _logSenderSend    :: !Bool
  } deriving Show
makeClassy ''RaftState

class HasNodeRole a where
  getNodeRole :: a -> Role
instance HasNodeRole (RaftState a) where
  getNodeRole s = s^.nodeRole

------------------------------------------------------------------------------

someFunc :: IO ()
someFunc = do
  a
  b

a :: IO ()
a  = do
  let b = Being 23
      p = Person (Being 24) "HC"
      w = Worker (Person (Being 25) "FCW") "Acadmic"
  foo b p w

foo :: (HasBeing b, HasBeing p, HasBeing w)
    => b -> p -> w
    -> IO ()
foo b p w = print (b^.age, p^.age, w^.age)

b :: IO ()
b  = do
  let r = RaftState
            Follower
            (Term 0)
            Nothing
            False
            (LogIndex 0)
            Nothing
            0
            Map.empty
            Map.empty
            3
            Map.empty
            0
            0
            True
  print r
  bar r

bar :: HasNodeRole r
    => r
    -> IO ()
bar r = print (getNodeRole r)
