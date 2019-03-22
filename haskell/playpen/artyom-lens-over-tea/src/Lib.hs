{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

module Lib where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer.Strict
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Serialize              (Serialize)
import qualified Data.Serialize              as S
import           Data.Word                   (Word64)
import           GHC.Generics
import           GHC.Int                     (Int64)

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

class RNodeRole a where
  rNodeRole :: a -> Role
instance RNodeRole (RaftState a) where
  rNodeRole s = s^.nodeRole

class RTerm a where
  rTerm :: a -> Term
instance RTerm (RaftState a) where
  rTerm s = s^.term
class WTerm a where
  wTerm :: Term -> a -> a
instance WTerm (RaftState a) where
  wTerm a s = s {_term = a}

------------------------------------------------------------------------------

someFunc :: IO ()
someFunc = do
  let b = Being 23
      p = Person (Being 24) "HC"
      w = Worker (Person (Being 25) "FCW") "Acadmic"
  foo b p w
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
            (3::Int)
            Map.empty
            0
            0
            True
  bar r
  x <- runStateT (runReaderT (runWriterT xxx) r) r
  print x

foo :: (HasBeing b, HasBeing p, HasBeing w)
    => b -> p -> w
    -> IO ()
foo b p w = do
  putStrLn "\n"
  print (b^.age, p^.age, w^.age)

bar :: RNodeRole r
    => r
    -> IO ()
bar r = do
  putStrLn "\n"
  print (rNodeRole r)

xxx
  :: (RNodeRole r, RTerm t, WTerm t, MonadWriter [String] m, MonadReader r m, MonadState t m)
  => m (Role, Term, t)
xxx = do
  r <- asks rNodeRole
  tell [show r]
  t <- gets rTerm
  tell [show t]
  x <- get
  let x' = wTerm (Term 45) x
  put x'
  return (r, t, x)

