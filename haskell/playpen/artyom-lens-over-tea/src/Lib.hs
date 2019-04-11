{-# LANGUAGE ConstraintKinds            #-}
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
makeLenses ''RaftState

class RNodeRole a where
  rNodeRole :: a -> Role
instance RNodeRole (RaftState a) where
  rNodeRole s = s^.nodeRole
class WNodeRole a where
  wNodeRole :: Role -> a -> a
instance WNodeRole (RaftState a) where
  wNodeRole r s = s {_nodeRole = r}

wNodeRole' :: (RNodeRole a, WNodeRole a, MonadState a m) => Role -> m ()
wNodeRole' r = get >>= put . wNodeRole r

class RTerm a where
  rTerm :: a -> Term
instance RTerm (RaftState a) where
  rTerm s = s^.term
class RTerm a => WTerm a where
  wTerm :: Term -> a -> a
instance WTerm (RaftState a) where
  wTerm t s = s {_term = t}

wTerm' :: (RTerm t, WTerm t, MonadState t m) => Term -> m ()
wTerm' t = get >>= put . wTerm t

type RNodeRoleRWTerm x = (RNodeRole x, RTerm x, WTerm x)

------------------------------------------------------------------------------

someFunc :: IO ()
someFunc = do
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
  -- (((Role, Term, RaftState Int), [String]), RaftState Int)
  (((r, t, s0), msgs), s1) <- runStateT (runReaderT (runWriterT xxx) r) r
  print r
  print t
  print s0
  print msgs
  print s1

bar :: RNodeRole r => r -> IO ()
bar r = do
  putStrLn "\n"
  print (rNodeRole r)

xxx
  ::
  -- (RNodeRole r, RTerm t, WTerm t, MonadWriter [String] m, MonadReader r m, MonadState t m)
     (RNodeRoleRWTerm x            , MonadWriter [String] m, MonadReader x m, MonadState x m)
  => m (Role, Term, x)
xxx = do
  r <- asks rNodeRole
  tell [show r]
  t <- gets rTerm
  tell [show t]
  s <- get
  put $ wTerm (t + 1) s
  t' <- gets rTerm
  tell [show t']
  wTerm' (t' + 1)
  t'' <- gets rTerm
  tell [show t'']
  return (r, t, s)

