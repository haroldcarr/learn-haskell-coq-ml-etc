{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Raft where

import           Control.Lens
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.ByteString          as BS
import qualified Data.Thyme.Clock         as Time
import qualified System.Random            as SR
------------------------------------------------------------------------------
import           Types

data RaftSpec m a = RaftSpec
  { _sendMessage  :: NodeID -> BS.ByteString -> m ()
  , _debugPrint   :: NodeID -> String        -> m ()
  , _getTimestamp :: m Time.UTCTime
  , _random       :: forall b . SR.Random b => (b, b) -> m b
  }
makeLenses ''RaftSpec

data RaftState a = RaftState
  { _nodeRole         :: Role
  , _timeSinceLastAER :: Int
  , _numTimeouts      :: Int
  , _logSenderSend    :: Bool
  }
makeLenses ''RaftState

type Raft m a = RWS.RWST (RaftEnv m a) () (RaftState a) m

data RaftEnv m a = RaftEnv
  { _cfg         :: Config
  , _clusterSize :: Int
  , _quorumSize  :: Int
  , _rs          :: RaftSpec (Raft m a) a
  }
makeLenses ''RaftEnv

mkRaftSpec :: RWS.MonadIO m => RaftSpec m a
mkRaftSpec = RaftSpec
  { _sendMessage  = \i bs -> RWS.liftIO (print i) >> RWS.liftIO (print bs)
  , _debugPrint   = \i  s -> RWS.liftIO (print i) >> RWS.liftIO (print  s)
  , _getTimestamp = RWS.liftIO Time.getCurrentTime
  , _random       = RWS.liftIO . SR.randomRIO
  }

mkRaftState :: RaftState a
mkRaftState = RaftState
  { _nodeRole = Follower
  , _timeSinceLastAER = 0
  , _numTimeouts      = 0
  , _logSenderSend    = True
  }

mkRaftEnv :: RWS.MonadIO m => RaftSpec (Raft m a) a -> RaftEnv m a
mkRaftEnv rs' = RaftEnv
  { _cfg = Config fAKENID True True
  , _clusterSize = 4
  , _quorumSize  = 3
  , _rs          = rs'
  }

mkSpecStateEnv :: RWS.MonadIO m => (RaftState a, RaftEnv m a)
mkSpecStateEnv =
  let spec   = mkRaftSpec
      env    = mkRaftEnv spec
      state' = mkRaftState
   in (state', env)

server :: Raft IO a ()
server = do
  e <- RWS.ask
  s <- RWS.get
  (e^.rs.debugPrint) fAKENID (show (s^.numTimeouts))
  (e^.rs.debugPrint) fAKENID (show (e^.clusterSize))
  return ()

run :: IO ()
run =
  let (s, e) = mkSpecStateEnv
   in RWS.void $ RWS.evalRWST server e s

fAKENID :: NodeID
fAKENID = NodeID "fakehost" 8080 "http://fakehost:8080"
