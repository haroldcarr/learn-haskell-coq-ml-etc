{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Raft where

import           Control.Lens
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Aeson               as JS
import qualified Data.ByteString          as BS
import qualified Data.Thyme.Clock         as Time
import           GHC.Generics
import qualified System.Random            as SR

data RaftSpec m a = RaftSpec
  { _sendMessage  :: Int -> BS.ByteString -> m ()
  , _debugPrint   :: Int -> String        -> m ()
  , _getTimestamp :: m Time.UTCTime
  , _random       :: forall b . SR.Random b => (b, b) -> m b
  }
makeLenses ''RaftSpec

data RaftState a = RaftState
  { _timeSinceLastAER :: Int
  , _numTimeouts      :: Int
  }
makeLenses ''RaftState

type Raft m a = RWS.RWST (RaftEnv m a) () (RaftState a) m

data RaftEnv m a = RaftEnv
  { _clusterSize :: Int
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
  { _timeSinceLastAER = 0
  , _numTimeouts      = 0
  }

mkRaftEnv :: RWS.MonadIO m => RaftSpec (Raft m a) a -> RaftEnv m a
mkRaftEnv rs' = RaftEnv
  { _clusterSize = 4
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
  (e^.rs.debugPrint) (s^.numTimeouts) (show (e^.clusterSize))
  return ()

type ShardId = Int

data X = X { foo :: Int } deriving (Eq,Ord,Read,Show,Generic)
instance JS.ToJSON   X
instance JS.FromJSON X

data NodeID = NodeID { _host :: !String, _port :: !Int, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Generic)
instance Show NodeID where
  show = ("NodeID " ++) . _fullAddr
$(makeLenses ''NodeID)
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
$(makeLenses ''RequestId)
instance Show RequestId where
  show (RequestId (Nonce n) sid) =
    concat ["RqId=", "s:", show sid, ",", "n:", show n]
instance JS.ToJSON   RequestId
instance JS.FromJSON RequestId
