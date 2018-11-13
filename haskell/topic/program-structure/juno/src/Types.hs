{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Control.Lens
import qualified Data.Aeson      as JS
import qualified Data.ByteString as BS
import           Data.List       (intercalate)
import           Data.Serialize  (Serialize)
import           Data.Word       (Word64)
import           GHC.Generics
import           GHC.Int         (Int64)

--- Base.hs

type ShardId = Int64

data NodeID = NodeID { _host :: !String, _port :: !Word64, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Generic)
instance Show NodeID where
  show = ("NodeID " ++) . _fullAddr
$(makeLenses ''NodeID)
instance Serialize NodeID
instance JS.ToJSON NodeID where
  toJSON = JS.genericToJSON JS.defaultOptions { JS.fieldLabelModifier = drop 1 }
instance JS.ToJSONKey NodeID
instance JS.FromJSON NodeID where
  parseJSON = JS.genericParseJSON JS.defaultOptions { JS.fieldLabelModifier = drop 1 }
instance JS.FromJSONKey NodeID

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord, Generic, Serialize)
instance JS.ToJSON Term
instance JS.FromJSON Term

startTerm :: Term
startTerm = Term (-1)

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic, Serialize)
instance JS.ToJSON LogIndex
instance JS.FromJSON LogIndex

newtype Nonce = Nonce Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic, Serialize)
instance JS.ToJSON   Nonce
instance JS.FromJSON Nonce

data RequestId = RequestId
  { _ridNonce :: Nonce
  , _ridShard :: ShardId
  } deriving (Read, Eq, Ord, Generic)
$(makeLenses ''RequestId)
instance Serialize RequestId
instance Show RequestId where
  show (RequestId (Nonce n) sid) =
    concat ["RqId=", "s:", show sid, ",", "n:", show n]
instance JS.ToJSON   RequestId
instance JS.FromJSON RequestId

data Role = Follower
          | Candidate
          | Leader
  deriving (Show, Generic, Eq)

type MsgId = String

type MsgNum = Int

msgIdNotSet :: String
msgIdNotSet = ""

mkMsgId :: NodeID -> MsgNum -> MsgId -> MsgId
mkMsgId NodeID{..} mid s =
  _fullAddr ++ "-" ++
  show mid ++
  if null s || s == msgIdNotSet
  then ""
  else "-" ++ unspace s
  where unspace = map unspaceChar
        unspaceChar x =
          if x == ' ' then '_' else x

---------- Message.hs

data RPC = AE'   Int
         | AER'  Int
  deriving (Show, Eq, Generic)

rpcCategory :: RPC -> String
rpcCategory (AE' _)  = "AE"
rpcCategory (AER' _) = "AER"

getMsgId :: RPC -> MsgId
getMsgId (AE'  _v) = "_aeMsgId   v"
getMsgId (AER' _v) = "_aerMsgId  v"

logShow :: RPC -> String
logShow rpc =
  intercalate "; " $ rpcCategory rpc :
  case rpc of
    AE'  _r -> ["show (r^.aeTerm)"
               ,"show (r^.prevLogIndex)"
               ,"show (r^.prevLogTerm)"
               ,"show (length (r^.aeEntries))"]
    AER' _r -> ["show (r^.aerTerm)"
               ,"show (r^.aerIndex)"
               ,"show (r^.aerSuccess)"
               ,"show (r^.aerConvinced)"]

----- Signed.hs

data MsgType = AE | AER | CMD | CMDB | CMDR | CMDE | CQ | CQR | CUPMSG | MSD | REV | RV | RVR
  deriving (Show, Eq, Ord, Generic)
instance Serialize MsgType

----- Checkpoint.hs

data RecoveryData
  = VerifiedLogIndex BS.ByteString LogIndex
  | WaitingPage Int LogIndex
  deriving (Eq , Show , Generic)

data OperationMode
  = Standard
  | Recovery RecoveryData
  deriving (Eq , Show , Generic)

--- Config.hs

data Config = Config
  { _nodeId            :: !NodeID
  , _enableDebug       :: !Bool
  , _dontDebugFollower :: !Bool
  }
  deriving (Eq, Show, Ord, Generic)
$(makeLenses ''Config)

