{-# LANGUAGE LambdaCase #-}

module RetrofitDebug where

------------------------------------------------------------------------------
import qualified Raft                       as R
import qualified Types                      as R
------------------------------------------------------------------------------
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.RWS.Strict (unless)
import qualified Data.ByteString.Char8               as BS
import Data.List (intercalate)
import System.Exit (ExitCode (ExitFailure))
import System.IO (hFlush, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (exitImmediately)

{-# ANN module "HLint: ignore Use camelCase" #-}

data DebugInfo
  = DbgNetwork     DI_Network
  | DbgConsensus   DI_Consensus
  | DbgRecovery    DI_Recovery
  | DbgApplication [String]
  | DbgError       [String]
  | DbgUnexpected  [String]
  | DbgInfo        [String]
  deriving (Eq , Show)

fields :: [String] -> String
fields = intercalate "; "

-- Collects and returns info. Does not print.
debugS :: Monad m => DebugInfo -> R.Raft m a (R.Role, R.NodeID, String)
debugS k = do
  nid <- view (R.cfg.R.nodeId)
  role' <- use R.nodeRole
  let cr = coloredRole role'
  return (role', nid, fields (cr : pp k))
  where
    coloredRole :: R.Role -> String
    coloredRole = \case
      R.Leader    -> "leader"
      R.Candidate -> "candidate"
      R.Follower  -> "follower"

-- Supports higher-level functions below.
debug :: Monad m => DebugInfo -> R.Raft m a ()
debug k = do
  dbg <- view (R.rs.R.debugPrint)
  (role', nid, flds)  <- debugS k
  dontDebugFollower' <- view (R.cfg.R.dontDebugFollower)
  case role' of
    R.Leader    -> dbg nid flds
    R.Candidate -> dbg nid flds
    R.Follower  -> unless dontDebugFollower' (dbg nid flds)

debugInRaft :: (Monad m) => DebugInfo -> R.Raft m a String
debugInRaft d = do
  debug d
  (_, nid, flds) <- debugS d
  return $ show nid ++ "; " ++ flds

debugPure :: DebugInfo -> String
debugPure = fields . pp

debugInfo  :: Monad m => [String] -> R.Raft m a ()
debugInfo   = void . debugInRaft . DbgInfo
debugInfoS ::              [String] ->          String
debugInfoS  = debugPure   . DbgInfo

logSend
  :: Monad m
  => R.NodeID -> R.MsgNum -> (R.NodeID, R.RPC)
  -> R.Raft m a ()
logSend myNodeId msgNum (target, rpc) = do
  shouldLog <- use R.logSenderSend
  when shouldLog $
    debugInfo [ "SEND"
              , R.rpcCategory rpc
              , R.mkMsgId myNodeId msgNum (R.getMsgId rpc)
              , R._fullAddr target
              , R.logShow rpc
              ]

logRec
  :: (String -> IO ())
  -> R.RPC
  -> IO ()
logRec dbg rpc =
  dbg $ fields
    [ "REC"
    , R.rpcCategory rpc
    , R.getMsgId rpc
    , R.logShow rpc
    ]

dHandleEnter, dHandleExit
  :: Monad m
  => R.RPC
  -> R.Raft m a ()
dHandleEnter = dbgH "Enter"
dHandleExit  = dbgH "Exit"
dbgH
  :: Monad m
  => String -> R.RPC
  -> R.Raft m a ()
dbgH enterOrExit rpc =
  debugInfo $
      enterOrExit
    : R.rpcCategory rpc
    : R.getMsgId rpc
    : [R.logShow rpc | enterOrExit == "Enter"]

errorExit :: [String] -> a
errorExit ss = unsafePerformIO $ do
  putStrLn ("ERROR: " ++ fields ss)
  hFlush stdout
  hFlush stderr
  exitImmediately (ExitFailure (-1))
  error "" -- so it can match any type

maybeErrorExit :: [String] -> Maybe a -> a
maybeErrorExit s Nothing  = errorExit s
maybeErrorExit _ (Just a) = a

eitherErrorExit :: Show e => [String] -> Either e a -> a
eitherErrorExit s (Left e)  = errorExit (s ++ [show e])
eitherErrorExit _ (Right a) = a

debugUnexpected   :: (Monad m) => [String] -> R.Raft m a String
debugUnexpected    = debugInRaft . DbgUnexpected
debugUnexpectedS  ::              [String] ->          String
debugUnexpectedS   = debugPure   . DbgUnexpected

data DI_Network
  = DbgPacketSent R.NodeID String
  | DbgPacketRcvd R.NodeID String
  deriving (Eq , Show)

debugNetwork :: (Monad m) => DI_Network -> R.Raft m a ()
debugNetwork = debug . DbgNetwork

data DI_Consensus
  = DbgRoleChange R.Role
  | DbgTermChange R.Term
  | DbgLbl R.MsgType String
  | DbgLogIndex R.LogIndex
  | DbgCommitIndex R.LogIndex
  | DbgHash String BS.ByteString
  | DbgCommandsCompleted Int
  | DbgResultSent
  | DbgNotEnoughEvidence Int [R.NodeID]
  deriving (Eq , Show)

debugRaft :: (Monad m) => DI_Consensus -> R.Raft m a ()
debugRaft = debug . DbgConsensus

debugRaftHandler :: (Monad m) => R.MsgType -> String -> R.Raft m a ()
debugRaftHandler m = debugRaft . DbgLbl m

data DI_Recovery
  = DbgRecTarget R.LogIndex
  | DbgRecovering
  | DbgDoneRecovering
  | DbgFailedRec String
  | DbgCatchingUp
  | DbgActive
  | DbgInactive
  | DbgCkptRequest R.NodeID R.LogIndex (Maybe Int)
  | DbgCkptResponse R.NodeID (Maybe Int) [String]
  | DbgOpMode R.OperationMode [String]
  | DbgOther [String]
  deriving (Eq , Show)

debugRecovery :: (Monad m) => DI_Recovery -> R.Raft m a ()
debugRecovery = debug . DbgRecovery

class PP a where
  pp :: a -> [String]

instance PP DebugInfo where
  pp (DbgNetwork     d) = "network"   : pp d
  pp (DbgConsensus   d) = "consensus" : pp d
  pp (DbgRecovery    d) = "recovery"  : pp d
  pp (DbgApplication d) = "app"       : d
  pp (DbgError       d) = "ERROR"     : d
  pp (DbgUnexpected  d) = "UNEXPECTED": d
  pp (DbgInfo        d) = "info"      : d

instance PP DI_Recovery where
  pp (DbgRecTarget li) = "tgt" : pp li
  pp DbgRecovering     = ["recovering"]
  pp DbgDoneRecovering = ["done-recovering"]
  pp DbgCatchingUp     = ["catching-up"]
  pp DbgActive         = ["active"]
  pp DbgInactive       = ["inactive"]
  pp (DbgFailedRec er) = ["failed", er]
  pp (DbgOpMode om ss) = "opmode" : show om : ss
  pp (DbgOther strs)   = "other" : strs
  pp (DbgCkptRequest  ni li pgi) = "ckpt-request"  : show pgi : (pp ni ++ pp li)
  pp (DbgCkptResponse ni pgi st) = "ckpt-response" : show pgi : (pp ni ++ st)

instance PP DI_Consensus where
  pp (DbgRoleChange role) = ["role-change", show role]
  pp (DbgTermChange t)    = "term-change" : pp t
  pp (DbgLbl msg str)     = [show msg, str]
  pp (DbgLogIndex li)     = "log-index" : pp li
  pp (DbgCommitIndex ci)  = "commit-index" : pp ci
  pp (DbgHash t h)          = t : pp h
  pp DbgResultSent        = ["sent-results"]
  pp (DbgCommandsCompleted i) = ["commands", show i]
  pp (DbgNotEnoughEvidence need slackers)
    = "not-enough-evidence" : "missing" : show need : "slackers" : map nodeidPP slackers


instance PP DI_Network where
  pp (DbgPacketSent ni packet) = "sent" : pp ni ++ [ packet ]
  pp (DbgPacketRcvd ni packet) = "rcvd" : pp ni ++ [ packet ]

nodeidPP :: R.NodeID -> String
nodeidPP nid =
  let nport    = R._port nid
      shardno  = show $ (nport - 10000) `mod` 4
      serverno = show $ (nport - 10000) `div` 4
  in "server:" ++ shardno ++ "/" ++ serverno ++ "/" ++ show nport

instance PP R.NodeID where
  pp = (:[]) . nodeidPP

instance PP R.Term where
  pp (R.Term t) = [show t]

logindexPP :: R.LogIndex -> String
logindexPP (R.LogIndex li) = show li

instance PP R.LogIndex where
  pp = (:[]) . logindexPP

instance PP BS.ByteString where
  pp = (:[]) . show
