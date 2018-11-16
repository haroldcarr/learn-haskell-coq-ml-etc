{-# LANGUAGE OverloadedStrings #-}

module RetrofitDebug where

------------------------------------------------------------------------------
import qualified Raft                       as R
import qualified Types                      as R
------------------------------------------------------------------------------
import qualified LogData                    as L
------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad              (void, join, when)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text                  as T
import           Data.Thyme
import           System.Exit                (ExitCode (ExitFailure))
import           System.IO                  (hFlush, stderr, stdout)
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Posix.Process       (exitImmediately)

{-# ANN module ("HLint: ignore Use camelCase"::String) #-}

retrofitFormat :: L.Format
retrofitFormat = L.JSONCompact

addTime :: UTCTime -> [L.LogItem] -> [L.LogItem]
addTime t ll = L.AT (T.pack (show t)) : ll

encodeLog :: L.Format -> [L.LogItem] -> BSL.ByteString
encodeLog fmt = L.encode fmt . L.LogList

addTimeEncodeLog :: UTCTime -> L.Format -> [L.LogItem] -> BSL.ByteString
addTimeEncodeLog t fmt ll = encodeLog fmt (addTime t ll)

data DebugInfo
  = DbgNetwork     DI_Network
  | DbgConsensus   DI_Consensus
  | DbgRecovery    DI_Recovery
  | DbgApplication [L.LogItem]
  | DbgError       [L.LogItem]
  | DbgUnexpected  [L.LogItem]
  | DbgInfo        [L.LogItem]
  deriving (Eq , Show)

debugS :: Monad m => DebugInfo -> R.Raft m a (R.Role, R.NodeID, [L.LogItem])
debugS k = do
  nid <- view (R.cfg.R.nodeId)
  role' <- use R.nodeRole
  let lrole = L.ROLE role'
  return (role', nid, lrole : pp k)

debug :: Monad m => DebugInfo -> R.Raft m a ()
debug k = do
  dbg <- view (R.rs.R.debugPrint)
  (role', nid, flds)  <- debugS k
  dontDebugFollower' <- view (R.cfg.R.dontDebugFollower)
  t <- join $ view (R.rs.R.getTimestamp)
  when ( role' == R.Leader    ||
         role' == R.Candidate ||
        (role' == R.Follower && not dontDebugFollower' )) $
    dbg nid (addTimeEncodeLog t retrofitFormat flds) -- HERE

debugInRaft :: Monad m => DebugInfo -> R.Raft m a [L.LogItem]
debugInRaft d = do
  debug d
  (_, nid, flds) <- debugS d
  return $ L.NID nid : flds

debugPure :: DebugInfo -> [L.LogItem]
debugPure = pp

debugInfo  :: Monad m => [L.LogItem] -> R.Raft m a ()
debugInfo   = void . debugInRaft . DbgInfo
debugInfoS ::            [L.LogItem] ->            [L.LogItem]
debugInfoS  = debugPure   . DbgInfo

logSend
  :: Monad m
  => R.NodeID -> R.MsgNum -> (R.NodeID, R.RPC)
  -> R.Raft m a ()
logSend myNodeId msgNum (target, rpc) = do
  shouldLog <- use R.logSenderSend
  when shouldLog $
    debugInfo [ L.MSG_TYPE       "SEND"
              , L.RPC_CATEGORY $ T.pack $ R.rpcCategory rpc
              , L.MSG_ID       $ T.pack $ R.mkMsgId myNodeId msgNum (R.getMsgId rpc)
              , L.TO           $ T.pack $ R._fullAddr target
              , L.RPC_TEXT     $ T.pack $ R.logShow rpc
              ]

logRec
  :: (BSL.ByteString -> IO ())
  -> R.RPC
  -> IO ()
logRec dbg rpc = do
  t <- getCurrentTime
  dbg $ addTimeEncodeLog t retrofitFormat -- HERE
    [ L.MSG_TYPE       "REC"
    , L.RPC_CATEGORY $ T.pack $ R.rpcCategory rpc
    , L.MSG_ID       $ T.pack $ R.getMsgId rpc
    , L.RPC_TEXT     $ T.pack $ R.logShow rpc
    ]

dHandleEnter, dHandleExit
  :: Monad m
  => R.RPC
  -> R.Raft m a ()
dHandleEnter = dbgH "Enter"
dHandleExit  = dbgH "Exit"
dbgH
  :: Monad m
  => T.Text -> R.RPC
  -> R.Raft m a ()
dbgH enterOrExit rpc =
  debugInfo $
      L.EorE enterOrExit
    : (L.RPC_TEXT $ T.pack $ R.rpcCategory rpc)
    : (L.MSG_ID   $ T.pack $ R.getMsgId rpc)
    : [L.RPC_TEXT $ T.pack $ R.logShow rpc | enterOrExit == "Enter"]

errorExit :: [L.LogItem] -> a
errorExit ss = unsafePerformIO $ do
  putStrLn ("ERROR: " ++ show ss) -- TODO
  hFlush stdout
  hFlush stderr
  exitImmediately (ExitFailure (-1))
  error "" -- so it can match any type

maybeErrorExit :: [L.LogItem] -> Maybe a -> a
maybeErrorExit s Nothing  = errorExit s
maybeErrorExit _ (Just a) = a

eitherErrorExit :: Show e => [L.LogItem] -> Either e a -> a
eitherErrorExit s (Left e)  = errorExit (s ++ [L.TXT $ T.pack $ show e])
eitherErrorExit _ (Right a) = a

debugUnexpected   :: Monad m => [L.LogItem] -> R.Raft m a [L.LogItem]
debugUnexpected    = debugInRaft . DbgUnexpected
debugUnexpectedS  ::              [L.LogItem] ->          [L.LogItem]
debugUnexpectedS   = debugPure   . DbgUnexpected

data DI_Network
  = DbgPacketSent R.NodeID L.LogItem
  | DbgPacketRcvd R.NodeID L.LogItem
  deriving (Eq , Show)

debugNetwork :: Monad m => DI_Network -> R.Raft m a ()
debugNetwork = debug . DbgNetwork

data DI_Consensus
  = DbgRoleChange R.Role
  | DbgTermChange R.Term
  | DbgLbl R.MsgType [L.LogItem]
  | DbgLogIndex R.LogIndex
  | DbgCommitIndex R.LogIndex
  | DbgHash String BS.ByteString
  | DbgCommandsCompleted Int
  | DbgResultSent
  | DbgNotEnoughEvidence Int [R.NodeID]
  deriving (Eq , Show)

debugRaft :: Monad m => DI_Consensus -> R.Raft m a ()
debugRaft = debug . DbgConsensus

debugRaftHandler :: Monad m => R.MsgType -> [L.LogItem] -> R.Raft m a ()
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
  | DbgCkptResponse R.NodeID (Maybe Int) [L.LogItem]
  | DbgOpMode R.OperationMode [L.LogItem]
  | DbgOther [L.LogItem]
  deriving (Eq , Show)

debugRecovery :: Monad m => DI_Recovery -> R.Raft m a ()
debugRecovery = debug . DbgRecovery

class PP a where
  pp :: a -> [L.LogItem]

instance PP DebugInfo where
  pp (DbgNetwork     d)                   = L.INFO "network"   : pp d
  pp (DbgConsensus   d)                   = L.INFO "consensus" : pp d
  pp (DbgRecovery    d)                   = L.INFO "recovery"  : pp d
  pp (DbgApplication d)                   = L.INFO "app"       : d
  pp (DbgError       d)                   = L.INFO "ERROR"     : d
  pp (DbgUnexpected  d)                   = L.INFO "UNEXPECTED": d
  pp (DbgInfo        d)                   = L.INFO "info"      : d

instance PP DI_Recovery where
  pp (DbgRecTarget li)                    = L.RECOVERY "tgt" : pp li
  pp DbgRecovering                        = [L.RECOVERY "recovering"]
  pp DbgDoneRecovering                    = [L.RECOVERY "done-recovering"]
  pp DbgCatchingUp                        = [L.RECOVERY "catching-up"]
  pp DbgActive                            = [L.RECOVERY "active"]
  pp DbgInactive                          = [L.RECOVERY "inactive"]
  pp (DbgFailedRec er)                    = [L.RECOVERY "failed", L.TXT $ T.pack er]
  pp (DbgOpMode om ss)                    = L.RECOVERY "opmode" : (L.TXT $ T.pack $ show om) : ss
  pp (DbgOther strs)                      = L.RECOVERY "other" : strs
  pp (DbgCkptRequest  ni li pgi)          =
    L.RECOVERY "ckpt-request"  : (L.TXT $ T.pack $ show pgi) : (pp ni ++ pp li)
  pp (DbgCkptResponse ni pgi st)          =
    L.RECOVERY "ckpt-response" : (L.TXT $ T.pack $ show pgi) : (pp ni ++ st)

instance PP DI_Consensus where
  pp (DbgRoleChange role)                 = [L.CONSENSUS "role-change", L.ROLE role]
  pp (DbgTermChange t)                    = L.CONSENSUS "term-change" : pp t
  pp (DbgLbl msg logItems)                = (L.CONSENSUS $ T.pack $ show msg) : logItems
  pp (DbgLogIndex li)                     = L.CONSENSUS "log-index" : pp li
  pp (DbgCommitIndex ci)                  = L.CONSENSUS "commit-index" : pp ci
  pp (DbgHash t h)                        = (L.CONSENSUS $ T.pack t) : pp h
  pp DbgResultSent                        = [L.CONSENSUS "sent-results"]
  pp (DbgCommandsCompleted i)             = [L.CONSENSUS "commands", L.TXT $ T.pack $ show i]
  pp (DbgNotEnoughEvidence need slackers) =
    L.CONSENSUS (T.pack $ "not-enough-evidence; missing; " ++ show need ++ "; slackers") : map nodeidPP slackers

instance PP DI_Network where
  pp (DbgPacketSent ni packet)            = L.NETWORK "sent" : pp ni ++ [ packet ]
  pp (DbgPacketRcvd ni packet)            = L.NETWORK "rcvd" : pp ni ++ [ packet ]

nodeidPP :: R.NodeID -> L.LogItem
nodeidPP nid =
  let nport    = R._port nid
      shardno  = show $ (nport - 10000) `mod` 4
      serverno = show $ (nport - 10000) `div` 4
  in L.TXT $ T.pack $ "server:" ++ shardno ++ "/" ++ serverno ++ "/" ++ show nport

instance PP R.NodeID where
  pp nid                                  = [L.NID nid, nodeidPP nid]

instance PP R.Term where
  pp t                                    = [L.TERM t]

logindexPP :: R.LogIndex -> L.LogItem
logindexPP = L.LI

instance PP R.LogIndex where
  pp                                      = (:[]) . logindexPP

instance PP BS.ByteString where
  pp                                      = (:[]) . L.TXT . T.pack . show
