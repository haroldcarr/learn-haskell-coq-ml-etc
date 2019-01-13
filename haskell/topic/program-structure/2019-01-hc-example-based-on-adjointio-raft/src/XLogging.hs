{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module XLogging where

import           XNodeState
import           XTypes
------------------------------------------------------------------------------
import           Control.Monad.State       (modify')
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Text                 as T
import           Data.Time
import           Protolude

-- | Representation of the logs' destination.
data LogDest
  = LogFile FilePath
  | LogStdout
  | NoLogs

-- | Representation of the severity of the logs.
data Severity
  = Info
  | Debug
  | Critical
  deriving Show

data LogMsg = LogMsg
  { lmTime     :: Maybe UTCTime
  , lmSeverity :: Severity
  , lmData     :: LogMsgData
  } deriving Show

data LogMsgData = LogMsgData
  { lmdNodeId    :: NodeId
  , lmdNodeState :: Mode
  , lmdMsg       :: Text
  } deriving (Show)

logMsgToText :: LogMsg -> Text
logMsgToText (LogMsg mt s d) =
  maybe "" timeToText mt <> "(" <> show s <> ")" <> " " <> logMsgDataToText d
 where
  timeToText  :: UTCTime -> Text
  timeToText t = "[" <> toS (timeToText' t) <> "]"
  timeToText'  = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

logMsgDataToText :: LogMsgData -> Text
logMsgDataToText LogMsgData{..} =
  "<" <> toS lmdNodeId <> " | " <> show lmdNodeState <> ">: " <> lmdMsg

class Monad m => XLogger v m | m -> v where
  loggerCtx :: m (NodeId, XNodeState v)

mkLogMsgData :: XLogger v m => Text -> m LogMsgData
mkLogMsgData msg = do
  (nid, nodeState) <- loggerCtx
  let mode = nodeMode nodeState
  pure $ LogMsgData nid mode msg

instance XLogger v m => XLogger v (XLoggerT v m) where
  loggerCtx = lift loggerCtx

--------------------------------------------------------------------------------
-- Logging with IO
--------------------------------------------------------------------------------

logToDest :: MonadIO m => LogDest -> LogMsg -> m ()
logToDest logDest logMsg =
  case logDest of
    LogStdout  -> putText (logMsgToText logMsg)
    LogFile fp -> liftIO $ appendFile fp (logMsgToText logMsg <> "\n")
    NoLogs     -> pure ()

logToStdout :: MonadIO m => LogMsg -> m ()
logToStdout = logToDest LogStdout

logToFile :: MonadIO m => FilePath -> LogMsg -> m ()
logToFile fp = logToDest (LogFile fp)

logWithSeverityIO :: forall m v. (XLogger v m, MonadIO m) => Severity -> LogDest -> Text -> m ()
logWithSeverityIO s logDest msg = do
  logMsgData <- mkLogMsgData msg
  now <- liftIO getCurrentTime
  let logMsg = LogMsg (Just now) s logMsgData
  logToDest logDest logMsg

logInfoIO :: (XLogger v m, MonadIO m) => LogDest -> Text -> m ()
logInfoIO = logWithSeverityIO Info

logDebugIO :: (XLogger v m, MonadIO m) => LogDest -> Text -> m ()
logDebugIO = logWithSeverityIO Debug

logCriticalIO :: (XLogger v m, MonadIO m) => LogDest -> Text -> m ()
logCriticalIO = logWithSeverityIO Critical

--------------------------------------------------------------------------------
-- Pure Logging
--------------------------------------------------------------------------------

newtype XLoggerT v m a = XLoggerT
  { unXLoggerT :: StateT [LogMsg] m a
  } deriving (Functor, Applicative, Monad, MonadState [LogMsg], MonadTrans)

runXLoggerT
  :: Monad m
  => XLoggerT v m a -- ^ The computation from which to extract the logs
  -> m (a, [LogMsg])
runXLoggerT = flip runStateT [] . unXLoggerT

type XLoggerM v = XLoggerT v Identity

runXLoggerM
  :: XLoggerM v a
  -> (a, [LogMsg])
runXLoggerM = runIdentity . runXLoggerT

logWithSeverity :: XLogger v m => Severity -> Text -> XLoggerT v m ()
logWithSeverity s txt = do
  !logMsgData <- mkLogMsgData txt
  let !logMsg = LogMsg Nothing s logMsgData
  modify' (++ [logMsg])

logInfo     :: XLogger v m => [Text] -> XLoggerT v m ()
logInfo      = logWithSeverity Info     . fields

logDebug    :: XLogger v m => [Text] -> XLoggerT v m ()
logDebug     = logWithSeverity Debug    . fields

logCritical :: XLogger v m => [Text] -> XLoggerT v m ()
logCritical  = logWithSeverity Critical . fields

fields :: [Text] -> Text
fields as = T.intercalate "; " (map toS as)
