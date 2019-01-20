{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}

module XMonad where

import           XActionOutput
import           XConfig
import           XEventInput
import           XLogging          (LogMsg, XLogger, XLoggerT (..), runXLoggerT)
import qualified XLogging          as Logging
import           XNodeState
import           XPersistent
import           XTypes
------------------------------------------------------------------------------
import           Control.Arrow     ((&&&))
import           Control.Monad.RWS
import           Protolude         hiding (pass)

--------------------------------------------------------------------------------
-- State Machine
--------------------------------------------------------------------------------

-- | Interface to handle commands in the underlying state machine.
-- Relates a state machine type (`sm`) to a command type (`v`).
-- Provides pure function to applying command to a state machine.
--
-- Functional dependency ensures only a single state machine command
-- to be defined to update the state machine.
class XSMP sm v | sm -> v where
  data XSMPError sm v
  type XSMPCtx   sm v = ctx | ctx -> sm v
  applyCmdXSMP :: XSMPCtx sm v -> sm -> v -> Either (XSMPError sm v) sm

class (Monad m, XSMP sm v) => XSM sm v m | m sm -> v where
  validateCmd :: v -> m (Either (XSMPError sm v) ())
  askXSMPCtx  ::      m         (XSMPCtx   sm v)

-- TODO : use this
applyCmdXSM :: XSM sm v m => sm -> v -> m (Either (XSMPError sm v) sm)
applyCmdXSM sm v  = do
  res <- validateCmd v
  case res of
    Left err -> pure (Left err)
    Right () -> do
      ctx <- askXSMPCtx
      pure (applyCmdXSMP ctx sm v)

--------------------------------------------------------------------------------
-- X Monad
--------------------------------------------------------------------------------

tellActions :: [Action sm v] -> TransitionM sm v ()
tellActions  = tell

data TransitionEnv sm v = TransitionEnv
  { nodeConfig   :: NodeConfig
  , stateMachine :: sm
  , nodeState    :: XNodeState v
  }

newtype TransitionM sm v a = TransitionM
  { unTransitionM :: XLoggerT v (RWS (TransitionEnv sm v) [Action sm v] PersistentState) a
  } deriving (Functor, Applicative, Monad)

instance MonadWriter [Action sm v] (TransitionM sm v) where
  tell   = TransitionM . XLoggerT . tell
  listen = TransitionM . XLoggerT . listen . unXLoggerT . unTransitionM
  pass   = TransitionM . XLoggerT . pass   . unXLoggerT . unTransitionM

instance MonadReader (TransitionEnv sm v) (TransitionM sm v) where
  ask     = TransitionM . XLoggerT $ ask
  local f = TransitionM . XLoggerT . local f . unXLoggerT . unTransitionM

instance MonadState PersistentState (TransitionM sm v) where
  get = TransitionM . XLoggerT $ lift   get
  put = TransitionM . XLoggerT . lift . put

instance XLogger v (RWS (TransitionEnv sm v) [Action sm v] PersistentState) where
  loggerCtx = asks ((configNodeId . nodeConfig) &&& nodeState)

runTransitionM
  :: TransitionEnv sm v
  -> PersistentState
  -> TransitionM sm v a
  -> ((a, [LogMsg]), PersistentState, [Action sm v])
runTransitionM transEnv persistentState transitionM =
  runRWS (runXLoggerT (unTransitionM transitionM)) transEnv persistentState

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

type ClientInputHandler ns sm r v
  =  Show v
  => NodeState ns v
  -> ClientId
  -> r
  -> TransitionM sm v (ResultState ns v)

type TimeoutHandler ns sm v
  =  Show v
  => NodeState ns v
  -> Timeout
  -> TransitionM sm v (ResultState ns v)

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

logCritical,logDebug,logInfo :: [Text] -> TransitionM sm v ()
logDebug    = TransitionM . Logging.logDebug
logInfo     = TransitionM . Logging.logInfo
logCritical = TransitionM . Logging.logCritical
