{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module XFollowerLoggedOut where

import           XAction
import           XClient
import           XEvent
import           XMonad
import           XNodeState
import           XPersistent
import           XTypes
import           XUtil
------------------------------------------------------------------------------
import           Protolude

--------------------------------------------------------------------------------
-- LoggedOut
--------------------------------------------------------------------------------

handleUsernamePassword
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedOut sm UsernamePassword v
handleUsernamePassword (NodeLoggedOutState s) cid up = do
  PersistentState{..} <- get
  if checkUsernamePassword up
    then do
      logInfo $ "LoggedOut.handleUsernamePassword valid":showInfo
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient cid CresEnterPin
                  ]
      pure (candidateResultState LoggedOutToCandidate CandidateState)
    else do
      logInfo $ "LoggedOut.handleUsernamePassword invalid":showInfo
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient cid CresInvalidUserNamePassword
                  , SendToClient cid CresEnterUsernamePassword
                  ]
      pure (loggedOutResultState NoChange s)
 where
  checkUsernamePassword _ = True
  showInfo = [pshow cid, pshow up]

handlePin
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedOut sm Pin v
handlePin (NodeLoggedOutState s) _c _p = do
  logCritical ["LoggedOut.handlePin: should not happend"]
  pure (loggedOutResultState NoChange s)

handleCommandOrQuit
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedOut sm (CommandOrQuit v) v
handleCommandOrQuit (NodeLoggedOutState s) _c _p = do
  logCritical ["LoggedOut.handleCommandOrQuit: should not happend"]
  pure (loggedOutResultState NoChange s)

handleTimeout :: TimeoutHandler 'LoggedOut sm v
handleTimeout (NodeLoggedOutState s) timeout = do
  logInfo ["LoggedOut.handleTimeout", pshow timeout]
  case timeout of
    HeartbeatTimeout -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient (ClientId "client") CresEnterUsernamePassword -- TODO client id
                  ]
      pure (loggedOutResultState NoChange s)

