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
------------------------------------------------------------------------------
import qualified Prelude
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
      logInfo $ "LoggedOut.handleUsernamePassword valid: " <> toS (Prelude.show cid) <> " " <> toS (Prelude.show up)
      tellAction (SendToClient cid CresEnterPin)
      pure (candidateResultState LoggedOutToCandidate CandidateState)
    else do
      logInfo $ "LoggedOut.handleUsernamePassword invalid: " <> toS (Prelude.show cid) <> " " <> toS (Prelude.show up)
      tellActions [ SendToClient cid CresInvalidUserNamePassword
                  , SendToClient cid CresEnterUsernamePassword
                  ]
      pure (loggedOutResultState NoChange s)
 where
  checkUsernamePassword _ = True

handlePin
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedOut sm Pin v
handlePin (NodeLoggedOutState s) _c _p = do
  logCritical "LoggedOut.handlePin: should not happend"
  pure (loggedOutResultState NoChange s)

handleAcctNumOrQuit
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedOut sm AccNumOrQuit v
handleAcctNumOrQuit (NodeLoggedOutState s) _c _p = do
  logCritical "LoggedOut.handleAcctNumOrQuit: should not happend"
  pure (loggedOutResultState NoChange s)

handleTimeout :: TimeoutHandler 'LoggedOut sm v
handleTimeout (NodeLoggedOutState s) timeout =
  case timeout of
    HeartbeatTimeout -> do
      logInfo "LoggedOut.handleTimeout"
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient (ClientId "client") CresEnterUsernamePassword -- TODO - which client?
                  ]
      pure (loggedOutResultState NoChange s)

