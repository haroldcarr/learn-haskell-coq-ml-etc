{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module XFollowerLoggedOut
  ( handleUsernamePassword
  , handleTimeout
  ) where

import           XAction
import           XClient
import           XEvent
import           XMonad
import           XNodeState
import           XPersistent
import           XTypes
------------------------------------------------------------------------------
import           Protolude

--------------------------------------------------------------------------------
-- LoggedOut
--------------------------------------------------------------------------------

handleUsernamePassword
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedOut sm UsernamePassword v
handleUsernamePassword (NodeLoggedOutState s) _nodeId up = do
  PersistentState{..} <- get
  if checkUsernamePassword up
    then do
      logInfo "LoggedOut.handleUsernamePassword valid"
      tellAction (SendToClient (ClientId "client") CresEnterPin)
      pure (candidateResultState LoggedOutToCandidate CandidateState)
    else do
      logInfo "LoggedOut.handleUsernamePassword invalid"
      tellActions [ SendToClient (ClientId "client") CresInvalidUserNamePassword
                  , SendToClient (ClientId "client") CresEnterUsernamePassword
                  ]
      pure (loggedOutResultState NoChange s)
 where
  checkUsernamePassword _ = True

handleTimeout :: TimeoutHandler 'LoggedOut sm v
handleTimeout (NodeLoggedOutState s) timeout =
  case timeout of
    HeartbeatTimeout -> do
      logInfo "LoggedOut.handleTimeout"
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient (ClientId "client") CresEnterUsernamePassword
                  ]
      pure (loggedOutResultState NoChange s)

