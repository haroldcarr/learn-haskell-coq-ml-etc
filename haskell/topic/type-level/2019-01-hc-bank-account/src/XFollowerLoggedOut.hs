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
import           XEvent
import           XMonad
import           XNodeState
import           XPersistent
import           XRPC
------------------------------------------------------------------------------
import qualified Prelude
import           Protolude

--------------------------------------------------------------------------------
-- LoggedOut
--------------------------------------------------------------------------------

handleUsernamePassword
  :: forall v sm
   . Show v
  => RPCHandler 'LoggedOut sm (UsernamePassword v) v
handleUsernamePassword (NodeLoggedOutState s) _nodeId up@(UsernamePassword _v) = do
  PersistentState{..} <- get
  if checkUsernamePassword up
    then do
      logInfo "LoggedOut.handleUsernamePassword valid"
      tellAction (ClientAction EnterPin)
      pure (candidateResultState TStartTwoFactorAuthN CandidateState)
    else do
      logInfo "LoggedOut.handleUsernamePassword invalid"
      tellAction (ClientAction EnterUsernamePassword)
      pure (loggedOutResultState Noop s)
 where
  checkUsernamePassword = Prelude.undefined

handleTimeout :: TimeoutHandler 'LoggedOut sm v
handleTimeout (NodeLoggedOutState s) timeout =
  case timeout of
    HeartbeatTimeout -> do
      logInfo "LoggedOut.handleTimeout"
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , ClientAction EnterUsernamePassword
                  ]
      pure (loggedOutResultState Noop s)

