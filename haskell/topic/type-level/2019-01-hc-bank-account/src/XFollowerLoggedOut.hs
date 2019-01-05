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
  => ClientInputHandler 'LoggedOut sm UsernamePassword v
handleUsernamePassword (NodeLoggedOutState s) _nodeId up = do
  PersistentState{..} <- get
  if checkUsernamePassword up
    then do
      logInfo "LoggedOut.handleUsernamePassword valid"
      tellAction (SendRPC (SendEnterPin EnterPin))
      pure (candidateResultState LoggedOutToCandidate CandidateState)
    else do
      logInfo "LoggedOut.handleUsernamePassword invalid"
      tellActions [ SendRPC (SendInvalidUsernamePassword InvalidUsernamePassword)
                  , SendRPC (SendEnterUsernamePassword   EnterUsernamePassword)
                  ]
      pure (loggedOutResultState NoChange s)
 where
  checkUsernamePassword = Prelude.undefined

handleTimeout :: TimeoutHandler 'LoggedOut sm v
handleTimeout (NodeLoggedOutState s) timeout =
  case timeout of
    HeartbeatTimeout -> do
      logInfo "LoggedOut.handleTimeout"
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendRPC (SendEnterUsernamePassword EnterUsernamePassword)
                  ]
      pure (loggedOutResultState NoChange s)

