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
handleUsernamePassword _ns@(NodeLoggedOutState s) _nodeId up@(UsernamePassword v) = do
  PersistentState{..} <- get
  if checkUsernamePassword up
    then candidateResultState TStartTwoFactorAuthN <$> startTwoFactorAuthN v
    else pure (loggedOutResultState Noop s)
 where
  checkUsernamePassword = Prelude.undefined

handleTimeout :: TimeoutHandler 'LoggedOut sm v
handleTimeout (NodeLoggedOutState _fs) timeout =
  case timeout of
    HeartbeatTimeout -> do
      logInfo "LoggedOut.handleTimeout"
      tellAction (ResetTimeoutTimer HeartbeatTimeout)
      pure (candidateResultState TStartTwoFactorAuthN CandidateState)

