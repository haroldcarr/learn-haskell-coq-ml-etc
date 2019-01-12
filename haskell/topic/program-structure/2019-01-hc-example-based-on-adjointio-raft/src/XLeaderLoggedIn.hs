{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XLeaderLoggedIn where

import           XClient
import           XEvent
import           XMonad
import           XNodeState
------------------------------------------------------------------------------
import qualified Prelude
import           Protolude

handleUsernamePassword
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedIn sm UsernamePassword v
handleUsernamePassword _ns@(NodeLoggedInState s) _cid _up = do
  logCritical "LoggedIn.handleUsernamePassword: should not happend"
  pure (loggedInResultState NoChange s)

handlePin
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedIn sm Pin v
handlePin (NodeLoggedInState s) _c _p = do
  logCritical "LoggedIn.handlePin: should not happend"
  pure (loggedInResultState NoChange s)

handleTimeout :: TimeoutHandler 'LoggedIn sm v
handleTimeout (NodeLoggedInState _s) timeout = do
  logInfo ("LoggedIn.handleTimeout: " <> toS (Prelude.show timeout))
  case timeout of
    HeartbeatTimeout ->
      pure (loggedOutResultState LoggedInToLoggedOut LoggedOutState)


