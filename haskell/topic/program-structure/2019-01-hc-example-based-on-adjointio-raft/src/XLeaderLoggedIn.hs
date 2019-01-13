{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XLeaderLoggedIn where

import           XAction
import           XClient
import           XEvent
import           XMonad
import           XNodeState
import           XTypes
------------------------------------------------------------------------------
import qualified Prelude
import           Protolude

handleUsernamePassword
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedIn sm UsernamePassword v
handleUsernamePassword _ns@(NodeLoggedInState s) _cid _up = do
  logCritical ["LoggedIn.handleUsernamePassword: should not happend"]
  pure (loggedInResultState NoChange s)

handlePin
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedIn sm Pin v
handlePin (NodeLoggedInState s) _c _p = do
  logCritical ["LoggedIn.handlePin: should not happend"]
  pure (loggedInResultState NoChange s)

handleAcctNumOrQuit
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedIn sm AccNumOrQuit v
handleAcctNumOrQuit (NodeLoggedInState s) c a = do
  logInfo ["LoggedIn.handleAcctNumOrQuit", toS (Prelude.show c) <> " " <> toS (Prelude.show a)]
  case anoqAcctNumOrQuit a of
    "Q" -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  -- , SendToClient c (CresAcctBalance _)
                  -- , SendToClient c (CresEnterAcctNumOrQuit "1,2,3")
                  , SendToClient c CresQuit
                  ]
      pure (loggedOutResultState LoggedInToLoggedOut LoggedOutState)
    _ -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  -- , SendToClient c (CresAcctBalance _)
                  , SendToClient c (CresEnterAcctNumOrQuit "1,2,3")
                  ]
      pure (loggedInResultState NoChange s)

handleTimeout :: TimeoutHandler 'LoggedIn sm v
handleTimeout (NodeLoggedInState _s) timeout = do
  logInfo ["LoggedIn.handleTimeout", toS (Prelude.show timeout)]
  case timeout of
    HeartbeatTimeout -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient (ClientId "client") CresEnterUsernamePassword -- TODO client id
                  ]
      pure (loggedOutResultState LoggedInToLoggedOut LoggedOutState)


