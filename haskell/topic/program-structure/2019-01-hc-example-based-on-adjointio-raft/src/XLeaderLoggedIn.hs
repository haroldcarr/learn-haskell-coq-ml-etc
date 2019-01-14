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
import           XUtil
------------------------------------------------------------------------------
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
  => ClientInputHandler 'LoggedIn sm (AcctNumOrQuit v) v
handleAcctNumOrQuit (NodeLoggedInState s) c a@(AcctNumOrQuit t v) = do
  logInfo ["LoggedIn.handleAcctNumOrQuit", pshow c, pshow a]
  case t of
    "R" -> do
      r <- CresStateMachine <$> asks stateMachine
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c r
                  , SendToClient c CresEnterAcctNumOrQuit
                  ]
      pure (loggedInResultState NoChange s)
    "W" -> do
      let s' = s { lsCmd = Just v }
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresEnterAcctNumOrQuit
                  ]
      pure (loggedInResultState NoChange s')
    "Q" -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresQuit
                  ]
      pure (loggedOutResultState LoggedInToLoggedOut LoggedOutState)
    _ -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresEnterAcctNumOrQuit
                  ]
      pure (loggedInResultState NoChange s)

handleTimeout :: TimeoutHandler 'LoggedIn sm v
handleTimeout (NodeLoggedInState _s) timeout = do
  logInfo ["LoggedIn.handleTimeout", pshow timeout]
  case timeout of
    HeartbeatTimeout -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient (ClientId "client") CresEnterUsernamePassword -- TODO client id
                  ]
      pure (loggedOutResultState LoggedInToLoggedOut LoggedOutState)


