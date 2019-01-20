{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XLeaderLoggedIn where

import           XActionOutput
import           XClient
import           XEventInput
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
handleUsernamePassword _ns@(NodeLoggedInState s) _ _ = do
  logCritical ["LoggedIn.handleUsernamePassword: should not happend"]
  pure (loggedInResultState NoChange s)

handlePin
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedIn sm Pin v
handlePin (NodeLoggedInState s) _c _p = do
  logCritical ["LoggedIn.handlePin: should not happend"]
  pure (loggedInResultState NoChange s)

handleCommandOrQuit
  :: forall v sm
   . Show v
  => ClientInputHandler 'LoggedIn sm (CommandOrQuit v) v
handleCommandOrQuit (NodeLoggedInState s) c a@(CommandOrQuit t v) = do
  logInfo ["LoggedIn.handleCommandOrQuit", pshow c, pshow a]
  case t of
    "R" -> do
      r <- CresStateMachine <$> asks stateMachine
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c r
                  , SendToClient c CresEnterCommandOrQuit
                  ]
      pure (loggedInResultState NoChange s)
    "W" -> do
      let s' = s { lsCmd = Just v }
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresEnterCommandOrQuit
                  ]
      pure (loggedInResultState NoChange s')
    "Q" -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresQuit
                  ]
      pure (loggedOutResultState LoggedInToLoggedOut LoggedOutState)
    _ -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresInvalidCommand
                  , SendToClient c CresEnterCommandOrQuit
                  ]
      pure (loggedInResultState NoChange s)

handleTimeout :: TimeoutHandler 'LoggedIn sm v
handleTimeout (NodeLoggedInState _) timeout = do
  logInfo ["LoggedIn.handleTimeout", pshow timeout]
  case timeout of
    HeartbeatTimeout -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient (ClientId "client") CresEnterUsernamePassword -- TODO client id
                  ]
      pure (loggedOutResultState LoggedInToLoggedOut LoggedOutState) -- TODO ??


