{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XCandidate where

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
  => ClientInputHandler 'Candidate sm UsernamePassword v
handleUsernamePassword (NodeCandidateState s) _cid _up = do
  logCritical ["Candidate.handleUsernamePassword: should not happend"]
  pure (candidateResultState NoChange s)

handlePin
  :: forall v sm
   . Show v
  => ClientInputHandler 'Candidate sm Pin v
handlePin (NodeCandidateState s) c p =
  if checkPin p
    then do
      logInfo $ "Candidate.handlePin: valid":showInfo
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresEnterCommandOrQuit
                  ]
      pure (loggedInResultState CandidateToLoggedIn (LoggedInState Nothing))
    else do
      logInfo $ "Candidate.handlePin invalid":showInfo
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient c CresInvalidPin
                  , SendToClient c CresEnterPin
                  ]
      pure (candidateResultState NoChange s)
 where
  checkPin _ = True
  showInfo = [pshow c, pshow p]

handleCommandOrQuit
  :: forall v sm
   . Show v
  => ClientInputHandler 'Candidate sm (CommandOrQuit v) v
handleCommandOrQuit (NodeCandidateState s) _c _p = do
  logCritical ["Candidate.handleCommandOrQuit: should not happend"]
  pure (candidateResultState NoChange s)

handleTimeout :: TimeoutHandler 'Candidate sm v
handleTimeout (NodeCandidateState _s) timeout = do
  logInfo ["Candidate.handleTimeout", pshow timeout]
  case timeout of
    HeartbeatTimeout -> do
      tellActions [ ResetTimeoutTimer HeartbeatTimeout
                  , SendToClient (ClientId "client") CresEnterUsernamePassword -- TODO client id
                  ]
      pure (loggedOutResultState CandidateToLoggedOut LoggedOutState)
