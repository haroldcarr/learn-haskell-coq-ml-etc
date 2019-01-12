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
------------------------------------------------------------------------------
import qualified Prelude
import           Protolude

handleUsernamePassword
  :: forall v sm
   . Show v
  => ClientInputHandler 'Candidate sm UsernamePassword v
handleUsernamePassword (NodeCandidateState s) _cid _up = do
  logCritical "Candidate.handleUsernamePassword: should not happend"
  pure (candidateResultState NoChange s)

handlePin
  :: forall v sm
   . Show v
  => ClientInputHandler 'Candidate sm Pin v
handlePin (NodeCandidateState s) c p =
  if checkPin p
    then do
      logInfo $ "Candidate.handlePin: valid: " <> toS (Prelude.show c) <> " " <> toS (Prelude.show p)
      tellAction (SendToClient c (CresEnterAcctNumOrQuit "1,2,3"))
      pure (loggedInResultState CandidateToLoggedIn LoggedInState)
    else do
      logInfo $ "Candidate.handlePin invalid: " <> toS (Prelude.show c) <> " " <> toS (Prelude.show p)
      tellActions [ SendToClient c CresInvalidPin
                  , SendToClient c CresEnterPin
                  ]
      pure (candidateResultState NoChange s)
 where
  checkPin _ = True

handleAcctNumOrQuit
  :: forall v sm
   . Show v
  => ClientInputHandler 'Candidate sm AccNumOrQuit v
handleAcctNumOrQuit (NodeCandidateState s) _c _p = do
  logCritical "Candidate.handleAcctNumOrQuit: should not happend"
  pure (candidateResultState NoChange s)

handleTimeout :: TimeoutHandler 'Candidate sm v
handleTimeout (NodeCandidateState _s) timeout = do
  logInfo ("Candidate.handleTimeout: " <> toS (Prelude.show timeout))
  case timeout of
    HeartbeatTimeout ->
      pure (loggedOutResultState CandidateToLoggedOut LoggedOutState)
