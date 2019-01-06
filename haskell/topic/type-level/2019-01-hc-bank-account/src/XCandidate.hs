{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XCandidate where

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
handleUsernamePassword (NodeCandidateState s) _nodeId _up = do
  logCritical "Candidate.handleUsernamePassword: should not happend"
  pure (candidateResultState NoChange s)

handleTimeout :: TimeoutHandler 'Candidate sm v
handleTimeout (NodeCandidateState _s) timeout = do
  logInfo ("Candidate.handleTimeout: " <> toS (Prelude.show timeout))
  case timeout of
    HeartbeatTimeout ->
      pure (loggedOutResultState CandidateToLoggedOut LoggedOutState)
