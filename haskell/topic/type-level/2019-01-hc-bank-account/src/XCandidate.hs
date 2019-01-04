{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XCandidate where

import           XEvent
import           XMonad
import           XNodeState
import           XRPC
------------------------------------------------------------------------------
import qualified Prelude
import           Protolude

handleUsernamePassword
  :: forall v sm
   . Show v
  => RPCHandler 'Candidate sm (UsernamePassword v) v
handleUsernamePassword _ns@(NodeCandidateState _s) _nodeId _up@(UsernamePassword _v) =
  Prelude.undefined

handleTimeout :: TimeoutHandler 'Candidate sm v
handleTimeout (NodeCandidateState fs) timeout =
  case timeout of
    HeartbeatTimeout -> pure (candidateResultState Noop fs)
