{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module XLeaderLoggedIn where

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
  => RPCHandler 'LoggedIn sm (UsernamePassword v) v
handleUsernamePassword _ns@(NodeLoggedInState _s) _nodeId _up@(UsernamePassword _v) =
  Prelude.undefined

handleTimeout :: TimeoutHandler 'LoggedIn sm v
handleTimeout (NodeLoggedInState fs) timeout =
  case timeout of
    HeartbeatTimeout -> pure (loggedInResultState Noop fs)


