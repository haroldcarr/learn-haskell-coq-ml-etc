{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module XHandle where

import           XAction
import           XClient
import qualified XCandidate         as Candidate
import           XEvent
import qualified XFollowerLoggedOut as Follower
import qualified XLeaderLoggedIn    as Leader
import           XLogging           (LogMsg)
import           XMonad
import           XNodeState
import           XPersistent
import           XRPC
------------------------------------------------------------------------------
import           Protolude

-- | Entry point for handling events.
handleEvent
  :: forall sm v
   . (RSMP sm v, Show v)
  => XNodeState v
  -> TransitionEnv sm v
  -> PersistentState
  -> Event v
  -> (XNodeState v, PersistentState, [Action sm v], [LogMsg])
handleEvent (XNodeState initNodeState') transitionEnv persistentState event =
  case handleEvent' initNodeState' transitionEnv persistentState event of
    ((ResultState _ resultState, logMsgs), persistentState', outputs) ->
      (XNodeState resultState, persistentState', outputs, logMsgs)

data XHandler ns sm v = XHandler
  { handleUsernamePassword :: ClientInputHandler ns sm UsernamePassword v
  , handlePin              :: ClientInputHandler ns sm Pin              v
  , handleAcctNumOrQuit    :: ClientInputHandler ns sm AccNumOrQuit     v
  , handleTimeout          :: TimeoutHandler     ns sm                  v
  }

followerXHandler  :: Show v => XHandler 'LoggedOut sm v
followerXHandler   = XHandler
  { handleUsernamePassword = Follower.handleUsernamePassword
  , handlePin              = Follower.handlePin
  , handleAcctNumOrQuit    = Follower.handleAcctNumOrQuit
  , handleTimeout          = Follower.handleTimeout
  }

candidateXHandler :: Show v => XHandler 'Candidate sm v
candidateXHandler  = XHandler
  { handleUsernamePassword = Candidate.handleUsernamePassword
  , handlePin              = Candidate.handlePin
  , handleAcctNumOrQuit    = Candidate.handleAcctNumOrQuit
  , handleTimeout          = Candidate.handleTimeout
  }

leaderXHandler    :: Show v => XHandler 'LoggedIn sm v
leaderXHandler     = XHandler
  { handleUsernamePassword = Leader.handleUsernamePassword
  , handlePin              = Leader.handlePin
  , handleAcctNumOrQuit    = Leader.handleAcctNumOrQuit
  , handleTimeout          = Leader.handleTimeout
  }

mkXHandler :: forall ns sm v. Show v => NodeState ns v -> XHandler ns sm v
mkXHandler nodeState =
  case nodeState of
    NodeLoggedOutState _ -> followerXHandler
    NodeCandidateState _ -> candidateXHandler
    NodeLoggedInState  _ -> leaderXHandler

handleEvent'
  :: forall ns sm v
   . (RSMP sm v, Show v)
  => NodeState ns v
  -> TransitionEnv sm v
  -> PersistentState
  -> Event v
  -> ((ResultState ns v, [LogMsg]), PersistentState, [Action sm v])
handleEvent' initNodeState' transitionEnv persistentState event =
  runTransitionM transitionEnv persistentState $
    case event of
      MessageEvent mev ->
        case mev of
          RPCMessageEvent    e -> handleRPCMessage           e
          ClientRequestEvent e -> handleClientRequestMessage e
      TimeoutEvent tout ->
        handleTimeout initNodeState' tout
 where
  XHandler{..} = mkXHandler initNodeState'

  handleRPCMessage :: RPCMessage v -> TransitionM sm v (ResultState ns v)
  handleRPCMessage (RPCMessage _sender _rpc) = panic "there are no RPC messages defined"

  handleClientRequestMessage :: ClientRequest v -> TransitionM sm v (ResultState ns v)
  handleClientRequestMessage msg = case msg of
    CreqUsernamePassword cid up -> handleUsernamePassword initNodeState' cid up
    CreqPin              cid  p -> handlePin              initNodeState' cid  p
    CreqAcctNumOrQuit    cid an -> handleAcctNumOrQuit    initNodeState' cid an