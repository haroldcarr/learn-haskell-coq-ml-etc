{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module XHandle
  ( handleEvent
  ) where

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
------------------------------------------------------------------------------
import           Protolude

-- | Entry point for handling incoming events.
handleEvent
  :: forall sm v
   . (RSMP sm v, Show v)
  => XNodeState v
  -> TransitionEnv sm v
  -> PersistentState
  -> Event v
  -> (XNodeState v, PersistentState, [Action sm v], [LogMsg])
handleEvent (XNodeState inNodeState) transitionEnv inPersistentState event =
  case handleEvent' inNodeState transitionEnv inPersistentState event of
    ((ResultState _ outNodeState, logMsgs), outPersistentState, outputs) ->
      (XNodeState outNodeState, outPersistentState, outputs, logMsgs)

handleEvent'
  :: forall ns sm v
   . (RSMP sm v, Show v)
  => NodeState ns v
  -> TransitionEnv sm v
  -> PersistentState
  -> Event v
  -> ((ResultState ns v, [LogMsg]), PersistentState, [Action sm v])
handleEvent' inNodeState transitionEnv persistentState event =
  runTransitionM transitionEnv persistentState $
    case event of
      MessageEvent me -> case me of
        ClientRequestEvent e -> case e of
          CreqUsernamePassword cid up -> handleUsernamePassword inNodeState cid up
          CreqPin              cid  p -> handlePin              inNodeState cid  p
          CreqCommandOrQuit    cid an -> handleCommandOrQuit    inNodeState cid an
      TimeoutEvent te ->
        handleTimeout inNodeState te
 where
  XHandler{..} = case inNodeState of
    NodeLoggedOutState _ -> followerXHandler
    NodeCandidateState _ -> candidateXHandler
    NodeLoggedInState  _ -> leaderXHandler

data XHandler ns sm v = XHandler
  { handleUsernamePassword :: ClientInputHandler ns sm UsernamePassword  v
  , handlePin              :: ClientInputHandler ns sm Pin               v
  , handleCommandOrQuit    :: ClientInputHandler ns sm (CommandOrQuit v) v
  , handleTimeout          :: TimeoutHandler     ns sm                   v
  }

followerXHandler  :: Show v => XHandler 'LoggedOut sm v
followerXHandler   = XHandler
  { handleUsernamePassword = Follower.handleUsernamePassword
  , handlePin              = Follower.handlePin
  , handleCommandOrQuit    = Follower.handleCommandOrQuit
  , handleTimeout          = Follower.handleTimeout
  }

candidateXHandler :: Show v => XHandler 'Candidate sm v
candidateXHandler  = XHandler
  { handleUsernamePassword = Candidate.handleUsernamePassword
  , handlePin              = Candidate.handlePin
  , handleCommandOrQuit    = Candidate.handleCommandOrQuit
  , handleTimeout          = Candidate.handleTimeout
  }

leaderXHandler    :: Show v => XHandler 'LoggedIn sm v
leaderXHandler     = XHandler
  { handleUsernamePassword = Leader.handleUsernamePassword
  , handlePin              = Leader.handlePin
  , handleCommandOrQuit    = Leader.handleCommandOrQuit
  , handleTimeout          = Leader.handleTimeout
  }

