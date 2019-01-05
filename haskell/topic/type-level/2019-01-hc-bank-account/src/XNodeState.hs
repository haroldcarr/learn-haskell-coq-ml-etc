{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module XNodeState where

import           Protolude

data Mode
  = LoggedOut
  | Candidate
  | LoggedIn
  deriving (Show)

-- | Valid state transitions.
data Transition (init :: Mode) (res :: Mode) where
  LoggedOutToCandidate :: Transition 'LoggedOut 'Candidate
  CandidateToLoggedIn  :: Transition 'Candidate 'LoggedIn
  CandidateToLoggedOut :: Transition 'Candidate 'LoggedOut
  LoggedInToLoggedOut  :: Transition 'LoggedIn  'LoggedOut
  NoChange             :: Transition init       init

deriving instance Show (Transition init res)

-- | Existential type `res` hide result type of transition.
data ResultState init v where
  ResultState
    :: Show v
    => Transition  init res
    -> NodeState   res  v
    -> ResultState init v

deriving instance Show v => Show (ResultState init v)

loggedOutResultState
  :: Show v
  => Transition init 'LoggedOut
  -> LoggedOutState v
  -> ResultState init v
loggedOutResultState transition state' =
  ResultState transition (NodeLoggedOutState state')

candidateResultState
  :: Show v
  => Transition init 'Candidate
  -> CandidateState v
  -> ResultState init v
candidateResultState transition state' =
  ResultState transition (NodeCandidateState state')

loggedInResultState
  :: Show v
  => Transition init 'LoggedIn
  -> LoggedInState v
  -> ResultState init v
loggedInResultState transition state' =
  ResultState transition (NodeLoggedInState state')

-- | Existential type `s` hides internal node state.
data XNodeState v where
  XNodeState :: { unXNodeState :: NodeState s v } -> XNodeState v

deriving instance Show v => Show (XNodeState v)

nodeMode :: XNodeState v -> Mode
nodeMode (XNodeState rns) =
  case rns of
    NodeLoggedOutState _ -> LoggedOut
    NodeCandidateState _ -> Candidate
    NodeLoggedInState  _ -> LoggedIn

initXNodeState :: XNodeState v
initXNodeState =
  XNodeState $
    NodeLoggedOutState LoggedOutState

-- | The volatile state of a Node.
data NodeState (a :: Mode) v where
  NodeLoggedOutState :: LoggedOutState v -> NodeState 'LoggedOut v
  NodeCandidateState :: CandidateState v -> NodeState 'Candidate v
  NodeLoggedInState  :: LoggedInState  v -> NodeState 'LoggedIn  v

deriving instance Show v => Show (NodeState s v)

data LoggedOutState v = LoggedOutState
  deriving Show

data CandidateState v = CandidateState
  deriving Show

data LoggedInState v = LoggedInState
  deriving Show

-- | Is node in loggedOut state?
isLoggedOut :: NodeState s v -> Bool
isLoggedOut = \case
  NodeLoggedOutState _ -> True
  _                    -> False

-- | Is node in candidate state?
isCandidate :: NodeState s v -> Bool
isCandidate = \case
  NodeCandidateState _ -> True
  _                    -> False

-- | Is node in loggedIn state?
isLoggedIn :: NodeState s v -> Bool
isLoggedIn = \case
  NodeLoggedInState _ -> True
  _                   -> False
