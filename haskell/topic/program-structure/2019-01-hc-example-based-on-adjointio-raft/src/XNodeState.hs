{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
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

-- | Ensures that only valid transitions happen between node states.
data Transition (init :: Mode) (res :: Mode) where
  LoggedOutToCandidate :: Transition 'LoggedOut 'Candidate
  CandidateToLoggedIn  :: Transition 'Candidate 'LoggedIn
  CandidateToLoggedOut :: Transition 'Candidate 'LoggedOut
  LoggedInToLoggedOut  :: Transition 'LoggedIn  'LoggedOut
  NoChange             :: Transition init       init

deriving instance Show (Transition init res)

-- | Used to combine a `Transition` with an event handler's resulting state.
-- It makes the `ResultState` type to be dependent on the transition that occurred,
-- therefore only valid transitions can be specified by the ResultState
-- (N.B.: make sure hand-written `Transition` is correct.
--
-- Event handlers have signatures:
--   handler :: NodeState init -> ... relevant handler data ... -> ResultState init
-- `NodeState`   : typed by the `Mode` entering a handler.
-- `ResultState` : typed by the `Mode` existing a handler.
-- So `ResultState` statically enforces that only valid transitions happen.
--
-- TODO: Use this approach to limit the actions a node can emit dependent on its current mode.
--
-- Existential type `res` hides the result type of transition (accessed via pattern matching).
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

-- | The volatile state of a node may vary depending on its mode.
-- DataKinds/ GADTs used to enforce that
-- volatile state carried by a node matches its mode.
data NodeState (a :: Mode) v where
  NodeLoggedOutState :: LoggedOutState v -> NodeState 'LoggedOut v
  NodeCandidateState :: CandidateState v -> NodeState 'Candidate v
  NodeLoggedInState  :: LoggedInState  v -> NodeState 'LoggedIn  v

deriving instance Show v => Show (NodeState s v)

data LoggedOutState v = LoggedOutState
  deriving Show

data CandidateState v = CandidateState
  deriving Show

newtype LoggedInState v = LoggedInState
  { lsCmd :: Maybe v
  } deriving Show

