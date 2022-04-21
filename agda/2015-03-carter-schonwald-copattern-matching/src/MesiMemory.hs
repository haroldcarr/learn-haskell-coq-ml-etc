{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module MesiMemory where

newtype CoData con = CoData { unCoData :: forall r. con r -> r }

-- https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/d117b1k?utm_source=share&utm_medium=web2x&context=3
-- https://gist.github.com/cartazio/5b1d3d8dd12b9279866d

-- https://en.wikipedia.org/wiki/MESI_protocol

data MesiState
  = Modified
  | Exclusive
  | Shared
  | Invalid
  deriving (Eq, Show)

data MesiCommandI r where
  LocalReadI   :: MesiCommandI (CoData MesiCommandI)
  LocalWriteI  :: MesiCommandI (CoData MesiCommandI)
  RemoteReadI  :: MesiCommandI (CoData MesiCommandI)
  RemoteWriteI :: MesiCommandI (CoData MesiCommandI)
  CurrentState :: MesiCommandI MesiState

mkMesiCommandI :: MesiState -> CoData MesiCommandI
mkMesiCommandI s = CoData $ \case
  LocalReadI   -> if s == Invalid  then mkMesiCommandI Exclusive
                  else                  mkMesiCommandI s
  ------------------------------------------------------------------------------
  LocalWriteI  ->                       mkMesiCommandI Modified
  ------------------------------------------------------------------------------
  RemoteReadI  -> if s == Invalid
                  || s == Modified then mkMesiCommandI Invalid
                  else                  mkMesiCommandI Shared
  ------------------------------------------------------------------------------
  RemoteWriteI ->                       mkMesiCommandI Invalid
  ------------------------------------------------------------------------------
  CurrentState ->                       s

localReadI   :: CoData MesiCommandI -> CoData MesiCommandI
localReadI   (CoData f) = f LocalReadI

localWriteI  :: CoData MesiCommandI -> CoData MesiCommandI
localWriteI  (CoData f) = f LocalWriteI

remoteReadI  :: CoData MesiCommandI -> CoData MesiCommandI
remoteReadI  (CoData f) = f RemoteReadI

remoteWriteI :: CoData MesiCommandI -> CoData MesiCommandI
remoteWriteI (CoData f) = f RemoteWriteI

currentState :: CoData MesiCommandI -> MesiState
currentState (CoData f) = f CurrentState

