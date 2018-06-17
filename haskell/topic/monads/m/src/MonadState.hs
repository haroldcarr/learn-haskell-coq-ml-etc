{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module MonadState
  ( MonadState(..)
    -- * State
  , State
  , runState
  , mapState
    -- * StateT
  , StateT (StateT)
  , runStateT
  , mapStateT
  ) where

import           MonadTrans
import           ReaderT
import           StateT     (State (..), StateT (..), get, mapState, mapStateT,
                             put, runState, state)

{-# ANN module "HLint: ignore Use const" #-}

------------------------------------------------------------------------------
-- mtl
-- Control.Monad.State

-- | Minimal definition is either both of @get@ and @put@ or just @state@
class Monad m => MonadState s m | m -> s where
    get :: m s
    get = MonadState.state (\s -> (s, s))

    put :: s -> m ()
    put s = MonadState.state (\_ -> ((), s))

    state :: (s -> (a, s)) -> m a
    state f = do
      s <- MonadState.get
      let ~(a, s') = f s
      MonadState.put s'
      return a

modify :: MonadState s m => (s -> s) -> m ()
modify f = MonadState.state (\s -> ((), f s))

gets :: MonadState s m => (s -> a) -> m a
gets f = do
    s <- MonadState.get
    return (f s)

instance Monad m => MonadState s (StateT s m) where
    get = StateT.get
    put = StateT.put
    state = StateT.state

instance MonadState s m => MonadState s (ReaderT r m) where
    get   = lift MonadState.get
    put   = lift . MonadState.put
    state = lift . MonadState.state
