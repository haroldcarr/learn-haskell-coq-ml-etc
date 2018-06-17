module StateT where

import           Control.Monad.Identity
import           MonadTrans

{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use <$>" #-}
{-# ANN module "HLint: ignore Use first" #-}

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
type    State  s     = StateT s Identity

state               :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f              = StateT (return . f)

runState            :: State s a -> s -> (a, s)
runState m           = runIdentity . runStateT m

get                 :: (Monad m) => StateT s m s
get                  = state $ \s -> (s, s)

put                 :: (Monad m) => s -> StateT s m ()
put s                = state $ \_ -> ((), s)

modify              :: (Monad m) => (s -> s) -> StateT s m ()
modify f             = state $ \s -> ((), f s)

gets                :: (Monad m) => (s -> a) -> StateT s m a
gets f               = state $ \s -> (f s, s)

mapState            ::   ((a, s) ->   (b, s)) -> State  s   a -> State  s   b
mapStateT           :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapState  f          = mapStateT (Identity . f . runIdentity)
mapStateT f m        =              StateT $ f . runStateT m

withState           :: (s -> s) -> State  s   a -> State  s   a
withStateT          :: (s -> s) -> StateT s m a -> StateT s m a
withState            = withStateT
withStateT f m       = StateT $ runStateT m . f

instance (Functor m) => Functor (StateT s m) where
    fmap f m         = StateT $ \s ->
        fmap (\(a, s') -> (f a, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a           = StateT $ \s -> return (a, s)
    StateT mf <*> StateT mx
                     = StateT $ \s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
    return a         = StateT $ \s -> return (a, s)
    m >>= k          = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance MonadTrans (StateT s) where
    lift m           = StateT $ \s -> do
        a <- m
        return (a, s)
