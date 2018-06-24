{-# LANGUAGE MultiParamTypeClasses  #-}

module ReaderT where

import           Control.Monad.Identity
import           MonadTrans

------------------------------------------------------------------------------
-- transformers
-- Control.Monad.Trans.Reader

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
type    Reader  r     = ReaderT r Identity

runReader      :: Reader r a -> r -> a
runReader m     = runIdentity . runReaderT m

ask            :: (Monad m) => ReaderT r m r
ask             = ReaderT return

local          :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local  f m      = ReaderT $ runReaderT m . f

asks           :: (Monad m) => (r -> a) -> ReaderT r m a
asks   f        = ReaderT (return . f)

mapReader      :: (  a ->   b) -> Reader  r   a -> Reader  r   b
mapReaderT     :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReader  f    = mapReaderT (Identity . f . runIdentity)
mapReaderT f m  =             ReaderT  $ f . runReaderT m

instance (Functor m) => Functor (ReaderT r m) where
    fmap f      = mapReaderT (fmap f)

instance (Applicative m) => Applicative (ReaderT r m) where
    pure        = liftReaderT . pure
    f <*> v     = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance (Monad m) => Monad (ReaderT r m) where
    return      = lift . return
    m >>= k     = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg    = lift (fail msg)

instance MonadTrans (ReaderT r) where
    lift        = liftReaderT

liftReaderT    :: m a -> ReaderT r m a
liftReaderT m   = ReaderT (const m)
