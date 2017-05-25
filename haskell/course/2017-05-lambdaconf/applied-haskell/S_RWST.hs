#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module S_RWST where

import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.RWS.Class
import           Control.Monad.Trans.Class
import           Data.IORef

newtype RWST r w s m a = RWST
  { runRWST :: r
            -> IORef w
            -> IORef s
            -> m a
  } deriving Functor

instance (Applicative m) => Applicative (RWST r w s m) where
  pure x = RWST $ \_r _w _s -> pure x
  RWST f <*> RWST x = RWST $ \r w s -> f r w s <*> x r w s

instance Monad m => Monad (RWST r w s m) where
  return = pure
  RWST m >>= f = RWST $ \r w s -> do
    x <- m r w s
    let RWST m' = f x
    m' r w s

instance MonadTrans (RWST r w s) where
  lift m = RWST $ \_r _w _s -> m

instance MonadIO m => MonadIO (RWST r w s m) where
  liftIO = lift . liftIO

instance Monad m => MonadReader r (RWST r w s m) where
  ask = RWST $ \r _w _s -> pure r
  local f (RWST m) = RWST $ \r w s -> m (f r) w s

instance (MonadIO m, Monoid w) => MonadWriter w (RWST r w s m) where
  tell w2 = RWST $ \_r wRef _s -> liftIO $ do
    w1 <- readIORef wRef
    writeIORef wRef $! mappend w1 w2
  pass (RWST f) = RWST $ \r wRef s -> do
    (a, g) <- f r wRef s
    liftIO $ modifyIORef wRef g
    pure a
  listen (RWST m) = RWST $ \r _wRef s -> do
    ref <- liftIO $ newIORef mempty
    a <- m r ref s
    w <- liftIO $ readIORef ref
    liftIO $ modifyIORef ref (`mappend` w)
    pure (a, w)

instance MonadIO m => MonadState s (RWST r w s m) where
  get   = RWST $ \_r _w s -> liftIO $ readIORef  s
  put x = RWST $ \_r _w s -> liftIO $ writeIORef s $! x

instance (MonadIO m, Monoid w) => MonadRWS r w s (RWST r w s m)

------------------------------------------------------------------------------

zz :: IO [Int]
zz = do
  i1 <- newIORef [2::Int]
  i2 <- newIORef [3::Int]
  runRWST zzz [1::Int] i1 i2

zzz :: RWST [Int] [Int] [Int] IO [Int]
zzz = do
  a <- ask
  put a
  tell [4]
  return a

------------------------------------------------------------------------------
-- see
-- https://www.fpcomplete.com/blog/2015/04/announcing-monad-unlift
-- https://slpopejoy.github.io/posts/Effectful02.html
