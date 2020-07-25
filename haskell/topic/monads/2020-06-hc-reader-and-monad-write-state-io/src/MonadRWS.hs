{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module MonadRWS where

import           Control.Monad.Writer.Strict
import           Data.IORef
import           Protolude
------------------------------------------------------------------------------

type RWSRef r w s = (r, IORef (w, s))

newtype RWSIO r w s a = RWSIO { runRWSIO :: RWSRef r w s -> IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadReader (RWSRef r w s) )
  via ReaderT (RWSRef r w s) IO

instance MonadState s (RWSIO r w s) where
  get = do
    (_, ref)   <- ask
    (_w, s)    <- liftIO (readIORef ref)
    pure s

  put s' = do
    (_, ref)   <- ask
    (w, _s)    <- liftIO (readIORef ref)
    liftIO (writeIORef ref (w, s'))
    pure ()

instance Monoid w => MonadWriter w (RWSIO r w s) where
  tell x = do
    (_, ref)   <- ask
    (w, s)     <- liftIO (readIORef ref)
    liftIO (writeIORef ref (x<>w, s))
    pure ()
  listen m = do
    x@(_, ref) <- ask
    (w, _s)    <- liftIO (readIORef ref)
    a          <- liftIO (runRWSIO m x)
    pure (a, w)
  pass m = do
    x@(_, ref) <- ask
    (w, s)     <- liftIO (readIORef ref)
    (a, f)     <- liftIO (runRWSIO m x)
    liftIO (writeIORef ref (f w, s))
    pure a

ask' :: (Monad m, MonadReader (RWSRef r w s) m) => m r
ask'  = fst <$> ask

type MonadRWS r w s m =
  ( MonadReader (RWSRef r w s) m
  , MonadWriter           w    m
  , MonadState              s  m )

initMonadRWS :: (MonadIO m, Monoid w) => r -> s -> m (RWSRef r w s)
initMonadRWS r s = (r,) <$> liftIO (newIORef (mempty, s))

resetMonadRWS :: (MonadIO m, Monoid w) => RWSRef r w s -> s -> m ()
resetMonadRWS (_, ref) s =
  liftIO (writeIORef ref (mempty, s))

