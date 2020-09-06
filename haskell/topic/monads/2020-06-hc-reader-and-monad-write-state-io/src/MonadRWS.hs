{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}

module MonadRWS where

import           Control.Monad.Writer.Strict
import           Data.IORef
import           Protolude
------------------------------------------------------------------------------

type RWSRef r w s = (r, IORef (w, s))

newtype RWSIO r w s a = RWSIO { runRWSIO :: RWSRef r w s -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (RWSRef r w s))
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
    liftIO (writeIORef ref (w<>x, s))
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

runMonadRWS0
  :: (MonadIO m, Monoid w)
  => RWSIO r w s a -> r -> s
  -> m (a, s, w, RWSRef r w s)
runMonadRWS0 act r s = do
  x@(_,ref) <- initMonadRWS r s
  a         <- liftIO (runRWSIO act x)
  (w, s')   <- liftIO (readIORef ref)
  pure (a, s', w, x)

-- | Typical usage: 'initMonadRWS' followed by one or more 'runMonadRWS'
runMonadRWS
  :: MonadIO m
  => RWSIO r w s a -> RWSRef r w s
  -> m (a, s, w, RWSRef r w s)
runMonadRWS act x@(_,ref) = do
  a         <- liftIO (runRWSIO act x)
  (w, s')   <- liftIO (readIORef ref)
  pure (a, s', w, x)

runMonadRWS'
  :: (MonadIO m, Monoid w)
  => RWSIO r w s a -> s -> RWSRef r w s
  -> m (a, s, w, RWSRef r w s)
runMonadRWS' act s x@(_,ref) = do
  liftIO (resetMonadRWS x s)
  a         <- liftIO (runRWSIO act x)
  (w, s')   <- liftIO (readIORef ref)
  pure (a, s', w, x)


