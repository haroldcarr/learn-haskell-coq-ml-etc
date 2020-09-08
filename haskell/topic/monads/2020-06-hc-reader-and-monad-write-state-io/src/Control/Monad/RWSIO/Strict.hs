{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}

module Control.Monad.RWSIO.Strict
  ( MonadRWS
  , RWSRef
  , RWSIO (..)
  , ask', get, tell, put
  , initMonadRWS
  , resetMonadRWS
  , resetMonadRWSWriter
  , runMonadRWS0
  , runMonadRWS
  , runMonadRWS'
  )
where

import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class (MonadReader, ask)
import           Control.Monad.State.Class  (MonadState, get, put)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Writer.Class (MonadWriter (..), tell)
import           Data.IORef
------------------------------------------------------------------------------

type RWSRef r w s = (r, IORef (w, s))

newtype RWSIO r w s a = RWSIO { unRWSIO :: RWSRef r w s -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (RWSRef r w s))
  via ReaderT (RWSRef r w s) IO

instance MonadState s (RWSIO r w s) where
  get = do
    (_, ref)   <- ask
    (_w, s)    <- liftIO (readIORef ref)
    pure s

  put s' = do
    (_, ref)   <- ask
    liftIO $ do
      (w, _s)  <- readIORef ref
      writeIORef ref (w, s')
      pure ()

instance Monoid w => MonadWriter w (RWSIO r w s) where
  tell x = do
    (_, ref)   <- ask
    liftIO $ do
      (w, s)   <- readIORef ref
      writeIORef ref (w<>x, s)
      pure ()

  listen m = do
    x@(_, ref) <- ask
    liftIO $ do
      (w, _s)  <- readIORef ref
      a        <- unRWSIO m x
      pure (a, w)

  pass m = do
    x@(_, ref) <- ask
    liftIO $ do
      (w, s)   <- readIORef ref
      (a, f)   <- unRWSIO m x
      writeIORef ref (f w, s)
      pure a

ask' :: (Monad m, MonadReader (RWSRef r w s) m) => m r
ask'  = fst <$> ask

type MonadRWS r w s m =
  ( MonadReader (RWSRef r w s) m
  , MonadWriter           w    m
  , MonadState              s  m )

initMonadRWS :: (MonadIO m, Monoid w) => r -> s -> m (RWSRef r w s)
initMonadRWS r s =      (r,) <$> liftIO (newIORef                     (mempty, s))

resetMonadRWS :: (MonadIO m, Monoid w) => RWSRef r w s -> s -> m ()
resetMonadRWS       (_, ref) s = liftIO (writeIORef   ref             (mempty, s))

resetMonadRWSWriter :: (MonadIO m, Monoid w) => RWSRef r w s -> m ()
resetMonadRWSWriter (_, ref)   = liftIO (modifyIORef' ref (\(_, s) -> (mempty, s)))

runMonadRWS0
  :: (MonadIO m, Monoid w)
  => RWSIO r w s a -> r -> s
  -> m (a, s, w, RWSRef r w s)
runMonadRWS0 act r s = liftIO $ do
  x@(_,ref) <- initMonadRWS r s
  a         <- unRWSIO act x
  (w, s')   <- readIORef ref
  pure (a, s', w, x)

-- | Typical usage: 'initMonadRWS' followed by one or more 'runMonadRWS'
runMonadRWS
  :: (MonadIO m, Monoid w)
  => RWSIO r w s a -> RWSRef r w s
  -> m (a, s, w, RWSRef r w s)
runMonadRWS act x@(_,ref) = liftIO $ do
  resetMonadRWSWriter x
  a         <- unRWSIO act x
  (w, s')   <- readIORef ref
  pure (a, s', w, x)

runMonadRWS'
  :: (MonadIO m, Monoid w)
  => RWSIO r w s a -> s -> RWSRef r w s
  -> m (a, s, w, RWSRef r w s)
runMonadRWS' act s x@(_,ref) = liftIO $ do
  resetMonadRWS x s
  a         <- unRWSIO act x
  (w, s')   <- readIORef ref
  pure (a, s', w, x)


