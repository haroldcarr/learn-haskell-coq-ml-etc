{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HC3 where

import Data.IORef
import Protolude
import Control.Monad.Writer.Strict
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

programHc3
  :: MonadRWS Int [Int] Int m
  => m Int
programHc3 = do
  x <- ask'
  n <- get
  tell [n+x]
  if n == 10 then pure n
  else put (n + 1) >> programHc3

top3 :: IO ()
top3 = do
  ior <- newIORef ([0::Int], 1::Int)
  a   <- runRWSIO (programHc3) (1::Int, ior)
  r   <- readIORef ior
  print (a, r)
