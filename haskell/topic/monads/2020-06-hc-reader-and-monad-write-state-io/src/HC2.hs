{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HC2 where

import Data.IORef
import Protolude
import Control.Monad.Writer.Strict
------------------------------------------------------------------------------

newtype RWSIO r w s a = RWSIO { runRWSIO :: IORef (r, w, s) -> IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadReader (IORef (r, w, s)) )
  via ReaderT (IORef (r, w, s)) IO

instance MonadState s (RWSIO r w s) where
  get = do
    ref         <- ask
    (_r, _w, s) <- liftIO (readIORef ref)
    pure s

  put s' = do
    ref <- ask
    (r, w, _s)  <- liftIO (readIORef ref)
    liftIO (writeIORef ref (r, w, s'))
    pure ()

instance Monoid w => MonadWriter w (RWSIO r w s) where
  tell x = do
    ref         <- ask
    (r, w, s)   <- liftIO (readIORef ref)
    liftIO (writeIORef ref (r, x<>w, s))
    pure ()
  listen m = do
    ref         <- ask
    (_r, w, _s) <- liftIO (readIORef ref)
    a           <- liftIO (runRWSIO m ref)
    pure (a, w)
  pass m = do
    ref         <- ask
    (r, w, s)   <- liftIO (readIORef ref)
    (a, f)      <- liftIO (runRWSIO m ref)
    liftIO (writeIORef ref (r, f w, s))
    pure a

ask' :: (MonadIO m, MonadReader (IORef (r, w, s)) m) => m r
ask'  = do
  ref           <- ask
  (r, _w, _s)   <- liftIO (readIORef ref)
  pure r

programHc2
  :: ( MonadReader (IORef (r, [Int], Int)) m
     , MonadWriter            [Int]        m
     , MonadState                    Int   m )
  => m Int
programHc2 = do
  _x <- ask
  n <- get
  tell [n]
  if n == 10 then  pure n
  else put (n + 1) >> programHc2

top2 :: IO ()
top2 = do
  ior <- newIORef (1::Int, [0::Int], 1::Int)
  a <- runRWSIO (programHc2) ior
  r <- readIORef ior
  print (a, r)

