{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HC where

import Data.IORef
import Protolude

newtype SIO s a = SIO { runSIO :: IORef s -> IO a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadReader (IORef s) )
  via ReaderT (IORef s) IO

instance MonadState s (SIO s) where
  get = do
    ref <- ask
    a   <- liftIO (readIORef ref)
    return a
  put s' = do
    ref <- ask
    liftIO (writeIORef ref s')
    return ()

data MyState = MyState
  { one :: Int
  , two :: [Text]
  } deriving Show

programHc :: MonadState MyState m => m MyState
programHc = do
  s@(MyState n x) <- get
  if n == 10 then pure s
  else put (MyState (n + 1) (show n : x)) >> programHc

top :: IO ()
top = do
  ior <- newIORef (MyState 0 ["HC"])
  a   <- runSIO programHc ior
  print a

------------------------------------------------------------------------------

{-
data MyState a = MyState
  { one :: a
  , two :: Text }

newtype RWSIO e w s a = RWSIO { runRWSIO :: IORef (w, s) -> IO a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (IORef (w, s)) )
  via ReaderT (IORef (w, s)) IO

type RWSRef e w s = (e, IORef (w, s))

newtype RWSIO e w s a = RWSIO { runRWSIO :: RWSRef e w s -> IO a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (RWSRef e w s) )
  via ReaderT (RWSRef e w s) IO

instance MonadState (RWSRef e w s) (RWSIO e w s) where

  get = do
    a@(_, ref) <- ask
    (_w, _s)   <- liftIO (readIORef ref)
    return a

  put (e, ref) = do
    (e, ref) <- ask
    (w, _) <- liftIO (readIORef ref)
    liftIO (writeIORef ref (w, _))
    return ()

programHc :: MonadState Int m => m ()
programHc = do
  n <- get
  if n == 10 then  pure ()
  else put (n + 1) >> programHc

top = do
  ior <- newIORef ([0::Int], 1::Int)
  let a = runRWSIO (programHc) (2::Int, ior)
  print a
-}
