{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.IORef
import Protolude

-- https://gist.github.com/friedbrice/520f627d927cb658c587bd3cdb6cf4dc

-- 'm' is abstract
-- 'program' works in any 'm' that implements 'MonadState Int m'
programHc :: (MonadState Int m, MonadHcPrint m) => m ()
programHc = do
  n <- get
  hcPrint n
  if n == 10 then  pure ()
  else put (n + 1) >> programHc

-- Testing will use 'State Int' for 'm'
test :: IO ()
test =
  let result = execState (programHc :: State Int ()) 0
      expect = 10
  in
    if result == expect then putStrLn ("Pass" :: Text)
    else panic ("Failed: " <> "expected: " <> show expect <> ", to: " <> show result)

-- for production
newtype App a = App { runApp :: IORef Int -> IO a }
  deriving (
    Functor, Applicative, Monad,
    -- 'MonadIO' supplies 'liftIO'
    -- That means 'App' can use an 'IO' operation.
    MonadIO,
    -- 'MonadReader (IORef Int)' supplies 'ask :: App (IORef Int)'
    -- enabling accesses to the 'IORef Int'
    MonadReader (IORef Int)
    -- Derive all the above instances because these instances already exist for
    -- 'ReaderT (IORef Int) IO' and because 'App a' and 'ReaderT (IORef Int) IO a'
    -- have identical underlying implementations: 'IORef Int -> IO a'
    ) via ReaderT (IORef Int) IO

-- implement 'MonadState Int App' so 'program' can use 'App' instad of abstract 'm'
instance MonadState Int App where
  get :: App Int
  get = do
    stateRef     <- ask
    currentState <- liftIO (readIORef stateRef)
    return currentState

  put :: Int -> App ()
  put newState = do
    stateRef <- ask
    liftIO (writeIORef stateRef newState)
    return ()

production :: IO ()
production  = do
  stateRef <- newIORef 0
  runApp (programHc :: App ()) stateRef
  endState <- readIORef stateRef
  print endState

p :: IO ()
p  = production

-------------------------

class Monad m => MonadHcPrint m where
  hcPrint :: Show a => a -> m ()

instance MonadHcPrint App where
  hcPrint a = liftIO (print a)
  {-# NOINLINE hcPrint #-}

instance MonadHcPrint (StateT Int Identity) where
  hcPrint _ = pure ()

