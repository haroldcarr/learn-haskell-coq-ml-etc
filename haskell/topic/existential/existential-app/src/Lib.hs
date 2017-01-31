{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.ByteString.Char8  (putStrLn)
import           Prelude                hiding (putStrLn)
import           TheSpec

data AppState
  = AppState String
  deriving (Eq, Show)

instance (State AppState)

appState :: AppState
appState = AppState "harold"

appApplyFn :: (forall s. State s => ((String -> IO ()) -> s -> Command -> IO (s, CommandResult)))
appApplyFn _ s c = do
  putStrLn (unCommandEntry (_cmdEntry c))
  return (s, CommandResult "world")

appApplyFn' :: (String -> IO ()) -> AppState -> Command -> IO (AppState, CommandResult)
appApplyFn' _ s c = do
  putStrLn (unCommandEntry (_cmdEntry c))
  return (s, CommandResult "world")

appSpec :: MonadIO m => Spec m
appSpec = mkSpec appApplyFn

-- appSpec' :: MonadIO m => Spec m
-- appSpec' = mkSpec appApplyFn'
