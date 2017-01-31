{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module TheSpec where

import           Control.Lens           hiding (Index, (|>))
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.ByteString.Char8  (putStrLn)
import           GHC.Generics           hiding (from)
import           Prelude                hiding (putStrLn)

class State s where

data Spec m = Spec
  {
    _applyLogEntry   :: forall s. State s => s -> Command -> m (s, CommandResult)
  }

newtype CommandEntry = CommandEntry { unCommandEntry :: ByteString }
  deriving (Show, Eq, Ord, Generic)

newtype CommandResult = CommandResult { unCommandResult :: ByteString }
  deriving (Show, Eq, Ord, Generic)

data Command = Command
  { _cmdEntry      :: !CommandEntry
  }
  deriving (Show, Eq, Generic)
makeLenses ''Command

spec :: MonadIO m
     => (forall s. State s => s -> Command -> m (s, CommandResult))
     -> Spec m
spec applyFn = Spec
    {
      _applyLogEntry   = applyFn
    }

mkSpec :: MonadIO m
    => (forall s. State s => ((String -> IO ()) -> s -> Command -> IO (s, CommandResult)))
    -> Spec m
mkSpec applyFn =
  spec (liftIO2 (applyFn (\_ -> return ())))

run :: MonadIO m
    => Spec m
    -> (forall s. State s => s -> Command -> m (s, CommandResult))
run spec@Spec{..}s c = _applyLogEntry s c

liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f a = liftIO . f a
