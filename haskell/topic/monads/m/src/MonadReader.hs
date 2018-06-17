{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module MonadReader
  ( MonadReader(..)
  , MonadReader.asks
    -- * Reader
  , Reader
  , runReader
  , mapReader
    -- * ReaderT
  , ReaderT (ReaderT)
  , runReaderT
  , mapReaderT
  ) where

import           MonadTrans
import           ReaderT         (Reader (..), ReaderT (..), ask, asks, local,
                                  mapReader, mapReaderT, runReader)
import           StateT

------------------------------------------------------------------------------
-- mtl
-- Control.Monad.Reader

class Monad m => MonadReader r m | m -> r where
    ask      :: m r
    ask       = reader id

    local    :: (r -> r) -> m a -> m a

    reader   :: (r -> a) -> m a
    reader f  = do
        r <- MonadReader.ask
        return (f r)

asks         :: MonadReader r m => (r -> a) -> m a
asks          = reader

-- | partially applied function type is a simple reader monad
instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f
    reader    = id

instance Monad m => MonadReader r (ReaderT r m) where
    ask       = ReaderT.ask
    local     = ReaderT.local
    reader    = ReaderT.asks

instance MonadReader r m => MonadReader r (StateT s m) where
    ask       = lift MonadReader.ask
    local     = mapStateT . MonadReader.local
    reader    = lift . reader
