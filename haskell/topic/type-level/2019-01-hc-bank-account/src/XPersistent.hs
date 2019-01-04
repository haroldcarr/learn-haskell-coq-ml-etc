{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module XPersistent where

import           Protolude

-- | Interface to read and write the persistent state to disk.
class Monad m => Persist m where
  type PersistError m
  readPersistentState
    :: Exception (PersistError m)
    => m (Either (PersistError m) PersistentState)
  writePersistentState
    :: Exception (PersistError m)
    => PersistentState -> m (Either (PersistError m) ())

newtype PersistentState = PersistentState
  { psX :: Int
  } deriving (Read, Show)

initPersistentState :: PersistentState
initPersistentState = PersistentState
  { psX = 0
  }
