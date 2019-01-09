{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module XLogUserAccounts where

import qualified Prelude
import           Protolude

newtype Entry v = Entry
  { entryUserAccounts :: v
  } deriving Show

type Entries v = Map Text (Entry v)

-- | Interface for nodes to write log entries to storage.
class (Show (XWriteLogError m), Monad m) => XWriteLog m v where
  type XWriteLogError m
  writeLogEntries
    :: Exception (XWriteLogError m)
    => Entries v
    -> m (Either (XWriteLogError m) ())

type XLog           m v = XWriteLog m v
type XLogExceptions m   = Exception (XWriteLogError m)

-- | Possible errors that come from writing logs to/from persistent storage.
data XLogError m where
  XLogWriteError :: Show (XWriteLogError m) => XWriteLogError m -> XLogError m

deriving instance Show (XLogError m)

updateLog
  :: forall m v
   . ( XWriteLog m v, Exception (XWriteLogError m) )
  => Entries v
  -> m (Either (XLogError m) ())
updateLog _entries = Prelude.undefined
