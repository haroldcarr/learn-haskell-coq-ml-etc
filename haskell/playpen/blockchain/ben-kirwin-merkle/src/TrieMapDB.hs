{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TrieMapDB
  (MapDB, runMapDB)
where

import           PPrelude

import           Control.Monad.State
import qualified Data.Map            as Map

type MapDB k v a = StateT (Map k v) Maybe a

runMapDB :: Ord k => DB k v a -> MapDB k v a
runMapDB = runDB putDB getDB
  where
    getDB key = do
      map <- get
      let value = Map.lookup key map
      lift value
    putDB key value = do
      map <- get
      let newMap = Map.insert key value map
      put newMap
