{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TemplateHaskell        #-}

module DataForTest where

------------------------------------------------------------------------------
import           MyMakeClassy
------------------------------------------------------------------------------
import           Control.Lens
------------------------------------------------------------------------------

firstStop, secondStop :: Int
firstStop  = 10000000
secondStop = 20000000

isTell :: Int -> Bool
isTell i = i `mod` 1000000 == 0
{-
firstStop, secondStop :: Int
firstStop  = 1000
secondStop = 2000

isTell :: Int -> Bool
isTell i = i `mod` 100 == 0
-}
------------------------------------------------------------------------------

data BlockStore a = BlockStore
  { _bsInner         :: Int
  , _bsStateComputer :: a
  } deriving (Eq, Show)
myMakeClassy ''BlockStore

data RoundManager a = RoundManager
  { _rmEpochState :: Int
  , _rmBlockStore :: BlockStore a
  } deriving (Eq, Show)
myMakeClassy ''RoundManager

instance RWBlockStore (RoundManager a) a where
  lBlockStore = lens _rmBlockStore (\x y -> x { _rmBlockStore = y})

