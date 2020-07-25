{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module HC4 where

------------------------------------------------------------------------------
import MyMakeClassy
import HC3
------------------------------------------------------------------------------
import Data.IORef
import Control.Lens
import Protolude
------------------------------------------------------------------------------

data BlockStore a = BlockStore
  { _bsInner         :: Int
  , _bsStateComputer :: a
  } deriving (Eq, Show)
myMakeClassy ''BlockStore

data RoundManager a = RoundManager
  { _rmEpochState        :: Int
  , _rmBlockStore        :: !(BlockStore a)
  } deriving (Eq, Show)
myMakeClassy ''RoundManager

instance RWBlockStore (RoundManager a) a where
  lBlockStore = lens _rmBlockStore (\x y -> x { _rmBlockStore = y})

process
  :: ( Monad m, MonadRWS r w s m
     , RWRoundManager s a, RWBlockStore s a )
  => m Int
process = do
  bs <- use lBlockStore
  rm <- use lRoundManager
  bsInner      .=  99
  rmEpochState .= 999
  pure (bs^.bsInner + rm^.rmEpochState)

top4 :: IO ()
top4 = do
  ior <- newIORef ([]::[Text], RoundManager 10 (BlockStore 200 3000))
  a   <- runRWSIO process (-1::Int, ior)
  r   <- readIORef ior
  print (a, r :: ([Text], RoundManager Int))

