{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module HC4 where

------------------------------------------------------------------------------
import           HC3
import           MyMakeClassy
------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Writer.Strict
import           Data.IORef
import           Protolude
------------------------------------------------------------------------------

data BlockStore a = BlockStore
  { _bsInner         :: Int
  , _bsStateComputer :: a
  } deriving (Eq, Show)
myMakeClassy ''BlockStore

data RoundManager a = RoundManager
  { _rmEpochState :: Int
  , _rmBlockStore :: !(BlockStore a)
  } deriving (Eq, Show)
myMakeClassy ''RoundManager

instance RWBlockStore (RoundManager a) a where
  lBlockStore = lens _rmBlockStore (\x y -> x { _rmBlockStore = y})

process
  :: ( Monad m, MonadRWS r [Text] s m
     , RWRoundManager s a, RWBlockStore s a )
  => m Int
process = do
  bs <- use lBlockStore
  rm <- use lRoundManager
  tell ["Hello"]
  bsInner      .=  99
  rmEpochState .= 999
  pure (bs^.bsInner + rm^.rmEpochState)

top4 :: IO ()
top4 = do
  x@(_, ref)       <- liftIO (initMonadRWS (-1::Int)
                                           (RoundManager 10 (BlockStore 200 (3000::Int))))
  a1               <- runRWSIO process x
  (w1::[Text], s1) <- readIORef ref
  print (a1, w1, s1)

  liftIO (resetMonadRWS x s1)
  a2               <- runRWSIO process x
  (w2::[Text], s2) <- readIORef ref
  print (a2, w2, s2)
