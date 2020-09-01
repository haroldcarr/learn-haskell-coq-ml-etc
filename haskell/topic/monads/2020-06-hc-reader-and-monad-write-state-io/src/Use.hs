{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Use where

------------------------------------------------------------------------------
import           MonadRWS
import           MyMakeClassy
------------------------------------------------------------------------------
import           Control.Lens
import qualified Control.Monad.Trans.RWS.Strict as RWST
import           Control.Monad.Writer.Strict
import           Data.IORef
import qualified Prelude
import           Protolude
------------------------------------------------------------------------------
{-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}
------------------------------------------------------------------------------

firstStop, secondStop :: Int
firstStop  = 10000000
secondStop = 20000000

isTell :: Int -> Bool
isTell i = i `mod` 1000000 == 0

------------------------------------------------------------------------------

program1
  :: MonadRWS Int [Int] Int m
  => Int
  -> m Int
program1 !stop = do
  !x  <- ask'
  !n  <- get
  put (n + 1)
  !n' <- get
  when (isTell n') (tell [n'+x])
  if n' == stop then get
  else program1 stop

top1 :: Bool -> IO (Int, [Int], Int)
top1 !doPrint = do
  x@(_,!ref) <- liftIO (initMonadRWS (1::Int) (0::Int))
  !a1        <- runRWSIO (program1 firstStop) x
  (!w1, !s1) <- readIORef ref
  when doPrint (print (a1, w1, s1))

  liftIO (resetMonadRWS x s1)
  !a2        <- runRWSIO (program1 secondStop) x
  (!w2, !s2) <- readIORef ref
  when doPrint (print (a2, w2, s2))
  pure (a2, w2++w1, s2)

------------------------------------------------------------------------------

data BlockStore a = BlockStore
  { _bsInner         :: !Int
  , _bsStateComputer :: !a
  } deriving (Eq, Show)
myMakeClassy ''BlockStore

data RoundManager a = RoundManager
  { _rmEpochState :: !Int
  , _rmBlockStore :: !(BlockStore a)
  } deriving (Eq, Show)
myMakeClassy ''RoundManager

instance RWBlockStore (RoundManager a) a where
  lBlockStore = lens _rmBlockStore (\x y -> x { _rmBlockStore = y})

program2
  :: (MonadRWS Int [Int] s m, RWRoundManager s a, RWBlockStore s a)
  => Int
  -> m Int
program2 !stop = do
  !x           <- ask'
  bsInner      %= (+ x)
  !bsi         <- use (lBlockStore.bsInner)
  when (isTell bsi) (tell [x+bsi])
  rmEpochState %= (+ bsi)
  if bsi == stop then use (lRoundManager.rmBlockStore.bsInner)
  else program2 stop

top2 :: Bool-> IO (Int, [Int], RoundManager Int)
top2 doPrint = do
  x@(_,!ref) <- liftIO (initMonadRWS (1::Int)
                                     (RoundManager 100 (BlockStore 0 (3000::Int))))
  !a1        <- runRWSIO (program2 firstStop) x
  (!w1, !s1) <- readIORef ref
  when doPrint (print (a1, w1, s1))

  liftIO (resetMonadRWS x s1)
  !a2        <- runRWSIO (program2 secondStop) x
  (!w2, !s2) <- readIORef ref
  when doPrint (print (a2, w2, s2))
  pure (a2, w2++w1, s2)

------------------------------------------------------------------------------

program3
  :: (Monad m, RWRoundManager s a, RWBlockStore s a)
  => Int
  -> RWST.RWST Int [Int] s m Int
program3 !stop = do
  !x           <- ask
  bsInner      %= (+ x)
  !bsi         <- use (lBlockStore.bsInner)
  when (isTell bsi) (tell [x+bsi])
  rmEpochState %= (+ bsi)
  if bsi == stop then use (lRoundManager.rmBlockStore.bsInner)
  else program3 stop

top3 :: Bool-> IO (Int, [Int], RoundManager Int)
top3 doPrint = do
  (!a1,!s1,!w1)<- RWST.runRWST (program3 firstStop)
                               (1::Int)
                               (RoundManager 100 (BlockStore 0 (3000::Int)))
  when doPrint (print (a1, w1, s1))

  (!a2,!s2,!w2)<- RWST.runRWST (program3 secondStop)
                               (1::Int)
                               s1
  when doPrint (print (a2, w2, s2))
  pure (a2, w2++w1, s2)

