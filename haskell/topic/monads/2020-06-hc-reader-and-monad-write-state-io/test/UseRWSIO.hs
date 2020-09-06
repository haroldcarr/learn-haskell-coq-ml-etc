{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}

module UseRWSIO where

------------------------------------------------------------------------------
import           Control.Monad.RWSIO.Strict
------------------------------------------------------------------------------
import           DataForTest
------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad              (when)
------------------------------------------------------------------------------

programInts
  :: MonadRWS Int [Int] Int m
  => Int
  -> m Int
programInts stop = do
  x  <- ask'
  n  <- get
  put (n + 1)
  n' <- get
  when (isTell n') (tell [n'+x])
  if n' == stop then get
  else programInts stop

runMonadRWSInts :: Bool -> IO (Int, [Int], Int)
runMonadRWSInts doPrint = do
  (a1, s1, w1, x1) <- runMonadRWS0 (programInts firstStop) (1::Int) (0::Int)
  when doPrint (print (a1, w1, s1))

  (a2, s2, w2,  _) <- runMonadRWS  (programInts secondStop) x1
  when doPrint (print (a2, w2, s2))

  pure (a2, w2++w1, s2)

------------------------------------------------------------------------------

programData
  :: (MonadRWS Int [Int] s m, RWRoundManager s a, RWBlockStore s a)
  => Int
  -> m Int
programData stop = do
  x            <- ask'
  bsInner      %= (+ x)
  bsi          <- use (lBlockStore.bsInner)
  when (isTell bsi) (tell [x+bsi])
  rmEpochState %= (+ bsi)
  if bsi == stop then use (lRoundManager.rmBlockStore.bsInner)
  else programData stop

runMonadRWSData :: Bool-> IO (Int, [Int], RoundManager Int)
runMonadRWSData doPrint = do
  (a1, s1, w1, x1) <- runMonadRWS0 (programData firstStop)
                                   (1::Int)
                                   (RoundManager 100 (BlockStore 0 (3000::Int)))
  when doPrint (print (a1, w1, s1))

  (a2, s2, w2,  _) <- runMonadRWS  (programData secondStop) x1
  when doPrint (print (a2, w2, s2))

  pure (a2, w2++w1, s2)

