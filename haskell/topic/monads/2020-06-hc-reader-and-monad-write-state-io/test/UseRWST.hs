{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}

module UseRWST where

------------------------------------------------------------------------------
import           DataForTest
------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad                  (when)
import           Control.Monad.Trans.RWS.Strict
------------------------------------------------------------------------------

programInts
  :: Monad m
  => Int
  -> RWST Int [Int] Int m Int
programInts stop = do
  x  <- ask
  n  <- get
  put (n + 1)
  n' <- get
  when (isTell n') (tell [n'+x])
  if n' == stop then get
  else programInts stop

runRWSTInts :: Bool -> IO (Int, [Int], Int)
runRWSTInts doPrint = do
  (a1,s1,w1) <- runRWST (programInts firstStop)  (1::Int) (0::Int)
  when doPrint (print (a1, w1, s1))

  (a2,s2,w2) <- runRWST (programInts secondStop) (1::Int) s1
  when doPrint (print (a2, w2, s2))

  pure (a2, w2++w1, s2)

------------------------------------------------------------------------------

programData
  :: (Monad m, RWRoundManager s a, RWBlockStore s a)
  => Int
  -> RWST Int [Int] s m Int
programData stop = do
  x            <- ask
  bsInner      %= (+ x)
  bsi          <- use (lBlockStore.bsInner)
  when (isTell bsi) (tell [x+bsi])
  rmEpochState %= (+ bsi)
  if bsi == stop then use (lRoundManager.rmBlockStore.bsInner)
  else programData stop

runRWSTData :: Bool-> IO (Int, [Int], RoundManager Int)
runRWSTData doPrint = do
  (a1,s1,w1) <- runRWST (programData firstStop)
                        (1::Int)
                        (RoundManager 100 (BlockStore 0 (3000::Int)))
  when doPrint (print (a1, w1, s1))

  (a2,s2,w2) <- runRWST (programData secondStop)
                        (1::Int)
                        s1
  when doPrint (print (a2, w2, s2))

  pure (a2, w2++w1, s2)

