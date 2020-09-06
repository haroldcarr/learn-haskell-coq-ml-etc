{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}

module UseRWSTIO where

------------------------------------------------------------------------------
import           Control.Monad.RWSIO.StrictX
------------------------------------------------------------------------------
import           DataForTest
------------------------------------------------------------------------------
--import           Control.Lens
import           Control.Monad                  (when)
-- import Debug.Trace
------------------------------------------------------------------------------
{-
programInts
  :: Int
  -> RWSTIO Int [Int] Int Int
programInts stop = do
  x  <- trace "ask" ask
  n  <- trace "get1" get
  trace "put" (put (n + 1))
  n' <- trace ("get2 " ++ show n)  get
  when (trace ("isTell " ++ show n' ++ " " ++ show (isTell n')) (isTell n'))
       (trace "tellXXX" (tell [n'+x]))
  if trace ("if " ++ show n' ++ " " ++ show stop) n' == stop then get
  else programInts stop
-}
programInts
  :: Int
  -> RWSTIO Int [Int] Int Int
programInts stop = do
  x  <- ask
  n  <- get
  put (n + 1)
  n' <- get
  when (isTell n') (tell [n'+x])
  if n' == stop then get
  else programInts stop

runRWSTIOInts :: Bool -> IO (Int, [Int], Int)
runRWSTIOInts doPrint = do
  (a1,s1,w1,x1) <- runRWSTIO0 (programInts firstStop)  (1::Int) (0::Int)
  when doPrint (print (a1, w1, s1))

  (a2,s2,w2, _) <- runRWSTIO  (programInts secondStop) x1
  when doPrint (print (a2, w2, s2))

  pure (a2, w2++w1, s2)

------------------------------------------------------------------------------
{-
programData
  :: (RWRoundManager s a, RWBlockStore s a)
  => Int
  -> RWSTIO Int [Int] s Int
programData stop = do
  x            <- ask
  bsInner      %= (+ x)
  bsi          <- use (lBlockStore.bsInner)
  when (isTell bsi) (tell [x+bsi])
  rmEpochState %= (+ bsi)
  if bsi == stop then use (lRoundManager.rmBlockStore.bsInner)
  else programData stop

runRWSTIOData :: Bool-> IO (Int, [Int], RoundManager Int)
runRWSTIOData doPrint = do
  (a1,s1,w1,x1) <- runRWSTIO0 (programData firstStop)
                              (1::Int)
                              (RoundManager 100 (BlockStore 0 (3000::Int)))
  when doPrint (print (a1, w1, s1))

  (a2,s2,w2, _) <- runRWSTIO  (programData secondStop)
                              x1
  when doPrint (print (a2, w2, s2))

  pure (a2, w2++w1, s2)
-}
