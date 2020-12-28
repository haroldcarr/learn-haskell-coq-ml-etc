module X where

-- https://wiki.haskell.org/Simple_STM_example

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

-- this will always print 0 for before and after
-- because it adds 2 10 times and subtracts 1 20 times

main :: IO ()
main = do
  shared <- atomically $ newTVar (0::Int)
  showTVarIO shared "Before"
  _ <- forkIO $ 10 `replicateM_` (appV (2 +) shared           >> milliSleep 50)
  _ <- forkIO $ 20 `replicateM_` (appV pred  shared           >> milliSleep 25)
  _ <- forkIO $ 25 `replicateM_` (readTVarIO shared >>= print >> milliSleep 20)
  milliSleep 800
  showTVarIO shared "After"
 where
  appV fn x         = atomically $ readTVar x >>= writeTVar x . fn
  milliSleep        = threadDelay . (* 1000)
  showTVarIO tv msg = readTVarIO tv >>= putStrLn . ((msg ++ ": ") ++) . show
