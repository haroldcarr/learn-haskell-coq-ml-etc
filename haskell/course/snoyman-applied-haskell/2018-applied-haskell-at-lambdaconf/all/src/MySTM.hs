module MySTM where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Say

main :: IO ()
main = do
  aliceVar <- newTVarIO (0::Int)
  bobVar <- newTVarIO (0::Int)

  _ <- forkIO $ payAlice aliceVar

  atomically $ do
    currentAlice <- readTVar aliceVar
    check (currentAlice >= 20)
    writeTVar aliceVar (currentAlice - 20)
    currentBob <- readTVar bobVar
    writeTVar bobVar (currentBob + 20)

  (finalAlice, finalBob) <- atomically $ do
    finalAlice <- readTVar aliceVar
    finalBob   <- readTVar bobVar
    return (finalAlice, finalBob)

  sayString $ "Final Alice: " ++ show finalAlice
  sayString $ "Final Bob: " ++ show finalBob

payAlice :: TVar Int -> IO ()
payAlice aliceVar = forever $ do
  threadDelay 1000000
  atomically $ do
    current <- readTVar aliceVar
    writeTVar aliceVar (current + 5)
  sayString "Paid Alice"

