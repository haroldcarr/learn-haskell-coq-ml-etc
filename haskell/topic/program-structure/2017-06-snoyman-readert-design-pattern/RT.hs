{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RT where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Control.Monad.State.Strict as State
import           Test.Hspec

data Env = Env
  { envLog     :: !(String -> IO ())
  , envBalance :: !(TVar Int)
  }

class HasLog a where
  getLog :: a -> (String -> IO ())
instance HasLog (String -> IO ()) where
  getLog = id
instance HasLog Env where
  getLog = envLog

class HasBalance a where
  getBalance :: a -> TVar Int
instance HasBalance (TVar Int) where
  getBalance = id
instance HasBalance Env where
  getBalance = envBalance

class Monad m => MonadBalance m where
  modifyBalance :: (Int -> Int) -> m ()
instance (HasBalance env, MonadIO m) => MonadBalance (ReaderT env m) where
  modifyBalance f = do
    env <- ask
    liftIO $ atomically $ modifyTVar' (getBalance env) f
instance Monad m => MonadBalance (State.StateT Int m) where
  modifyBalance = State.modify

-- Type says IO can't happen here
modify
  :: MonadBalance m
  => (Int -> Int)
  -> m ()
modify = modifyBalance

logSomething
  :: (MonadReader env m, HasLog env, MonadIO m)
  => String
  -> m ()
logSomething msg = do
  env <- ask
  liftIO $ getLog env msg

main2 :: IO ()
main2 = do
  var <- newTVarIO (1 :: Int)
  let env = Env putStrLn var
  xx <- State.runStateT (runReaderT x env) 0
  print xx

x :: ReaderT Env (State.StateT Int IO) ()
x = do
  somethingThatModifies
  somethingThatLogs

somethingThatLogs :: (MonadIO m, HasLog env) => ReaderT env m ()
somethingThatLogs = logSomething "HELLO"

somethingThatModifies
  :: (MonadIO m, HasBalance env)
  => ReaderT env (State.StateT Int m) ()
somethingThatModifies = modify (*2)

main :: IO ()
main = hspec $ do
  describe "modify" $ do
    it "IO" $ do
      var <- newTVarIO (1 :: Int)
      runReaderT (modify (+ 2)) var
      res <- readTVarIO var
      res `shouldBe` 3
    it "pure" $ do
        let res = State.execState (modify (+ 2)) (1 :: Int)
        res `shouldBe` 3
  describe "logSomething" $
    it "works" $ do
      var <- newTVarIO ""
      let logFunc msg = atomically $ modifyTVar var (++ msg)
      let msg1 = "Hello "
      let msg2 = "World\n"
      runReaderT (logSomething msg1 >> logSomething msg2) logFunc
      res <- readTVarIO var
      res `shouldBe` (msg1 ++ msg2)

