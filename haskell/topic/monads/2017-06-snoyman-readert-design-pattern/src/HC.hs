{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HC where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Test.Hspec

data MyEnv   = MyEnv String Float deriving (Eq, Show)
data MyState = MyState Int Double deriving (Eq, Show)

data Env = Env
  { envReader  :: MyEnv
  , envWriter  :: !(TVar [String])
  , envState   :: !(TVar MyState) }

class    HasReader            a where getReader :: a -> MyEnv
instance HasReader          Env  where getReader  = envReader

class    HasWriter            a  where getWriter :: a -> TVar [String]
instance HasWriter          Env  where getWriter  = envWriter

class HasState                a  where getState  :: a -> TVar MyState
instance HasState (TVar MyState) where getState = id
instance HasState           Env  where getState   = envState

class Monad m => MonadReaderX m where
  gtReader :: m MyEnv
instance (Monad m, HasReader env) => MonadReaderX (ReaderT env m) where
  gtReader = asks getReader

class Monad m => MonadWriterX m where
  modifyWriter :: ([String] -> [String]) -> m ()
instance (HasWriter env, MonadIO m) => MonadWriterX (ReaderT env m) where
  modifyWriter f = do
    env <- ask
    liftIO $ atomically $ modifyTVar' (getWriter env) f

class Monad m => MonadStateX m where
  modifyState :: (MyState -> MyState) -> m ()
instance (HasState env, MonadIO m) => MonadStateX (ReaderT env m) where
  modifyState f = do
    env <- ask
    liftIO $ atomically $ modifyTVar' (getState env) f

-- now way to do IO here
modify :: MonadStateX m => (MyState -> MyState) -> m ()
modify  = modifyState

tell :: MonadWriterX m => String -> m ()
tell m = modifyWriter (m:)

program :: (MonadReaderX m, MonadWriterX m, MonadStateX m) => m Int
program  = do
  MyEnv me _ <- gtReader
  tell ("Enter program " ++ me)
  modify (\(MyState i f) -> MyState (i + 2) f)
  pure 3

initAndStart :: IO ()
initAndStart  = do
  w      <- newTVarIO []
  s      <- newTVarIO (MyState 1 1.0)
  let ews = Env (MyEnv "HC" 9.9) w s
  r      <- runReaderT program ews
  w'     <- readTVarIO w
  s'     <- readTVarIO s
  print (w', s', r)

hctop :: IO ()
hctop = hspec test

test :: Spec
test  = describe "HC" $ do
  describe "modify" $
    it "works, IO" $ do
      w <- newTVarIO []
      s <- newTVarIO (MyState 1 1.0)
      runReaderT (modify (\(MyState i f) -> MyState (i + 2) f)) (Env (MyEnv "HC" 9.9) w s)
      res <- readTVarIO s
      res `shouldBe` MyState 3 1.0
  describe "tell" $
    it "works" $ do
      w <- newTVarIO []
      s <- newTVarIO (MyState 1 1.0)
      let msg1 = "Hello "
          msg2 = "World"
      runReaderT (tell msg1 >> tell msg2) (Env (MyEnv "HC" 9.9) w s)
      res <- readTVarIO w
      res `shouldBe` [msg2, msg1]
