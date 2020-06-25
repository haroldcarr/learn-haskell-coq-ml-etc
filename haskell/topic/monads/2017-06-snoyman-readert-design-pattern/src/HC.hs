{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module HC where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import qualified Control.Monad.Reader   as MReader
import           Test.Hspec

data MyEnv   = MyEnv   String Float  deriving (Eq, Show)
data MyState = MyState Int    Double deriving (Eq, Show)

data Env = Env
  { envReader :: !MyEnv
  , envWriter :: !(TVar [String])
  , envState  :: !(TVar MyState)
  , envPrint  :: forall a. Show a => a -> IO () }

class    HasReader              a  where getReader :: a -> MyEnv
instance HasReader            Env  where getReader  = envReader

class    HasWriter              a  where getWriter :: a -> TVar [String]
instance HasWriter (TVar [String]) where getWriter  = id
instance HasWriter            Env  where getWriter  = envWriter

class    HasState               a  where getState  :: a -> TVar MyState
instance HasState  (TVar MyState)  where getState   = id
instance HasState             Env  where getState   = envState

class    HasPrint               a  where getPrint  :: a -> (Show x => x -> IO ())
instance HasPrint             Env  where getPrint   = envPrint

class Monad m => MonadReaderX m where
  ask :: m MyEnv

instance (Monad   m, HasReader env) => MonadReaderX (MReader.ReaderT env m) where
  ask  = MReader.asks getReader

class Monad m => MonadWriterX m where
  tell :: String -> m ()

instance (MonadIO m, HasWriter env) => MonadWriterX (MReader.ReaderT env m) where
  tell s = do
    env <- MReader.ask
    liftIO $ atomically $ modifyTVar' (getWriter env) (s:)

class Monad m => MonadStateX m where
  put    ::  MyState ->             m ()
  get    ::                         m MyState
  modify :: (MyState -> MyState) -> m ()

instance (MonadIO m, HasState env) => MonadStateX (MReader.ReaderT env m) where
  put s = do
    env <- MReader.ask
    liftIO $ atomically $ writeTVar   (getState env) s
  get = do
    env <- MReader.ask
    liftIO $ readTVarIO               (getState env)
  modify f = do
    env <- MReader.ask
    liftIO $ atomically $ modifyTVar' (getState env) f

class MonadIO m => MonadPrintX m where
  doPrint ::  Show a => a -> m ()

instance (HasPrint env, MonadIO m) => MonadPrintX (MReader.ReaderT env m) where
  doPrint a = do
    env <- MReader.ask
    liftIO $ getPrint env a

type RWSTX m = (MonadReaderX m, MonadWriterX m, MonadStateX m)

program :: (RWSTX m, MonadPrintX m) => m Int
program  = do
  MyEnv me _ <- ask
  tell ("Enter program " ++ me)
  modify (\(MyState i f) -> MyState (i + 2) f)
  tell "after modifyState"
  doPrint "doPrint"
  pure 3

initAndRunProgram :: IO ()
initAndRunProgram  = do
  w      <- newTVarIO []
  s      <- newTVarIO (MyState 1 1.0)
  let ews = Env (MyEnv "HC" 9.9) w s print
  r      <- MReader.runReaderT program ews
  w'     <- readTVarIO w
  s'     <- readTVarIO s
  putStrLn "\n"
  print (w', s', r)
  putStrLn "\n"

hctop :: IO ()
hctop = hspec test

test :: Spec
test  = describe "HC" $ do
  describe "modify" $
    it "works, IO" $ do
      w <- newTVarIO []
      s <- newTVarIO (MyState 1 1.0)
      MReader.runReaderT (modify (\(MyState i f) -> MyState (i + 2) f))
                         (Env (MyEnv "HC" 9.9) w s print)
      res <- readTVarIO s
      res `shouldBe` MyState 3 1.0
  describe "tell" $
    it "works" $ do
      w <- newTVarIO []
      s <- newTVarIO (MyState 1 1.0)
      let msg1 = "Hello "
          msg2 = "World"
      MReader.runReaderT (tell msg1 >> tell msg2) (Env (MyEnv "HC" 9.9) w s print)
      res <- readTVarIO w
      res `shouldBe` [msg2, msg1]
