{-# LANGUAGE FlexibleInstances #-}
-- Define a monad transformer ReaderT, such that the following works:

{-# LANGUAGE DeriveFunctor #-}

module RT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

type Reader r a = ReaderT r Identity

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r
    u  *> v = ReaderT $ \ r -> runReaderT u r  *> runReaderT v r
    u <*  v = ReaderT $ \ r -> runReaderT u r <*  runReaderT v r

instance (Monad m) => Monad (ReaderT r m) where
    return   = lift . return
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    m >> k = ReaderT $ \ r -> runReaderT m r >> runReaderT k r
    fail msg = lift (fail msg)

runReader :: Reader a r a -> r -> a
runReader r = runIdentity . runReaderT r

ask :: Monad m => ReaderT (m r) m a
ask = _

main :: IO ()
main = runReaderT main' "Hello World"

main' :: ReaderT String IO ()
main' = do
  lift $ putStrLn "I'm going to tell you a message"
  liftIO $ putStrLn "The message is:"
  message <- ask
  lift $ putStrLn message
