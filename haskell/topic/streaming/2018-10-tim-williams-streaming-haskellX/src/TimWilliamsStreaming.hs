{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}

module TimWilliamsStreaming where

import Control.Monad.Trans

-- ListT done right

-- list elements 'a' interleaved with effect 'm'
newtype ListT (m :: * -> *) (a :: *) = ListT { runListT :: m (Step m a) }
  deriving Functor

data Step (m :: * -> *) (a :: *)
  = Cons a (ListT m a)
  | Nil
  deriving Functor

instance Monad m => Semigroup (ListT (m :: * -> *) (a :: *)) where
  (<>) (ListT mstep) s' = ListT $ mstep >>= \case
    Cons a s -> return $ Cons a (s `mappend` s')
    Nil      -> runListT s'

instance Monad m => Monoid (ListT (m :: * -> *) (a :: *)) where
  mempty  = ListT $ return Nil
  mappend = (<>)

concat :: Monad m => ListT m (ListT (m :: * -> *) (a :: *)) -> ListT m a
concat (ListT mstep) = ListT $ mstep >>= \case
  Cons s ss -> runListT $ s `mappend` TimWilliamsStreaming.concat ss
  Nil       -> return Nil

instance Monad m => Applicative (ListT (m :: * -> *)) where
  pure a = ListT $ return $ Cons a mempty
  (<*>) = undefined
  {-
  ListT m <*> l = ListT $ m >>= \case
    Nil       -> pure Nil
    Cons f l' -> runListT (fmap f l <|> (l' <*> l))
  -}

instance Monad m => Monad (ListT (m :: * -> *)) where
  return  = pure -- yields control and delivers a result
  (>>=) :: ListT (m :: * -> *) (a :: *) -> (a -> ListT m (b :: *)) -> ListT m b
  s >>= f = TimWilliamsStreaming.concat $ fmap f s

-- lift monad 'm' into 'ListT'
-- :k MonadTrans :: ((* -> *) -> * -> *) -> Constraint
instance MonadTrans ListT where
  lift m  = ListT $ m >>= \a -> return (Cons a mempty)

-- lift IO monad 'm' into 'ListT'
-- :k MonadIO :: (* -> *) -> Constraint
instance MonadIO m => MonadIO (ListT (m :: * -> *)) where
  liftIO  = lift . liftIO

-- completely evaluate a stream computation
mapMS_ :: Monad m => (a -> m ()) -> ListT m a -> m ()
mapMS_ f (ListT mstep) = mstep >>= \case
  Cons a s -> f a >> mapMS_ f s
  Nil      -> return ()

-- incrementation on-demand computation
-- similar to Java's Iterable<>
type Stream' a = ListT IO a

example :: IO ()
example = mapMS_ print (return 1 <> return 2 <> return 3 :: Stream' Int)