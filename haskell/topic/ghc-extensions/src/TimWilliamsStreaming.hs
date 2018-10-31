{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase    #-}

module TimWilliamsStreaming where

-- ListT done right

-- list elements 'a' interleaved with effect 'm'
newtype ListT m a = ListT { runListT :: m (Step m a) }
  deriving Functor

data Step m a
  = Cons a (ListT m a)
  | Nil
  deriving Functor

instance Monad m => Semigroup (ListT m a) where
  (<>) = undefined

instance Monad m => Monoid (ListT m a) where
  mempty               = ListT $ return Nil
  mappend (ListT m) s' = ListT $ m >>= \case
    Cons a s -> return $ Cons a (s `mappend` s')
    Nil      -> runListT s'

concat :: Monad m => ListT m (ListT m a) -> ListT m a
concat (ListT m) = ListT $ m >>= \case
  Cons s ss -> runListT $ s `mappend` TimWilliamsStreaming.concat ss
  Nil       -> return Nil

instance Monad m => Applicative (ListT m) where
  pure a  = ListT $ return $ Cons a mempty
  (<*>)   = undefined

instance Monad m => Monad (ListT m) where
  return  = pure
  (>>=) :: ListT m a -> (a -> ListT m b) -> ListT m b
  s >>= f = TimWilliamsStreaming.concat $ fmap f s

instance MonadTrans ListT where
  lift    = ListT $ m >>= \a -> return (Cons a mempty)

instance MonadIO m => MonadIO (ListT m) where
  liftIO  = lift (liftIO m)