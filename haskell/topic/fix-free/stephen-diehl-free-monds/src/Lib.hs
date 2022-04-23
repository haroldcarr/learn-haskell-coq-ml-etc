module Lib where

-- http://dev.stephendiehl.com/hask/#free-monads

import           Control.Monad.Free

{-
Pure ::           a  -> Free f a
Free :: f (Free f a) -> Free f a

liftF   :: (Functor f, MonadFree f m) =>      f a -> m a
retract ::                    Monad f => Free f a -> f a

Free monads : monads that
- instead of having a join operation that combines computations
- forms composite computations from application of a functor

join ::       Monad m => m (m a) -> m a
wrap :: MonadFree f m => f (m a) -> m a

example : Partiality monad : models computations which can diverge

create a free monad from the Maybe functor
used to fix the call-depth of function (e.g., Ackermann)
-}

type Partiality a = Free Maybe a

-- Non-termination.
never :: Partiality a
never = fix (Free . Just)
 where
  fix  :: (a -> a) -> a
  fix f = let x = f x in x

fromMaybe :: Maybe a -> Partiality a
fromMaybe (Just x) = Pure x
fromMaybe Nothing  = Free Nothing

runPartiality :: Int -> Partiality a -> Maybe a
runPartiality 0 _               = Nothing
runPartiality _ (Pure a)        = Just a
runPartiality _ (Free Nothing)  = Nothing
runPartiality n (Free (Just a)) = runPartiality (n - 1) a

ack :: Int -> Int -> Partiality Int
ack 0 n = Pure (n + 1)
ack m 0 = Free (Just $ ack (m-1) 1)
ack m n = Free (Just $ ack m (n-1)) >>= ack (m-1)

runack :: IO ()
runack  = do
  let diverge = never :: Partiality ()
  print $ runPartiality 1000 diverge
  print $ runPartiality 1000 (ack 3 4)
  print $ runPartiality 5500 (ack 3 4)
  print $ show               (ack 1 2)
  print $ runPartiality    3 (ack 1 2)
  print $ runPartiality    4 (ack 1 2)
