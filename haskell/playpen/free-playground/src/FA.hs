{-# LANGUAGE FlexibleContexts #-}
-- experiment trying run applicative args in parallel
module FA where

import           Control.Applicative.Free (Ap, liftAp, runAp, runAp_)
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad.IO.Class
import           Data.List                ((\\))

{-# ANN module "HLint: ignore Eta reduce" #-}

data Compute a = Compute { cName    :: String
                         , cCompute :: Integer -> Integer -- TODO return should be `a`
                         }

mkCpt :: String -> (Integer -> Integer) -> Ap Compute a
mkCpt n f = liftAp $ Compute n f

type Three a b c = (a,b,c)
thr :: a -> b -> c -> Three a b c
thr a b c = (a,b,c)

cpt :: Ap Compute (Three Integer Integer Integer)
cpt = thr <$> mkCpt "factorial" factorial
          <*> mkCpt "fibonacci" fibonacci
          <*> mkCpt "primes"    (last . primesTo)

getF :: Ap Compute a -> [Integer -> Integer]
getF = runAp_ (\(Compute _ f) -> [f])
{-
runF :: MonadIO m => Integer -> Ap Compute a -> m [Chan Integer]
runF n = runAp_ (\(Compute _ f) -> do
                      chan <- liftIO newChan
                      liftIO (forkIO (writeChan chan (f n)))
                      pure [chan])
-}
factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]

fibonacci :: (Eq a, Num a, Num b) => a -> b
fibonacci n = case n of
    0 -> 0
    1 -> 1
    _ -> fibonacci (n-1) + fibonacci (n-2)

primesTo :: (Enum a, Eq a, Num a) => a -> [a]
primesTo m = sieve [2..m]
  where
    sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
    sieve [] = []

