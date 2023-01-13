{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# LANGUAGE FlexibleContexts #-}

-- experiment trying run applicative args in parallel

module FA_args_in_parallel where

import           Control.Applicative.Free (Ap, liftAp, runAp_)
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                ((\\))

{-# ANN module "HLint: ignore Eta reduce" #-}

data Compute a = Compute { cName    :: String
                         , cCompute :: Integer -> Integer -- TODO return should be `a`
                         }

mkCpt :: String -> (Integer -> Integer) -> Ap Compute Integer
mkCpt n f = liftAp $ Compute n f

unMkCpts :: Ap Compute a -> [Integer -> Integer]
unMkCpts  = runAp_ (\(Compute _ f) -> [f])

runF :: (MonadIO m, Monoid (m [Chan Integer])) => Integer -> Ap Compute a -> m [Chan Integer]
runF n = runAp_ (\(Compute _ f) -> do
                      chan <- liftIO newChan
                      liftIO (forkIO (writeChan chan (f n)))
                      pure [chan])

------------------------------------------------------------------------------

type Three a b c = (a,b,c)
thr :: a -> b -> c -> Three a b c
thr a b c = (a,b,c)

cpt :: Ap Compute (Three Integer Integer Integer)
cpt = thr <$> mkCpt "factorial" factorial
          <*> mkCpt "fibonacci" fibonacci
          <*> mkCpt "primes"    (last . primesTo)

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

test :: IO ()
test  = do
  cs <- runF 5 cpt
  forM_ cs $ \c -> do
    i <- readChan c
    print i
