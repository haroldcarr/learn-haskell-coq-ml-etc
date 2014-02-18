{-# LANGUAGE BangPatterns #-}

module C05 where

import Control.DeepSeq

-- p. 111

data TimeMachine = TM { manufacturer :: String, year :: Integer } deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom m y = TM m y : timeMachinesFrom m (y+1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

-- zip [1 .. ] "abcd"

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

infinite2020Machines :: [TimeMachine]
infinite2020Machines = TM "Timely Inc." 2020 : infinite2020Machines

infinite2020Machines' :: [TimeMachine]
infinite2020Machines' = repeat $ TM "Timely Inc." 2020

specialOffer :: [TimeMachine]
specialOffer = cycle [TM m 2005, TM m 1994, TM m 908]
               where m = "Timely Inc."

fibonacci2 :: [Integer]
fibonacci2 = map fst $ iterate (\(n,n1) -> (n1,n+n1)) (0,1)
-- fibonacci2 !! 20

-- exercise 5-1 - p. 115

-- http://www.amazon.com/Lambda-Calculus-Types-Perspectives-Logic/dp/0521766141
-- based on Miranda program by D. Turner
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:x) = p : sieve [n | n <- x , (n `mod` p) > 0]
    sieve []    = error "HC: should not happen"

-- TODO: this hangs after printing last prime in range : doesn't print final list bracket
primesUpTo :: Integer -> [Integer]
primesUpTo n = [p | p <- primes, p < n]

-- p. 120

-- foldl  (+) 0 [1 .. 1000000000] -- Segmentation fault: 11
-- foldl' (+) 0 [1 .. 1000000000] -- 500000000500000000

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
    where sumForce' [] z = z
          sumForce' (y:ys) z = let s = z + y in s `seq` sumForce' ys s

sumForce2 :: [Integer] -> Integer
sumForce2 xs = sumForce' xs 0
    where sumForce' [] z = z
          sumForce' (y:ys) z = sumForce' ys $! (z+y)

-- sumForce  [1 .. 1000000000]
-- sumForce2 [1 .. 1000000000]

-- bang patterns - p. 121

sumYears :: [TimeMachine] -> Integer
sumYears xs = sumYears' xs 0
    where sumYears' [] z = z
          sumYears' (TM _ !y :ys) z = let !s = z + y in sumYears' ys s

foo :: a
foo = undefined

-- strict field - p. 128

data ListL a = ListL !Integer [a]

-- unpacked - p. 128
-- unpack data in place rather than reference to data

data ClientU = GovOrgU     {-# UNPACK #-} !Int String
             | CompanyU    {-# UNPACK #-} !Int String PersonU String
             | IndividualU {-# UNPACK #-} !Int PersonU
             deriving Show

data PersonU = PersonU String String deriving Show

-- lazy/strict containers - p. 129

{-
e.g.,
Data.Map.Lazy   : only key evaluated when inserting
Data.Map.Strict : both key and value evaluated when inserting
-}

-- Control.DeepSeq (deepseq ($!!)) -- similar to seq and ($!) but goes all the way

-- to make a data types support deep evaluation with deepseq, make them instances NFData type class
-- force all the fields and return ()

instance NFData ClientU where
    rnf (GovOrgU i n)                  = i `deepseq` n `deepseq` ()
    rnf (CompanyU i n (PersonU f l) r) = i `deepseq` n `deepseq` f `deepseq` l `deepseq` r `deepseq` ()
    rnf (IndividualU i (PersonU f l))  = i `deepseq` f `deepseq` l `deepseq` ()

-- End of file.
