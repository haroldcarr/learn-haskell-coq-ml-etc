{-
Created       : 2014 Feb                   by Harold Carr.
Last Modified : 2014 Jul 06 (Sun) 00:45:08 by Harold Carr.
-}

{-# LANGUAGE BangPatterns #-}

module C05 where

import           Control.DeepSeq
import           Data.List       (find)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- p. 111

data TimeMachine = TM { manufacturer :: String, year :: Integer } deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom m y = TM m y : timeMachinesFrom m (y+1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

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

lazy :: T.Test
lazy = T.TestList
    [
      U.teq "lazy01" (take 3 timelyIncMachines)
                     [ TM {manufacturer = "Timely Inc.", year = 100}
                     , TM {manufacturer = "Timely Inc.", year = 101}
                     , TM {manufacturer = "Timely Inc.", year = 102}
                     ]
    , U.teq "lazy02" (find (\(TM { year = y}) -> y > 2018)  timelyIncMachines)
                     (Just (TM {manufacturer = "Timely Inc.", year = 2019}))

    , U.teq "lazy03" (zip [(1::Int) .. ] "abcd")
                     [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
    , U.teq "lazy04" (fibonacci  !! 20)
                     6765
    , U.teq "lazy05" (fibonacci2 !! 20)
                     6765
    ]

------------------------------------------------------------------------------
-- Exercise 5-1 - p. 115

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

e51 :: T.Test
e51 = T.TestList
    [
      U.teq "e510" (take 20 primes) [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
    ]

------------------------------------------------------------------------------
-- Exercise 5-2 - p. 118

e52 :: [T.Test]
e52 = U.tt "e52"
     [ fibonacci !! 3
     , (      0 : 1 : zipWith (+) fibonacci (tail fibonacci)) !! 3
     , (head $ tail $ zipWith (+) fibonacci (tail fibonacci))
     , (head $ tail $ zipWith (+) [0,1,1,2,3] (tail [0,1,1,2,3])) -- shorthand for lots more here
     , (head $ tail $ zipWith (+) [0,1,1,2,3]         [1,1,2,3])
     , (head $ tail $             [0+1,1+1,1+2,2+3])
     , (head $                        [1+1,1+2,2+3])
     ,                                 1+1
     ]
     2

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- bang patterns - p. 121

-- !y : make sure following addition is NOT given a thunk.
-- !s : make addition happen at each step (instead of building a thunk)
sumYears :: [TimeMachine] -> Integer
sumYears xs = sumYears' xs 0
    where sumYears' [] z = z
          sumYears' (TM _ !y :ys) z = let !s = z + y in sumYears' ys s

------------------------------------------------------------------------------
-- irrefutable patten - p. 121
-- matching on it never fails - so avoid deconstruction in pattern match

junk :: Maybe a
junk = case longOperation of
           ~(Just x) -> Just x
  where
    longOperation = undefined

foo :: a
foo = undefined

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------

c05 :: IO T.Counts
c05 = do
    _ <- T.runTestTT lazy
    _ <- T.runTestTT e51
    T.runTestTT $ T.TestList $ e52

-- End of file.
