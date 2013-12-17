{-
Created       : 2013 Dec 15 (Sun) 21:08:32 by carr.
Last Modified : 2013 Dec 16 (Mon) 18:31:38 by carr.
-}

import Control.Concurrent
import Control.Monad (when)
import Data.Numbers.Primes (isPrime)

import Test.HUnit       as T
import Test.HUnit.Util  as U -- https://github.com/haroldcarr/test-hunit-util
import System.IO.Unsafe -- for unit tests

default (Integer)

------------------------------------------------------------------------------
listNumPrimesInRanges :: Integral a => a -> a -> IO [Int]
listNumPrimesInRanges j block = do
    numPrimesFoundInEachBlock <- newMVar []
    children                  <- newMVar []
    listNumPrimesInRanges' children 0 numPrimesFoundInEachBlock
    waitForChildren children
    takeMVar numPrimesFoundInEachBlock
 where
    listNumPrimesInRanges' c i np | i <= j = do forkChild c (push (numPrimesInRange i block) np)
                                                listNumPrimesInRanges' c (i+1) np
                                  | otherwise = return ()


numPrimesInRange :: Integral a => a -> a -> Int
numPrimesInRange i block = numPrimesInRange' ((i * block) + 1) []
  where
    numPrimesInRange' j acc
        | j <= (i + 1) * block = numPrimesInRange' (j + 1) (if isPrime j then j:acc else acc)
        | otherwise            = length acc -- could calculate length at each step, but WANT to take the unnecessary time hit here


{-
numPrimesInRange 0 (10^9)
numPrimesInRange 0 (10^7)
=> Segmentation fault: 11
-}

tr0 :: [T.Test]
tr0 = U.t "tr0"
      (numPrimesInRange 0 (10^5))
      9592

tr1 :: [T.Test]
tr1 = U.t "tr1"
      (numPrimesInRange 1 (10^5))
      8392

tr2 :: [T.Test]
tr2 = U.t "tr2"
      (numPrimesInRange 2 (10^5))
      8013

tr3 :: [T.Test]
tr3 = U.t "tr3"
      (numPrimesInRange 3 (10^5))
      7863

tr4 :: [T.Test]
tr4 = U.t "tr4"
      (numPrimesInRange 4 (10^5))
      7678

tr5 :: [T.Test]
tr5 = U.t "tr5"
      (numPrimesInRange 5 (10^5))
      7560

trExpectedResult :: [Int]
trExpectedResult = [7560,7678,7863,8013,8392,9592]
tr :: [T.Test]
tr = U.t "tr"
     (unsafePerformIO (listNumPrimesInRanges 5 (10^5)))
     trExpectedResult

------------------------------------------------------------------------------

findPrimesTo :: (Num a, Ord a) => a -> Int -> IO (Int, Int)
findPrimesTo numChildren limit = do
    intSupply      <- newMVar 2
    numPrimesFound <- newMVar 0
    children       <- newMVar []
    findPrimesTo' children 1 intSupply numPrimesFound
    waitForChildren children
    ri <- takeMVar intSupply
    rp <- takeMVar numPrimesFound
    return (ri, rp)
 where
    findPrimesTo' c i ints primes  | i <= numChildren = do forkChild c (findPrime limit ints primes)
                                                           findPrimesTo' c (i+1) ints primes
                                   | otherwise = return ()

findPrime :: Int -> MVar Int -> MVar Int -> IO ()
findPrime limit ints primes = do
    i <- getAndInc ints
    when (i < limit) $
        if isPrime i
        then do getAndInc primes
                findPrime limit ints primes
        else findPrime limit ints primes

-- http://answers.yahoo.com/question/index?qid=1006050901081
expectedTenToTheSixthResult :: Int
expectedTenToTheSixthResult = 78498

tfp0 :: [T.Test]
tfp0 = U.t "tfp0"
       (unsafePerformIO (findPrimesTo 1 (10^6)))
       (1000001, expectedTenToTheSixthResult)

tfp1 :: [T.Test]
tfp1 = U.t "tfp1"
      (unsafePerformIO (findPrimesTo 6 (10^6)))
      (1000006, expectedTenToTheSixthResult)

------------------------------------------------------------------------------

push :: Int -> MVar [Int] -> IO ()
push x numPrimesFoundInEachBlock = do
    v <- takeMVar numPrimesFoundInEachBlock
    putMVar numPrimesFoundInEachBlock (x:v)

getAndInc :: MVar Int -> IO Int
getAndInc count = do { v <- takeMVar count; putMVar count (v+1); return v }

-- next two from http://www.haskell.org/ghc/docs/7.6.2/html/libraries/base/Control-Concurrent.html
waitForChildren :: MVar [MVar a] -> IO ()
waitForChildren children = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren children

forkChild :: MVar [MVar ()] -> IO a -> IO ThreadId
forkChild children io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkFinally io (\_ -> putMVar mvar ())

------------------------------------------------------------------------------

runTests :: IO Counts
runTests =
    T.runTestTT $ TestList $ tr0 ++ tr1 ++ tr2 ++ tr3 ++ tr4 ++ tr5 ++ tr ++
                             tfp0 ++ tfp1

-- End of file.
