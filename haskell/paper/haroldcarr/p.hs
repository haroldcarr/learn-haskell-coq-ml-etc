{-
Created       : 2013 Dec 15 (Sun) 21:08:32 by carr.
Last Modified : 2013 Dec 17 (Tue) 18:53:24 by carr.
-}

import Control.Concurrent
import Control.Monad (when)
import Data.Numbers.Primes (isPrime)

import Test.HUnit       as T
import Test.HUnit.Util  as U -- https://github.com/haroldcarr/test-hunit-util
import System.IO.Unsafe -- for unit tests

default (Integer)

{-
Example from
_The Art of Multiprocessor Programming, Revised Reprint_, Maurice Herlihy, Nir Shavit
Section 1.1
-}

------------------------------------------------------------------------------

listNumPrimesInRangesTo :: Integral a => a -> IO [Int]
listNumPrimesInRangesTo block = do
    numPrimesFoundInEachBlock <- newMVar []
    children                  <- newMVar []
    listNumPrimesInRangesTo' children 0 numPrimesFoundInEachBlock
    waitForChildren children
    takeMVar numPrimesFoundInEachBlock
 where -- the algorithm requires always forking 10 children
    listNumPrimesInRangesTo' c i np | i <= 9 = do forkChild c (numPrimesInRangeIO i block np)
                                                  listNumPrimesInRangesTo' c (i+1) np
                                    | otherwise = return ()


listNumPrimesInRangesToSequential :: Integral a => a -> [Int]
listNumPrimesInRangesToSequential block = lnps 0 []
  where
    lnps i acc | i <= 9    = lnps (i + 1) ((numPrimesInRange i block):acc)
               | otherwise = acc


numPrimesInRange :: Integral a => a -> a -> Int
numPrimesInRange i block  = numPrimesInRange' ((i * block) + 1) []
  where
    numPrimesInRange' j acc
        | j <= (i + 1) * block = numPrimesInRange' (j + 1) (if isPrime j then j:acc else acc)
        | otherwise            = length acc -- could calculate length at each step, but WANT to take the unnecessary time hit here

numPrimesInRangeIO :: Integral a => a -> a -> MVar [Int] -> IO ()
numPrimesInRangeIO i block primes = do
    let p = numPrimesInRange i block
    push p primes
--    tid <- myThreadId
--    forkIO (putStrLn ("id: " ++ (show tid) ++ " ; prime: " ++ (show p)))
--    putStrLn (show tid ++ " DONE")
--    return ()


{-
numPrimesInRange 0 (10^9)
numPrimesInRange 0 (10^7)
=> Segmentation fault: 11

10^5
=> 100000
(0 * (10^5)) + 1
1
(0 + 1) * (10^5)
100000

(1 * (10^5)) + 1
100001
(1 + 1) * (10^5)
200000

(5 * (10^5)) + 1
500001
(5 + 1) * (10^5)
600000

(9 * (10^5)) + 1
900001
(9 + 1) * (10^5)
1000000
-}

tr0 :: [T.Test]
tr0 = U.t "tr0"
      (numPrimesInRange 0 (10^5))
      9592

tr1 :: [T.Test]
tr1 = U.t "tr1"
      (numPrimesInRange 2 (10^5))
      8013

tr2 :: [T.Test]
tr2 = U.t "tr2"
      (numPrimesInRange 4 (10^5))
      7678

trExpectedResult :: [Int]
trExpectedResult = [7224,7323,7408,7445,7560,7678,7863,8013,8392,9592]

tr :: [T.Test]
tr = U.t "tr"
     (unsafePerformIO (listNumPrimesInRangesTo (10^5)))
     trExpectedResult

trs :: [T.Test]
trs = U.t "trs"
     (listNumPrimesInRangesToSequential (10^5))
     trExpectedResult

------------------------------------------------------------------------------

findPrimesTo :: (Num a, Ord a) => a -> Int -> IO (Int, Int)
findPrimesTo numChildren limit = do -- numChildren is 0-based
    intSupply      <- newMVar 2
    numPrimesFound <- newMVar 0
    children       <- newMVar []
    findPrimesTo' children 0 intSupply numPrimesFound
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
    when (i < limit) $ do
        when (isPrime i) $ do
            getAndInc primes
            return ()
        findPrime limit ints primes

-- http://answers.yahoo.com/question/index?qid=1006050901081
tfp0 :: [T.Test]
tfp0 = U.t "tfp0"
       (unsafePerformIO (findPrimesTo 0 (10^6)))
       (1000001, sum trExpectedResult)

tfp1 :: [T.Test]
tfp1 = U.t "tfp1"
       (unsafePerformIO (findPrimesTo 4 (10^6)))
       (1000005, sum trExpectedResult)

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
    T.runTestTT $ TestList $ tr0 ++ tr1 ++ tr2 ++ tr ++ trs ++
                             tfp0 ++ tfp1

main = runTests

-- End of file.
