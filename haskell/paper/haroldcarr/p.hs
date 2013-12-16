{-
Created       : 2013 Dec 15 (Sun) 21:08:32 by carr.
Last Modified : 2013 Dec 16 (Mon) 10:25:18 by carr.
-}

import Control.Concurrent
import Control.Monad (when)
import Data.Numbers.Primes (isPrime)
import System.IO.Unsafe (unsafePerformIO)

listPrimesInRange :: Integral a => a -> a -> Int
listPrimesInRange i block = listPrimesInRange' ((i * block) + 1) []
  where
    listPrimesInRange' j acc
        | j <= (i + 1) * block = listPrimesInRange' (j + 1) (if isPrime j then j:acc else acc)
        | otherwise            = length acc -- could calculate length at each step, but WANT to take the unnecessary time hit here


lpir :: Integral a => a -> a -> IO [Int]
lpir j block = do
    counts <- newEmptyMVar
    putMVar counts []
    _ <- lpir' 0 counts
    takeMVar counts
 where
    lpir' i counts | i <= j = do _ <- forkIO (push (listPrimesInRange i block) counts)
                                 lpir' (i+1) counts
                   | otherwise = return ()


push :: Int -> MVar [Int] -> IO ()
push x counts = do
    v <- takeMVar counts
    let v' = x:v
    putMVar counts v'


{-
listPrimesInRange 0 (10^9)
listPrimesInRange 0 (10^7)
=> Segmentation fault: 11

listPrimesInRange 0 (10^5)
listPrimesInRange 1 (10^5)
listPrimesInRange 2 (10^5)
listPrimesInRange 3 (10^5)
listPrimesInRange 4 (10^5)
listPrimesInRange 5 (10^5)

lpir 5 (10^5)
-}

inc :: MVar Int -> IO Int
inc count = do { v <- takeMVar count; putMVar count (v+1); return v }

findPrime :: Int -> MVar Int -> MVar Int -> IO ()
findPrime limit ints primes = do
    i <- inc ints
    when (i < limit) $
        if isPrime i
        then do _ <- inc primes
                findPrime limit ints primes
        else findPrime limit ints primes

fop :: IO (Int, Int)
fop = do
    i <- newEmptyMVar
    putMVar i 2
    p <- newEmptyMVar
    putMVar p 0
    findPrime 10000 i p
    ri <- takeMVar i
    rp <- takeMVar p
    return (ri, rp)

intSupply :: IO (MVar a)
intSupply = newEmptyMVar

numPrimesFound :: IO (MVar a)
numPrimesFound = newEmptyMVar

fp :: (Num a, Ord a) => a -> Int -> IO (Int, Int)
fp j limit = do
    i <- intSupply
    putMVar i 2
    p <- numPrimesFound
    putMVar p 0
    _ <- fp' 0 i p
    waitForChildren
    ri <- takeMVar i
    rp <- takeMVar p
    return (ri, rp)
 where
    fp' i ints primes  | i <= j = do _ <- forkChild (findPrime limit ints primes)
                                     fp' (i+1) ints primes
                       | otherwise = return ()


-- from http://www.haskell.org/ghc/docs/7.6.2/html/libraries/base/Control-Concurrent.html
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkFinally io (\_ -> putMVar mvar ())

-- End of file.
