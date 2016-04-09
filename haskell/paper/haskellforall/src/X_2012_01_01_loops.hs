{-
Created       : 2014 Oct 07 (Tue) 08:53:35 by Harold Carr.
Last Modified : 2014 Oct 19 (Sun) 21:01:05 by Harold Carr.

2012-01-01
http://www.haskellforall.com/2012/01/haskell-for-c-programmers-for-loops.html
FOR LOOPS
-}

module X_2012_01_01_loops where

import           Control.Applicative
import           Control.Monad.State.Lazy
import           Data.Lens.Lazy

-- FOLDS

{-
C:
double sum(double *xs, size_t num_xs) {
    size_t i;
    double result = 0.0;
    for (i = 0; i < num_xs; i++) {
        result += xs[i];
    }
    return result;
}
-}

-- C-like loops in Haskell:

while :: (Monad m) => m Bool -> m a -> m ()
while cond action = do
    c <- cond
    when c $ do
        action
        while cond action

for :: (Monad m) => m a -> m Bool -> m b -> m c -> m ()
for init0 cond post action = do
    init0
    while cond $ do
        action
        post

data Status = Status { _i :: Int, _result :: Double }

class Default a where def :: a

instance Default Int    where def = 0
instance Default Double where def = 0.0
instance Default Status where def = Status def def

i      :: Lens Status Int
i       = lens _i      (\x s -> s { _i      = x })
result :: Lens Status Double
result  = lens _result (\x s -> s { _result = x })

sum1     :: [Double] -> Int -> Double
sum1 xs n = flip evalState def $ do
    result ~= 0
    for (i ~= 0) (liftM (< n) (access i)) (i += 1) $ do
        i' <- access i
        result += (xs !! i')
    access result

sum2   :: [Double] -> Double
sum2 xs = flip evalState def $ do
    -- i.e. foreach (double x in xs) { ... }
    forM xs $ \x ->
        modify (\a -> a + x)
    get

sum3   :: [Double] -> Double
sum3 xs = flip evalState (def :: Double) $
    foldM (\a x -> return (a + x)) 0.0 xs

sum4   :: [Double] -> Double
sum4 xs = foldl (\x a -> x + a) 0.0 xs

sum5 :: [Double] -> Double
sum5  = foldl (+) 0.0

-- LOOPS

{-
C:
void printer(double *xs, size_t num_xs)
{
    for (i = 0; i < num_xs; i++)
    { printf("%f\n", xs[i]); }
}
-}

{- TODO
-- printer1 :: (Show a) => [a] -> Int -> IO ()
printer1 xs n = flip execStateT 0 $
    for (put 0) (liftM (< n) get) (modify (+ 1)) $ do
        i' <- get
        print $ xs !! i'
-}

printer2 :: (Show a) => [a] -> IO ()
printer2 xs = forM_ xs $ \x -> print x

printer3 :: (Show a) => [a] -> IO ()
printer3 xs = forM_ xs print

printer4 :: (Show a) => [a] -> IO ()
printer4 = mapM_ print


nested1 :: IO ()
nested1 =
    forM_ [1..10::Int] $ \i' ->
        forM_ [1..10::Int] $ \j ->
            print (i', j)

nested2 :: IO ()
nested2 =
    mapM_ print $ do
        i' <- [1..10::Int]
        j  <- [1..10::Int]
        return (i', j)

nested3 :: IO ()
nested3 =
    mapM_ print [(i', j) | i' <- [1..10::Int], j <- [1..10::Int]]

nested4 :: IO ()
nested4 =
    mapM_ print $ (,) <$> [1..10::Int] <*> [1..10::Int]

-- Modularity

-- separate data generation from data consumption

generator :: [(Integer, Integer)]
generator  = (,) <$> [1..10] <*> [1..10]

consumer :: (Show a) => [a] -> IO ()
consumer  = mapM_ print

-- End of file.
