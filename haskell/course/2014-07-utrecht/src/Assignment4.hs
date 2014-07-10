{-
Created       : by Ruud Koot.
Last Modified : 2014 Jul 10 (Thu) 08:49:01 by Harold Carr.
-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- HC : added this
{-# LANGUAGE MultiParamTypeClasses  #-}

module Assignment4 where

import           Prelude

import           Control.Monad
import           Data.Foldable
import           Data.Monoid      (Monoid (..), Sum (..), (<>))
import           Data.Ratio
import           System.Random

import           System.IO.Unsafe (unsafePerformIO)
import qualified Test.HUnit       as T
import qualified Test.HUnit.Util  as U

-- | A Game of Chance

------------------------------------------------------------------------------
-- * The Gambling Monad

data Coin = H | T
    deriving (Bounded, Eq, Enum, Ord, Show)

data Dice = D1 | D2 | D3 | D4 | D5 | D6
    deriving (Bounded, Eq, Enum, Ord, Show)

data Outcome = Win | Lose
    deriving (Eq, Ord, Show)

-- In 7.8, Monad derives from Functor (but I am using 7.6, so do it explicitly)
class Monad m => MonadGamble m where
    toss :: m Coin
    roll :: m Dice

-- Exercise 1

-- win if num heads of 6 coin tosses <= single roll dice heads (in that order)
game :: MonadGamble m => m Outcome
game = do
    ts <- replicateM 6 toss
    d  <- roll
    return $ if (1 + (fromEnum d)) >= count (== H) ts then Win else Lose

count :: (a -> Bool) -> [a] -> Int
-- count p = foldr (\x acc -> if p x then acc + 1 else acc) 0
count p = length . filter p

------------------------------------------------------------------------------
-- * Simulation

-- Exercise 2

instance Random Coin where
--  random :: (RandomGen g, Random a) => g -> (a, g)
    random  = randomR (minBound, maxBound)
--  randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
    randomR = myRandomR

instance Random Dice where
    random  = randomR (minBound, maxBound)
    randomR = myRandomR

myRandomR :: (Enum a, RandomGen g) => (a, a) -> g -> (a, g)
myRandomR (l,h) g = let (i,g') = randomR (fromEnum l, fromEnum h) g
                    in (toEnum i, g')

-- Exercise 3

instance MonadGamble IO where
    -- toss :: IO Coin
    toss = randomIO -- this connects with above
    -- roll :: IO Dice
    roll = randomIO

-- Exercise 4

simulate :: IO Outcome -> Integer -> IO Rational
simulate g n = do
    o <- replicateM (fromIntegral n) g
    return $ toInteger (count (== Win) o) % toInteger (length o)

------------------------------------------------------------------------------
-- * Decision trees

data DecisionTree a
    = Result a
    | Decision [DecisionTree a]
    deriving (Eq, Show)

-- Exercise 5

instance Monad DecisionTree where
    -- return :: a -> DecisionTree a
    return            = Result
    -- (>>=) :: DecisionTree a -> (a -> DecisionTree b) -> DecisionTree b
    Result   a  >>= f = f a
    Decision as >>= f = Decision $ map (>>= f) as

-- Exercise 6

instance MonadGamble DecisionTree where
    toss = Decision [Result  H, Result  T]
    roll = Decision [Result D1, Result D2, Result D3, Result D4, Result D5, Result D6]

-- Exercise 7

flatten :: DecisionTree a -> [DecisionTree a]
flatten r@(Result _) = [r]
flatten (Decision xs) = Prelude.concatMap flatten xs

probabilityOfWinning :: DecisionTree Outcome -> Rational
probabilityOfWinning o =
    let fl = flatten o
    in toInteger (count (\(Result x) -> x == Win) fl) % toInteger (length fl)

-- game :: DecisionTree Outcome
-- probabilityOfWinning (game :: DecisionTree Outcome)

e7 :: T.Test
e7 = T.TestList
    [
      U.teq "e7" (probabilityOfWinning (game :: DecisionTree Outcome)) (85 % 128)
    ]

------------------------------------------------------------------------------
-- | Instrumented State Monad

-- Exercise 8

class Monad m => MonadState m s | m -> s where

    get :: m s
    get = undefined

    put :: s -> m ()
    put = undefined

    modify :: (s -> s) -> m s
    modify = undefined

-- * Instrumentation

data Counts = Counts {
    binds   :: Int,
    returns :: Int,
    gets    :: Int,
    puts    :: Int
} deriving (Eq, Show)

-- Exercise 9

instance Monoid Counts where
    mempty  = undefined
    mappend = undefined
    -- Note: mappend is the same as (<>) from the previous assignment. The
    --       Monoid class from the standard library requires you to call this
    --       mappend in the instance definition, but you can still use (<>)
    --       in the rest of your code.

oneBind, oneReturn, oneGet, onePut :: Counts
oneBind   = undefined
oneReturn = undefined
oneGet    = undefined
onePut    = undefined

newtype State' s a = State' { runState' :: (s, Counts) -> (a, s, Counts) }

-- Exercise 10

instance Monad (State' s) where

    -- return :: a -> State' s a
    return x = undefined

    -- (>>=) :: State' s a -> (a -> State' s b) -> State' s b
    st >>= k = undefined

instance MonadState (State' s) s where

    -- get :: State' s s
    get = undefined

    -- put :: s -> State' s ()
    put = undefined

-- * Tree Labeling

data Tree a = Branch (Tree a) a (Tree a) | Leaf
    deriving (Eq, Ord, Show)

-- Exercise 11

label :: MonadState m Int => Tree a -> m (Tree (Int, a))
label = undefined

-- Exercise 12

run :: State' s a -> s -> (a, Counts)
run = undefined

------------------------------------------------------------------------------

a4 :: IO T.Counts
a4 = do
    T.runTestTT e7

-- End of file.
