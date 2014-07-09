{-
Created       : by Ruud Koot.
Last Modified : 2014 Jul 09 (Wed) 08:57:21 by Harold Carr.
-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- HC : added this
{-# LANGUAGE MultiParamTypeClasses  #-}

module Assignment4 where

import           Control.Monad
import           Data.Foldable    (Foldable (..))
import           Data.Monoid      (Monoid (..), Sum (..), (<>))
import           Data.Ratio
import           System.Random    (Random (random, randomR), getStdRandom,
                                   mkStdGen, next, randomIO)

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
    ts <- liftM (filter (== H)) (replicateM 6 toss)
    d  <- roll
    return $ if (1 + (fromEnum d)) >= length ts then Win else Lose

------------------------------------------------------------------------------
-- * Simulation

-- Exercise 2

instance Random Coin where
--  random :: (RandomGen g, Random a) => g -> (a, g)
    random         g = randomR (H,T) g

--  randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
    randomR (_, _) g = let (i,g') = next g in (if even i then H else T, g')

instance Random Dice where
    random         g = let (i,g') = randomR (fromEnum (minBound :: Dice), fromEnum (maxBound :: Dice)) g
                       in (toEnum i, g')

    randomR (_,_) g = let (i,g') = next g in
        ( if      mod i 6 == 0 then D6
          else if mod i 5 == 0 then D5
          else if mod i 4 == 0 then D4
          else if mod i 3 == 0 then D3
          else if mod i 2 == 0 then D2
          else                      D1
        , g')

e :: T.Test
e = T.TestList
    [
--      U.teq "e00" (do (i,g) <- randomR (H,T) (mkStdGen 6)
--      U.teq "e00" (do (i,g) <- randomR (D1,D6) (mkStdGen 6)
    ]

-- Exercise 3

instance MonadGamble IO where
    -- toss :: IO Coin
    toss = randomIO
    -- roll :: IO Dice
    roll = randomIO

-- Exercise 4

simulate :: IO Outcome -> Integer -> IO Rational
simulate g n = do
    o <- replicateM (fromIntegral n) g
    let w = filter (== Win) o
    return $ toInteger (length w) % toInteger (length o)

------------------------------------------------------------------------------
-- * Decision trees

data DecisionTree a
    = Result a
    | Decision [DecisionTree a]
    deriving (Eq, Show)

-- Exercise 5

instance Monad DecisionTree where

    -- return :: a -> DecisionTree a
    return = undefined

    -- (>>=) :: DecisionTree a -> (a -> DecisionTree b) -> DecisionTree b
    (>>=) = undefined

-- Exercise 6

instance MonadGamble DecisionTree where
    toss = undefined
    roll = undefined

-- Exercise 7

probabilityOfWinning :: DecisionTree Outcome -> Rational
probabilityOfWinning = undefined

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
    T.runTestTT e

-- End of file.
