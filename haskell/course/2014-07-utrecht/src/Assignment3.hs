{-
Created       : by Ruud Koot.
Last Modified : 2014 Jul 09 (Wed) 01:55:21 by Harold Carr.
-}

module Assignment3 where

import           Control.Arrow   ((&&&))
import           Data.Function   (on)
import           Data.List       (group, sort, sortBy)
import           Data.Maybe      (fromJust, isJust)
import           Data.Set        (Set, empty, insert, size)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

-- | Containers

data Rose a = Rose a [Rose a]
    deriving (Eq, Show)

-- * Exercise 1

instance Functor Rose where
    fmap f (Rose a as) = Rose (f a) (map (fmap f) as)

e1 :: T.Test
e1 = T.TestList
    [
      U.teq "e100" (fmap (*2) (Rose (2::Int) [Rose 4 [Rose  6 []]]))
                              (Rose 4        [Rose 8 [Rose 12 []]])
    ]

class Monoid a where
    mempty ::           a
    (<>)   :: a -> a -> a

instance Monoid [a] where
    mempty = []
    (<>)   = (++)

newtype Sum     a = Sum     { unSum :: a } deriving (Eq, Show)
newtype Product a = Product { unProduct :: a } deriving (Eq, Show)

instance Num a => Monoid (Sum a) where
    mempty           = Sum 0
    Sum n1 <> Sum n2 = Sum (n1 + n2)

-- * Exercise 2

instance Num a => Monoid (Product a) where
    mempty                   = Product 1
    Product n1 <> Product n2 = Product (n1 * n2)

e2 :: T.Test
e2 = T.TestList
    [
      U.teq "e200" (Product 2 <> Product 4) (Product (8::Int))
    , U.teq "e201" (Product 2 <> mempty)    (Product (2::Int))
    ]

class Functor f => Foldable f where
    fold    :: Monoid m =>             f m -> m
    foldMap :: Monoid m => (a -> m) -> f a -> m
    -- * Exercise 4
    foldMap f fa = fold (fmap f fa)

instance Foldable [] where
    fold = foldr (<>) mempty

-- * Exercise 3

instance Foldable Rose where
    fold r = fold (squish r [])
               where
                 squish (Rose a as) xs = a : foldr squish xs as

e3_4 :: T.Test
e3_4 = T.TestList
    [
      U.teq "e300" (fold          (Rose (Sum 2) [Rose (Sum 4) [Rose  (Sum 6) []]]))   (Sum (12::Int))
    , U.teq "e400" (foldMap (Sum) (Rose      2  [Rose      4  [Rose       6  []]]))   (Sum (12::Int))
    ]

-- * Exercise 5

fsum, fproduct :: (Foldable f, Num a) => f a -> a
fsum     = undefined
fproduct = undefined

-- | Poker

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
    deriving (Bounded, Enum, Eq, Ord)

-- * Exercise 6

instance Show Rank where
    show = undefined

data Suit = S | H | D | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
    show (Card { rank = r, suit = s }) = undefined

type Deck = [Card]

-- * Exercise 8

fullDeck, piquetDeck :: Deck
fullDeck   = undefined
piquetDeck = undefined

newtype Hand = Hand { unHand :: [Card] } deriving (Eq, Show)

data HandCategory
    = HighCard      [Rank]
    | OnePair       Rank [Rank]
    | TwoPair       Rank Rank Rank
    | ThreeOfAKind  Rank Rank Rank
    | Straight      Rank
    | Flush         [Rank]
    | FullHouse     Rank Rank
    | FourOfAKind   Rank Rank
    | StraightFlush Rank
    deriving (Eq, Ord, Show)

-- * Exercise 9

sameSuits :: Hand -> Bool
sameSuits = undefined

-- * Exercise 10

isStraight :: [Rank] -> Maybe Rank
isStraight = undefined

-- * Exercise 11

ranks :: Hand -> [Rank]
ranks = undefined

-- * Exercise 12

order :: Hand -> [(Int, Rank)]
order = undefined

-- * Exercise 13

handCategory :: Hand -> HandCategory
handCategory = undefined

-- * Exercise 14

instance Ord Hand where
    compare = undefined

-- * Exercise 15

combs :: Int -> [a] -> [[a]]
combs = undefined

-- * Exercise 16

allHands :: Deck -> [Hand]
allHands = undefined

-- * Exercise 17

distinctHands :: Deck -> Set Hand
distinctHands = undefined

-- * Question 1

{- ANSWER -}

-- * Question 2

{- ANSWER -}

------------------------------------------------------------------------------

a3 :: IO T.Counts
a3 = do
    _ <- T.runTestTT e1
    _ <- T.runTestTT e2
    T.runTestTT e3_4

-- End of file.

