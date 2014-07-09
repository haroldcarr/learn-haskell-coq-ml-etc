{-
Created       : by Ruud Koot.
Last Modified : 2014 Jul 09 (Wed) 06:28:27 by Harold Carr.
-}

module Assignment3 where

import           Control.Arrow   ((&&&))
import           Control.Monad   (guard)
import           Data.Function   (on)
import           Data.List       (group, sort, sortBy)
import           Data.Map        as M (alter, elems, empty, toList)
import           Data.Maybe      (fromJust, isJust)
import           Data.Set        (Set, empty, insert, size)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------

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
-- Note : you could factor this by defining an "unbox" type class to generalize Sum and Product then use here.
fsum, fproduct :: (Foldable f, Num a) => f a -> a
fsum fa     = let (Sum     result) = foldMap Sum     fa in result
fproduct fa = let (Product result) = foldMap Product fa in result

e5 :: T.Test
e5 = T.TestList
    [
      U.teq "e500" (fsum     (Rose 2 [Rose 4 [Rose 6 []]]))   (12::Int)
    , U.teq "e501" (fproduct (Rose 2 [Rose 4 [Rose 6 []]]))   (48::Int)
    ]

------------------------------------------------------------------------------

-- | Poker

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
    deriving (Bounded, Enum, Eq, Ord)

-- * Exercise 6

instance Show Rank where
    show R2  =  "2"
    show R3  =  "3"
    show R4  =  "4"
    show R5  =  "5"
    show R6  =  "6"
    show R7  =  "7"
    show R8  =  "8"
    show R9  =  "9"
    show R10 = "10"
    show J   =  "J"
    show Q   =  "Q"
    show K   =  "K"
    show A   =  "A"

data Suit = S | H | D | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
    show (Card { rank = r, suit = s }) = show r ++ show s

type Deck = [Card]

-- * Exercise 8

fullDeck, piquetDeck :: Deck
fullDeck   = mkDeck R2
piquetDeck = mkDeck R7

mkDeck :: Rank -> Deck
mkDeck startRank = [ Card r s | r <- [ startRank .. A ], s <- [ S .. C ] ]

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
sameSuits h =
    let ((Card _ s):cs) = unHand h
    in all (\(Card _ s') -> s' == s) cs

-- * Exercise 10

isStraight :: [Rank] -> Maybe Rank
isStraight rs =
    let sr = sort rs
    in if all (\(x,y) -> succ x == y) $ zip sr (tail sr)
           then Just (last sr)
           else Nothing

isStraight' :: [Rank] -> Maybe Rank
isStraight' rs = do
    let sr = sort rs
    guard $ all (\(x,y) -> succ x == y) $ zip sr (tail sr)
    return $ last sr

-- * Exercise 11

ranks :: Hand -> [Rank]
ranks = reverse . sort . map (\(Card r _) -> r) . unHand

-- * Exercise 12

order :: Hand -> [(Int, Rank)]
order h = M.elems $ foldr go M.empty $ unHand h
  where
    go (Card r _) acc = M.alter (\x -> case x of
                                            Nothing    -> Just (  1, r)
                                            Just (n,_) -> Just (n+1, r))
                        r
                        acc

-- * Exercise 13

handCategory :: Hand -> HandCategory
handCategory h =
    let rh = ranks h
    in case isStraight rh of
        Just hc -> if sameSuits h then StraightFlush hc else Straight hc
        Nothing ->
            case order h of
                [_     ,_      ,_      ,_      ,_ ] -> if sameSuits h then Flush rh else HighCard    rh
                [(4,r4),(1,r1) ]                    -> FourOfAKind  r4 r1
                [(3,r3),(2,r2) ]                    -> FullHouse    r3 r2
                [(3,r3),(1,r1) ,(1,r1')]            -> ThreeOfAKind r3 r1  r1'
                [(2,r2),(2,r2'),(1,r1) ]            -> TwoPair      r2 r2' r1
                [(2,r2),(1,r1) ,(1,r1'),(1,rr)]     -> OnePair      r2 [r1,r1',rr]

-- * Exercise 14

instance Ord Hand where
    compare h1 h2 = compare (handCategory h1) (handCategory h2)

------------------------------------------------------------------------------

-- * Exercise 15

combs :: Int -> [a] -> [[a]]
combs = undefined

-- * Exercise 16

allHands :: Deck -> [Hand]
allHands = undefined

-- * Exercise 17

distinctHands :: Deck -> Set Hand
distinctHands = undefined

ep :: T.Test
ep = T.TestList
    [
      U.teq "ep00" (sameSuits (Hand [Card R2 D, Card R3 D, Card R4 D, Card R5 D, Card R6 D])) True
    , U.teq "ep01" (sameSuits (Hand [Card R2 D, Card R3 D, Card R4 D, Card R5 D, Card R6 C])) False

    , U.teq "ep02" (isStraight  [R2, R3, R4, R5, R6])   (Just R6)
    , U.teq "ep03" (isStraight  [R2, R3, R4, R5, R7])   Nothing
    , U.teq "ep04" (isStraight' [R2, R3, R4, R5, R6])   (Just R6)
    , U.teq "ep05" (isStraight' [R2, R3, R4, R5, R7])   Nothing

    , U.teq "ep06" (ranks (Hand [Card R2 D, Card R3 D, Card R4 D, Card R5 D, Card R6 C]))         [R6, R5, R4, R3, R2]

    , U.teq "ep07" (order (Hand [Card R7 H, Card R7 D, Card Q S,  Card R7 S, Card Q H]))          [(3, R7),(2, Q)]

    , U.teq "ep08" (handCategory (Hand [Card R7 H, Card R7 D, Card Q S,  Card R7 S, Card Q H]))   (FullHouse R7 Q)

    , U.teq "ep09" (compare (Hand [Card R7 H, Card R7 D, Card Q S,  Card R7 S, Card Q  H])
                            (Hand [Card R2 D, Card R3 D, Card R4 D, Card R5 D, Card R6 C]))       GT
    ]

-- * Question 1

{- ANSWER -}

-- * Question 2

{- ANSWER -}

------------------------------------------------------------------------------

a3 :: IO T.Counts
a3 = do
    _ <- T.runTestTT e1
    _ <- T.runTestTT e2
    _ <- T.runTestTT e3_4
    _ <- T.runTestTT e5
    T.runTestTT ep

-- End of file.

