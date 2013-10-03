{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Oct 02 (Wed) 22:11:22 by carr.

See my question and answers on Set representation:
    http://stackoverflow.com/questions/19086408/haskell-how-to-define-instance-show-set-for-type-set-int-bool

Also:
    http://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell

NOTE: LANGUAGE above only needed for Set' representation below.
-}

module X02FunSets where

import Data.List (intercalate)

newtype Set = Set (Int -> Bool)

contains :: Set -> Int -> Bool
contains (Set s) = s -- applies s to an int (not shown)

singletonSet :: Int -> Set
singletonSet elem = Set (== elem)

union :: Set -> Set -> Set
union (Set s) (Set t) = Set (\x -> s x || t x)

intersect :: Set -> Set -> Set
intersect (Set s) (Set t) = Set (\x -> s x && t x)

diff :: Set -> Set -> Set
diff (Set s) (Set t) = Set (\x -> s x  && not (t x))

filter' :: Set -> (Int -> Bool) -> Set
filter' (Set s) p = Set (\x -> s x && p x) -- same as intersect

bound = 1000

forall :: Set -> (Int -> Bool) -> Bool
forall (Set s) p = iter (-bound)
  where iter a
            | a > bound        = True
            | s a && not (p a) = False
            | otherwise        = iter (a + 1)

exists :: Set -> (Int -> Bool) -> Bool
exists s p = not $ forall s $ \x -> not (p x)

map' :: Set -> (Int -> Int) -> Set
map' (Set s) f = iter (-bound) $ const False -- same as: \x -> False
  where iter a m
            | a > bound = Set m
            | s a       = iter (a + 1) $ \x -> (x == f a) || m x
            | otherwise = iter (a + 1) m

instance Show Set where
    show (Set s) =
        let xs = [show x | x <- [(-bound) .. bound], s x]
        in "{" ++ intercalate "," xs ++ "}"

{-
def printSet(s: Set) = println(toString(s))
-}

-- ALTERNATE

type Set' = Int -> Bool

contains' :: Set' -> Int -> Bool
contains' s = s

intersect' :: Set' -> Set' -> Set'
intersect' s t x = s x && t x

toString :: Set' -> String
toString s =
    let xs = [show x | x <- [(-bound) .. bound], contains' s x]
    in "{" ++ intercalate "," xs ++ "}"

instance Show Set' where show = toString

{-
anyIntBoolFun1 = \x -> -10 < x
anyIntBoolFun2 = \x ->   x < 0
setIntBoolFun1 = Set anyIntBoolFun1
setIntBoolFun2 = Set anyIntBoolFun2

main = do
    putStrLn $ show $ intersect  setIntBoolFun1 setIntBoolFun2
    putStrLn $ show $ intersect' anyIntBoolFun1 anyIntBoolFun2
-}

-- *Main> main
-- {-9,-8,-7,-6,-5,-4,-3,-2,-1}
-- {-9,-8,-7,-6,-5,-4,-3,-2,-1}

-- End of file.
