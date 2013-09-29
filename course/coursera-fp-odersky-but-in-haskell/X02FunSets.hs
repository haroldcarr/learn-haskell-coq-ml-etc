{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Sep 29 (Sun) 09:03:14 by carr.
-}

module X02FunSets where

import Data.List (intercalate)

type Set = Int -> Bool


contains :: Set -> Int -> Bool
contains s elem = s elem

singletonSet :: Int -> Set
singletonSet elem = \x -> x == elem

union :: Set -> Set -> Set
union s t = \x -> s x || t x

intersect :: Set -> Set -> Set
intersect s t = \x -> s x && t x

diff :: Set -> Set -> Set
diff s t = \x -> s x  && (not $ t x)

filter' :: Set -> (Int -> Bool) -> Set
filter' s p = \x -> s x && p x -- same as intersect

bound = 1000

forall :: Set -> (Int -> Bool) -> Bool
forall s p = iter (-bound)
  where iter a =
            if      a > bound        then True
            else if s a && not (p a) then False
            else                          iter (a+1)

exists :: Set -> (Int -> Bool) -> Bool
exists s p = not $ forall s $ \x -> not (p x)

map' :: Set -> (Int -> Int) -> Set
map' s f = iter (-bound) $ \x -> False
  where iter a m =
            if      a > bound then m
            else if s a       then iter (a+1) $ \x -> if x == f a then True else m x
            else                   iter (a+1) m

toString :: Set -> String
toString s =
    let xs = [(show x) | x <- [(-bound) .. bound], contains s x]
    in "{" ++ (intercalate "," xs) ++ "}"

{-
def printSet(s: Set) = println(toString(s))
-}

-- End of file.
