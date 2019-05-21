module X_2019_05_21_NonEmpty_Exercise where

import Prelude            as P
import Data.List.NonEmpty as NE

-- Below, whereever it uses 'undefined' means
-- there is no 'a' to create a 'NonEmpty a' with - so the function is not TOTAL.

-- I create a total version by changing the output type to a 'Maybe'.

-- My version is "lossless" - meaning that if 'a's exist, the conversion do not lose any of them.
-- But that is extra.  For this exercise, it is fair to create functions that only return
-- one (or more) 'a' and lose others.

-- The main point is to know when the is NOT an 'a' available.

--------------------------------------------------

a :: [[a]] -> [a]
a xs = concat xs

--------------------------------------------------

-- I think you called this one 'toNonEmpty' - but your version has an incorrect type.
b :: [[a]] -> NonEmpty a
b xs = case concat xs of
  []  -> undefined
  xs' -> NE.fromList xs'

b' :: [[a]] -> Maybe (NonEmpty a)
b' xs = case concat xs of
  []  -> Nothing
  xs' -> Just (NE.fromList xs')

--------------------------------------------------

c :: [NonEmpty a] -> [a]
c xs = concatMap NE.toList xs

--------------------------------------------------

d :: [NonEmpty a] -> NonEmpty a
d xs = case c xs of
  []  -> undefined
  xs' -> NE.fromList xs'

d' :: [NonEmpty a] -> Maybe (NonEmpty a)
d' xs = case c xs of
  []  -> Nothing
  xs' -> Just (NE.fromList xs')

--------------------------------------------------

e :: NonEmpty [a] -> [a]
e xs = concat (NE.toList xs)

--------------------------------------------------

f :: NonEmpty [a] -> NonEmpty a
f xs = case concat (NE.toList xs) of
  []  -> undefined
  xs' -> NE.fromList xs'

f' :: NonEmpty [a] -> Maybe (NonEmpty a)
f' xs = case concat (NE.toList xs) of
  []  -> Nothing
  xs' -> Just (NE.fromList xs')

--------------------------------------------------

g :: NonEmpty (NonEmpty a) -> [a]
g xs = concatMap NE.toList (NE.toList xs)

--------------------------------------------------

h :: NonEmpty (NonEmpty a) -> NonEmpty a
h xs = NE.fromList (g xs)
