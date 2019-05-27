module X_2019_05_21_NonEmpty_Exercise where

import           Prelude            as P
import qualified Data.List.NonEmpty as NE

{-# ANN module ("HLint: ignore Eta reduce"::String) #-}

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

a' :: [[a]] -> [a]
a'  []  = []
a' [[]] = []
a' ([]:_ :_) = []
a' ((x:_):_) = [x]

--------------------------------------------------

-- I think you called this one 'toNonEmpty' - but your version has an incorrect type.
b :: [[a]] -> NE.NonEmpty a
b xs = case concat xs of
  []  -> undefined        -- *****
  xs' -> NE.fromList xs'

b' :: [[a]] -> Maybe (NE.NonEmpty a)
b' xs = case concat xs of
  []  -> Nothing
  xs' -> Just (NE.fromList xs')

b'' :: [[a]] -> Maybe (NE.NonEmpty a)
b'' xs = NE.nonEmpty (concat xs)

--------------------------------------------------

c :: [NE.NonEmpty a] -> [a]
c xs = concatMap NE.toList xs

c' :: [NE.NonEmpty a] -> [a]
c'   []  = []
c' (x:_) = NE.toList x

--------------------------------------------------

d :: [NE.NonEmpty a] -> NE.NonEmpty a
d xs = case c xs of
  []  -> undefined        -- *****
  xs' -> NE.fromList xs'

d' :: [NE.NonEmpty a] -> Maybe (NE.NonEmpty a)
d' xs = case c xs of
  []  -> Nothing
  xs' -> Just (NE.fromList xs')

d'' :: [NE.NonEmpty a] -> Maybe (NE.NonEmpty a)
d'' xs = NE.nonEmpty (c xs)

-- eta reduction
d''' :: [NE.NonEmpty a] -> Maybe (NE.NonEmpty a)
d'''  = NE.nonEmpty . c

--------------------------------------------------

e :: NE.NonEmpty [a] -> [a]
e xs = concat (NE.toList xs)

--------------------------------------------------

f :: NE.NonEmpty [a] -> NE.NonEmpty a
f xs = case concat (NE.toList xs) of
  []  -> undefined        -- *****
  xs' -> NE.fromList xs'

f' :: NE.NonEmpty [a] -> Maybe (NE.NonEmpty a)
f' xs = case concat (NE.toList xs) of
  []  -> Nothing
  xs' -> Just (NE.fromList xs')

--------------------------------------------------

g :: NE.NonEmpty (NE.NonEmpty a) -> [a]
g xs = concatMap NE.toList (NE.toList xs)

--------------------------------------------------

h :: NE.NonEmpty (NE.NonEmpty a) -> NE.NonEmpty a
h xs = NE.fromList (g xs)

{-
a :: [           [            a]] -> [a]
b :: [           [            a]] -> NE.NonEmpty a
c :: [           NE.NonEmpty  a ] -> [a]
d :: [           NE.NonEmpty  a ] -> NE.NonEmpty a
e :: NE.NonEmpty [            a]  -> [a]
f :: NE.NonEmpty [            a]  -> NE.NonEmpty a
g :: NE.NonEmpty (NE.NonEmpty a)  -> [a]
h :: NE.NonEmpty (NE.NonEmpty a)  -> NE.NonEmpty a
-}
