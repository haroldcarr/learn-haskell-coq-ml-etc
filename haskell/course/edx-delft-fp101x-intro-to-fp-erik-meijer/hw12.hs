{-
Created       : 2014 Dec 27 (Sat) 14:14:39 by Harold Carr.
Last Modified : 2014 Dec 30 (Tue) 12:31:03 by Harold Carr.
-}

{-# LANGUAGE NPlusKPatterns #-}

import           Prelude         hiding (all, length, map, repeat, replicate,
                                  take, (++), (.))

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 12

------------------------------------------------------------------------------
-- EXERCISE 0

l0 :: [a] -> a
l0 [x] = x
l0 (_:xs) = last xs

fr0 :: (a -> b -> b) -> b -> [a] -> b
fr0 _ v [] = v
fr0 f v (x : xs) = f x (fr0 f v xs)

i0 :: [a] -> [a]
i0 [_] = []
i0 (x:xs) = x : i0 xs

d0 :: Int -> [a] -> [a]
d0 0 xs = xs
d0 n [] = []
d0 n (_ : xs) = d0 (n-1) xs

a0 :: [a] -> [a] -> [a]
a0 [] ys = ys
a0 (x : xs) ys = x : (a0 xs ys)

fl0 :: (a -> b -> a) -> a -> [b] -> a
fl0 _ v [] = v
fl0 f v (x : xs) = fl0 f (f v x) xs

e0 :: [Test]
e0 = U.t "e0"
     (l0 [1::Int])
     1

------------------------------------------------------------------------------
-- EXERCISE 1

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

add :: Nat -> Nat -> Nat
add  Zero    m =             m
add (Succ n) m = Succ (add n m)

{-
Using induction on n, show:

add n (Succ m) = Succ (add n m)

A: NO
Base case: OK
                                           add Zero     (Succ m)
=   { applying add }                                     Succ m
=   { unapplying add }               Succ (add Zero           m)

Inductive case: NO
                                           add (Succ n) (Succ m)
=   { applying add }                 Succ (add (Succ m)       n) -- NO
=   { applying add }           Succ (Succ (add       m        n))
=   { applying commutativity } Succ (Succ (add       n        m))
=   { unapplying add }               Succ (add (Succ n)       m)

B: OK
Base case: OK
                                           add Zero     (Succ m)
=   { applying add }                                     Succ m
=   { unapplying add }               Succ (add Zero           m)

Inductive case: OK
                                           add (Succ n) (Succ m)
=   { applying add }                 Succ (add       n  (Succ m)) -- OK
=   { induction hypothesis }   Succ (Succ (add       n        m)) -- OK?
=   { unapplying add }               Succ (add (Succ n)       m)  -- OK

C: NO
Base case: NO
                                           add  Zero    (Succ m)
=   { applying add }                                     Succ m
=   { unapplying add }               Succ (add       m   Zero)   -- NO

Inductive case: NO
                                           add (Succ n) (Succ m)
=   { applying add }                 Succ (add       n  (Succ m))
=   { induction hypothesis }   Succ (Succ (add       n        m))
=   { unapplying add }               Succ (add       m  (Succ n)) -- NO

D: NO
Base case: OK
                                           add  Zero    (Succ m)
=   { applying add }                                     Succ m
=   { unapplying add }               Succ (add  Zero          m)

Inductive case: NO
                                           add (Succ n) (Succ m)
=   { applying add }                 Succ (add       n  (Succ m))
=   { applying add }           Succ (Succ (add       n        m)) -- NO
=   { unapplying add }               Succ (add (Succ n)       m)
-}

------------------------------------------------------------------------------
-- EXERCISE 2

{-
Using exercise 1 property X:
    add n (Succ m) = Succ (add n m)
and              property Y:
    add n Zero     = n
show
    add n m = add m n (addition is commutative)
by induction on n

A: NO
Base case: NO
                                         add    Zero       m
=   { applying add }                                       m
=   { unapplying add }                   add       m    Zero -- NO

Inductive case:
                                         add (Succ n)      m
=   { applying add }               Succ (add       n       m)
=   { induction hypothesis }       Succ (add       m       n)
=   { unapplying add }                   add       m (Succ n)

B: NO
Base case: OK
                                         add    Zero       m
=   { applying add }                                       m
=   { property Y of add }                add       m    Zero -- OK

Inductive case: NO
                                         add (Succ n)      m
=   { applying add }               Succ (add       n       m) -- OK
=   { induction hypothesis }       Succ (add       m       n) -- OK?
=   { unapplying add }                   add (Succ m)      n  -- OK - but NOT what is to be shown

C: OK
Base case: OK
                                         add    Zero       m
=   { applying add }                                       m
=   { property Y of add }                add       m    Zero -- OK

Inductive case: OK
                                         add (Succ n)      m
=   { applying add }               Succ (add       n       m) -- OK
=   { induction hypothesis }       Succ (add       m       n) -- OK?
=   { property X of add }                add       m (Succ n) -- OK

D: NO
Base case: OK
                                         add    Zero       m
=   { applying add }                                       m
=   { property Y of add }                add       m    Zero -- OK

Inductive case: NO?
                                         add (Succ n)      m
=   { induction hypothesis }             add       m (Succ n)
-}

------------------------------------------------------------------------------
-- EXERCISE 3

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

all :: (a -> Bool) -> [a] -> Bool
all p    []  = True
all p (x:xs) = p x && all p xs

{-
Show that replicate produces a list with identical elements, by induction on n >= 0
with the help of the function all.

i.e., prove
    all (== x) (replicate n x)
is always true.

A: OK?
Base case: OK
                                     all (== x) (replicate 0 x)
=   { applying replicate }           all (== x) []
=   { applying all }                 True

Inductive case: OK?
                                     all (== x) (replicate (n + 1) x)
=   { applying replicate }           all (== x) (x : replicate n x)
=   { applying all }                 x == x && all (== x) (replicate n x)
=   { applying == }                  True && all (== x) (replicate n x)
=   { applying && }                  all (== x) (replicate n x)
=   { induction hypothesis }         True

B: NO
Base case: OK
                                     all (== x) (replicate 0 x)
=   { applying replicate }           all (== x) []
=   { applying all }                 True

Inductive case: NO
                                     all (== x) (replicate (n + 1) x)
=   { applying replicate }           all (== x) ([x] ++ replicate n x) -- NOT the definition of replicate
=   { applying all }                 x == x && all (== x) (replicate n x)
=   { applying == }                  True && all (== x) (replicate n x)
=   { applying && }                  all (== x) (replicate n x)
=   { induction hypothesis }         True

C: NO
Base case: NO
                                     all (== x) (replicate 1 x) -- NO : not the base case
=   { applying replicate }           all (== x) (x : []])
=   { applying all }                 x == x && all (== x) []
=   { applying == }                  True && all (== x) []
=   { applying && }                  all (== x) []
=   { applying all }                 True

Inductive case: OK
                                     all (== x) (replicate (n + 1) x)
=   { applying replicate }           all (== x) (x : replicate n x)
=   { applying all }                 x == x && all (== x) (replicate n x)
=   { applying == }                  True && all (== x) (replicate n x)
=   { applying && }                  all (== x) (replicate n x)
=   { induction hypothesis }         True

D: NO
Base case: OK
                                     all (== x) (replicate 0 x)
=   { applying replicate }           all (== x) []
=   { applying all }                 True

Inductive case: NO
                                     all (== x) (replicate (n + 1) x)
=   { applying replicate }           all (== x) (x : replicate n x)
=   { applying all }                 x == x && all (== x) (replicate n x)
=   { applying == }                  True && all (== x) (replicate n x)
=   { applying && }                  all (== x) (replicate n x)
=   { applying all }                 True -- NO
-}

------------------------------------------------------------------------------
-- EXERCISE 4

(++) :: [a] -> [a] -> [a]
[]       ++ ys =            ys
(x : xs) ++ ys = x : (xs ++ ys)

{-
show
    xs ++ [] = xs

A: NO
Base case: OK
                                    []  ++ []
=   { applying ++ }                        []

Inductive case: NO
                              (x :  xs) ++ []
=   { applying ++ }            x : (xs  ++ []) -- OK
=   { applying ++ }            x :  xs         -- NO

B: NO
Base case: OK
                                    []  ++ []
=   { applying ++ }                        []

Inductive case: NO
                              (x :  xs) ++ []
=   { applying ++ }            x :  xs         -- NO

C: NO
Base case: NO
                                    xs  ++ []
=   { applying ++ }                 xs         -- NO

Inductive case: OK?
                              (x :  xs) ++ []
=   { applying ++ }            x : (xs  ++ [])
=   { induction hypothesis }   x :  xs         -- OK?

D: OK
Base case: OK
                                    []  ++ []
=   { applying ++ }                        []

Inductive case: OK
                              (x :  xs) ++ []
=   { applying ++ }            x : (xs  ++ [])
=   { induction hypothesis }   x :  xs         -- OK?
-}

------------------------------------------------------------------------------
-- EXERCISE 5
{-
using exercise 4 (++) def, show associativity
    xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

A: OK
Base case: OK
                                        [] ++ (ys   ++ zs)
=   { applying ++ }                            ys   ++ zs  -- OK
=   { unapplying ++ }                  ([] ++  ys)  ++ zs  -- OK

Inductive case: OK
                                (x :   xs) ++ (ys   ++ zs)
=   { applying ++ }              x :  (xs  ++ (ys   ++ zs)) -- OK
=   { induction hypothesis }     x : ((xs  ++  ys)  ++ zs)  -- OK?
=   { unapplying ++ }           (x :  (xs  ++  ys)) ++ zs   -- OK
=   { unapplying ++ }          ((x :   xs) ++  ys)  ++ zs   -- OK

B: NO
Base case: NO
                                        [] ++ (ys   ++ zs)
=   { applying ++ }                            ys   ++ zs   -- OK
=   { unapplying ++ }                  (ys ++ zs)   ++ []   -- NO

Inductive case: OK
                                (x :   xs) ++ (ys   ++ zs)
=   { applying ++ }              x :  (xs  ++ (ys   ++ zs))
=   { induction hypothesis }     x : ((xs  ++  ys)  ++ zs)
=   { unapplying ++ }           (x :  (xs  ++  ys)) ++ zs
=   { unapplying ++ }          ((x :   xs) ++  ys)  ++ zs

C: NO
Base case: OK
                                        [] ++ (ys   ++ zs)
=   { applying ++ }                            ys   ++ zs   -- OK
=   { unapplying ++ }                  ([] ++  ys)  ++ zs   -- OK

Inductive case: NO
                                (x :   xs) ++ (ys   ++ zs)
=   { applying ++ }              x :  (xs  ++ (ys   ++ zs)) -- OK
=   { induction hypothesis }     x : ((xs  ++  ys)  ++ zs)
=   { unapplying ++ }           (x :  (xs  ++  ys)) ++ zs   -- NOT ENOUGH

D: NO
Base case: OK
                                        [] ++ (ys   ++ zs)
=   { applying ++ }                            ys   ++ zs
=   { unapplying ++ }                  ([] ++  ys)  ++ zs

Inductive case: NO
                                (x :   xs) ++ (ys   ++ zs)
=   { applying ++ }              x :  (xs  ++ (ys   ++ zs))
=   { induction hypothesis }     x : ((xs  ++  ys)  ++ zs)
=   { unapplying ++ }           (x :  (xs  ++  ys)) ++ zs
=   { unapplying ++ }          ((x :   ys) ++  xs)  ++ zs  -- NO
-}

------------------------------------------------------------------------------
-- EXERCISE 6

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)

{-
show
    map f (map g xs) = map (f . g) xs

A: NO
Base case: NO
                                            map  f  (map g  [])
=   { applying the outer map }                       map g  [] -- NO
=   { applying map }                                        []
=   { unapplying map }                      map (f   .   g) []

Inductive case: OK
                                            map f (map g (x : xs))
=   { applying the inner map }              map f (g x : map g xs)     -- OK
=   { applying the outer map }              f (g x) : map f (map g xs) -- OK
=   { induction hypothesis }                f (g x) : map (f . g) xs   -- OK?
=   { unapplying . }                        (f . g) x : map (f . g) xs -- OK
=   { unapplying map }                      map (f . g) (x : xs)       -- OK

B: NO
Base case: OK
                                            map  f  (map g  [])
=   { applying the inner map }              map  f          []
=   { applying map }                                        []
=   { unapplying map }                      map (f   .   g) []

Inductive case: NO
                                            map f (map g (x : xs))
=   { applying the inner map }              map f (g x : map g xs)     -- OK
=   { applying the outer map }              f (g x) : map f (map g xs) -- OK
=   { induction hypothesis }                f (g x) : map (f . g) xs   -- OK?
=   { unapplying . }                        f . g x : map (f . g) xs   -- NO
=   { unapplying map }                      map (f . g) (x : xs)

C: OK
Base case: OK
                                            map  f  (map g  [])
=   { applying the inner map }              map  f          []
=   { applying map }                                        []
=   { unapplying map }                      map (f   .   g) []

Inductive case: OK
                                            map f (map g (x : xs))
=   { applying the inner map }              map f (g x : map g xs)     -- OK
=   { applying the outer map }              f (g x) : map f (map g xs) -- OK
=   { induction hypothesis }                f (g x) : map (f . g) xs   -- OK?
=   { unapplying . }                        (f . g) x : map (f . g) xs -- OK
=   { unapplying map }                      map (f . g) (x : xs)       -- OK

D: NO
Base case: OK
                                            map  f  (map g  [])
=   { applying the inner map }              map  f          []
=   { applying map }                                        []
=   { unapplying map }                      map (f   .   g) []

Inductive case: NO
                                            map f (map g (x : xs))
=   { applying the inner map }              map f (g x : map g xs)
=   { applying the outer map }              f (g x) : map f (map g xs)
=   { induction hypothesis }                map (f . g) (x : xs)
-}

------------------------------------------------------------------------------
-- EXERCISE 7

length :: [a] -> Int
length [] = 0
length (x : xs) = 1 + length xs

{-
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

show
    length (xs ++ ys) = length xs + length ys

A: NO
Base case: NO
                                            length ([x] ++ ys)   -- NO
=	{ applying ++ }                     length (x : ys)
=	{ applying length }                 1 + length ys
=	{ unapplying length }               length [x] + length ys

Inductive case:
                                            length ((x :  xs) ++ ys)
=	{ applying ++ }                     length  (x : (xs  ++ ys))
=	{ applying length }                 1 + length (xs ++ ys)
=	{ induction hypothesis }            1 + (length xs + length ys)
=	{ associativity of + }              (1 + length xs) + length ys
=	{ unapplying length }               length (x : xs) + length ys

B: OK
Base case: OK
                                                 length ([] ++ ys)
=	{ applying ++ }                                 length ys
=	{ applying identity element of + }  0         + length ys
=	{ unapplying length }               length [] + length ys

Inductive case:
                                            length ((x :  xs) ++ ys)
=	{ applying ++ }                     length  (x : (xs  ++ ys))    -- OK
=	{ applying length }                 1 +   length (xs  ++ ys)     -- OK
=	{ induction hypothesis }            1 + (length xs + length ys)  -- OK
=	{ associativity of + }              (1 + length xs) + length ys  -- OK
=	{ unapplying length }               length (x : xs) + length ys  -- OK

C:
Base case: OK
                                                 length ([] ++ ys)
=	{ applying ++ }                                 length ys
=	{ applying identity element of + }  0         + length ys
=	{ unapplying length }               length [] + length ys

Inductive case:
                                            length ((x : xs) ++ ys)
=	{ applying length }                 1 + length (xs ++ ys)       -- OK
=	{ induction hypothesis }            1 + (length xs + length ys) -- OK
=	{ associativity of + }              (1 + length xs) + length ys -- OK
=	{ unapplying length }               length (x : xs) + length ys -- OK

D: NO
Base case: OK
                                                 length ([] ++ ys)
=	{ applying ++ }                                 length ys
=	{ applying identity element of + }  0         + length ys
=	{ unapplying length }               length [] + length ys

Inductive case: NO
                                            length ((x : xs) ++ ys)
=	{ induction hypothesis }            length (x : xs) + length ys
-}

------------------------------------------------------------------------------
-- EXERCISE 8

{-
Choose the correct explanation of the induction principle for finite lists.

The induction principle for finite lists states that in order to show
that a property P holds for all lists, it is sufficient to show that:

A: NO
- P holds for the base case: the singleton list [x]. -- NO
- if P holds for any list x : xs, then it also holds for the list xs.

B: NO
- P holds for the base case: the singleton list [x]. -- NO
- if P holds for any list xs, then it also holds for the list x : xs, for any element x.

C: NO
- P holds for the base case: the empty list []. -- OK
- if P holds for *a*   list xs, then it also holds for the list x : xs, for *an*  element x. -- NO

D: OK
- P holds for the base case: the empty list []. -- OK
- if P holds for *any* list xs, then it also holds for the list x : xs, for *any* element x. -- OK
-}

------------------------------------------------------------------------------
-- EXERCISE 9

{-
length [] = 0
length (_ : xs) = 1 + length xs
-}

take 0             _  = []
take (n + 1)      []  = []
take (n + 1) (x : xs) = x : take n xs

repeat x = x : repeat x

{-
show
    length (take n (repeat x)) = n

A: NO
Base case: NO
                                      length (take 1 (repeat x)) -- NO
=	{ applying take }             length [x]
=	{ applying length }           1

Inductive case:
                                      length (take (n + 1) (repeat x))
=	{ applying repeat }           length (take (n + 1) (x : repeat x))
=	{ applying take }             length (x : take n (repeat x))
=	{ applying length }           1 + length (take n (repeat x))
=	{ induction hypothesis }      1 + n
=	{ commutativity of + }        n + 1

B: OK
Base case: OK
                                      length (take 0 (repeat x))
=	{ applying take }             length []
=	{ applying length }           0

Inductive case: OK?
                                      length (take (n + 1) (repeat x))
=	{ applying repeat }           length (take (n + 1) (x : repeat x)) -- OK
=	{ applying take }             length (x : take n (repeat x))       -- OK
=	{ applying length }           1 + length (take n (repeat x))       -- OK
=	{ induction hypothesis }      1 + n                                -- OK?
=	{ commutativity of + }        n + 1                                -- OK

C: NO
Base case: OK
                                      length (take 0 (repeat x))
=	{ applying take }             length []
=	{ applying length }           0

Inductive case: NO
                                      length (take (n + 1) (repeat x))
=	{ applying take }             length (x : take n xs)               -- NO
=	{ applying length }           1 + length (take n xs)
=	{ induction hypothesis }      1 + n
=	{ commutativity of + }        n + 1

D: NO
Base case: OK
                                      length (take 0 (repeat x))
=	{ applying take }             length []
=	{ applying length }           0

Inductive case: NO
                                      length (take (n + 1) (repeat x))
=	{ applying take }             length (x : take n (repeat x))      -- NO
=	{ applying length }           1 + length (take n (repeat x))
=	{ induction hypothesis }      1 + n
-}

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0

-- End of file.


