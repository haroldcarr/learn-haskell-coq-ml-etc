{-
Created       : 2015 Apr 23 (Thu) 09:34:25 by Harold Carr.
Last Modified : 2015 Apr 23 (Thu) 11:42:48 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 12.9 Exercises

{-
REDUCTION

- INNERMOST : redex that contains no other redex (choose left if more than one)
  - evaluates args to functions before application : BY VALUE
- OUTERMOST : redex that is contained in no other redex (choose left if more than one)
  - application before arg eval : BY NAME
  - can handle (potentially ) INFINITE structures
  - note: some builtin funs (eg., (+)) are STRICT, so eval args first anyway
  - efficiency : square n = n * n
    - rather than evaluate square (1+2) (1+2) "n" expression twice
      SHARE argument and only eval once
  - LAZY : OUTERMOST + SHARE
- lambda expressions cannot be reduced until applied

MODULAR PROGRAMMING

- lazy enables separation of control from data

define in module A: ones = 1 : ones
use in module    B: take 3 ones

- without lazy, need to combine

 replicate 0 _ = []
 replicate n x = x : replicate n x

- but care needed, e.g.:

 filter    (<=5) [1 .. ] -- non-terminating
 takeWhile (<=5) [1 .. ] --     terminating

STRICT

- strict function application
  square $! (1 + 2)
  (f $! x) y    -- eval x before app
  (f x) $! y    -- eval y before app
  (f $! x) $! y -- eval both
-}

------------------------------------------------------------------------------
-- 1

p x y = x + y
m x y = x * y

e1a :: [Test]
e1a = U.tt "e1a"
     [ 1 `p` (2 `m` 3)
       -- inner
     ,  1 `p` (2  *  3)
     , 1 `p` 6
       -- outer
     , 1  +  (2  *  3)
     , 1  +  6
     ]
     7

e1b :: [Test]
e1b = U.tt "e1b"
     [ (1 `p` 2) `m` (2 `p` 3)
       -- inner
     , (1  +  2) `m` (2 `p` 3)
     , 3         `m` (2 `p` 3)
     , 3         `m` (2  +  3)
     , 3         `m` 5
     , 3          *  5
       -- outer
     , (1 `p` 2)  *  (2 `p` 3)
     , (1  +  2)  *  (2 `p` 3)
     ]
     15

e1c :: [Test]
e1c = U.tt "e1c"
     [ fst (1 `p` 2, 2 `p` 3)
       -- inner
     , fst (1  +  2, 2 `p` 3)
     , fst (3      , 2 `p` 3)
     , fst (3      , 5      )
     ,      3
       -- outer
     ,      1 `p` 2
     ,      3
     ]
     3

e1d :: [Test]
e1d = U.tt "e1d"
     [ (\x -> 1 `p` x) (2 `m` 3)
       -- inner
     , (\x -> 1 `p` x) 6
     ,        1 `p` 6
       -- outer
     ,        1 `p` (2 `m` 3)
     ,        1  +  (2 `m` 3)
     ,        1  +  (2  *  3)
     ,        1  +  6
     ]
     7

------------------------------------------------------------------------------
-- 2 : see e1c

------------------------------------------------------------------------------
-- 3

mult :: Num a => a -> a -> a
mult = \x -> \y -> x `m` y

e3 :: [Test]
e3 = U.tt "e3"
     [ mult 3 4
     , (\x -> \y -> x `m` y) 3 4
     ,       (\y -> 3 `m` y)   4
     ,              3 `m` 4
     ,              12
     ]
     12

------------------------------------------------------------------------------
-- 4

{-
0,1,1,2,3,5, 8
  0,1,1,2,3, 5,8
  1,2,3,4,8,12
-}

fibs :: [Integer]
fibs = 0 : 1 : [ x + y | (x,y) <- zip fibs (tail fibs) ]

------------------------------------------------------------------------------
-- 5

fib :: Int -> Integer
fib n = last $ take (n + 1) fibs

e5a :: [Test]
e5a = U.t "e5a"
     (fib  9)
     34

fibGT :: Integer -> Integer
fibGT n = head $ filter (n<=) fibs

e5b :: [Test]
e5b = U.t "e5b"
     (fibGT  1000)
     1597

------------------------------------------------------------------------------
-- 6

{-
BALANCED (filling from right)
             Node Leaf               1 Leaf
             Node Leaf               1 (Node Leaf 1 Leaf)
             Node (Node Leaf 1 Leaf) 1 (Node Leaf 1 Leaf)
Node Leaf 1 (Node (Node Leaf 1 Leaf) 1 (Node Leaf 1 Leaf))

UNBALANCED (filling from right)
                                       Node Leaf 1 Leaf
                          Node Leaf 1 (Node Leaf 1 Leaf)
             Node Leaf 1 (Node Leaf 1 (Node Leaf 1 Leaf))
Node Leaf 1 (Node Leaf 1 (Node Leaf 1 (Node Leaf 1 Leaf)))
-}

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

-- unbalanced versions

repeatT :: a -> Tree a
repeatT x = Node Leaf x (repeatT x)

takeT :: Int -> Tree a -> Tree a
takeT 0  _           = Leaf
takeT _  Leaf        = Leaf
takeT n (Node l x r) = Node l x (takeT (n - 1) r)

e6a :: [Test]
e6a = U.t "e6a"
     ((takeT 3 (repeatT 1))::Tree Int)
     (Node Leaf 1 (Node Leaf 1 (Node Leaf 1 Leaf)))

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT

e6b :: [Test]
e6b = U.t "e6b"
     ((replicateT 3 1)::Tree Int)
     (Node Leaf 1 (Node Leaf 1 (Node Leaf 1 Leaf)))

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1a ++ e1b ++ e1c ++ e1d ++
                               e3 ++
                               e5a ++ e5b ++
                               e6a ++ e6b

-- End of file.


