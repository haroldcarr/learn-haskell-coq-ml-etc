{-
Created       : 2014 Dec 01 (Mon) 13:34:38 by Harold Carr.
Last Modified : 2014 Dec 01 (Mon) 16:53:45 by Harold Carr.
-}

{-# LANGUAGE NPlusKPatterns #-}

import           Data.Char
import           Data.List
import           Unsafe.Coerce

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 9

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

------------------------------------------------------------------------------
-- EXERCISE 0

n2i0 Zero = 0
n2i0 (Succ n) = n2i0 n + 1

n2i1 (Succ n) = n2i1 n + 1
n2i1 Zero = 0

n2i3 (Succ n) = 1 + n2i3 n
n2i3 Zero = 0

n2i5 = head . m
  where
    m Zero = [0]
    m (Succ n) = [ sum [ x | x <- (1 : m n) ]]

n2i6 :: Nat -> Integer
n2i6 = \n -> genericLength [c | c <- show n, c == 'S']

-- n2i7 :: Nat -> Integer
-- n2i7 = \n -> length [c | c <- show n, c == 'S']

e0 :: [Test]
e0 = U.tt "e0"
     [ n2i0 (Succ (Succ (Succ Zero)))
     , n2i1 (Succ (Succ (Succ Zero)))
     , n2i3 (Succ (Succ (Succ Zero)))
     , n2i5 (Succ (Succ (Succ Zero)))
     , n2i6 (Succ (Succ (Succ Zero)))
     ]
     3

------------------------------------------------------------------------------
-- EXERCISE 1

i2n0 0       = Zero
i2n0 (n + 1) = Succ (i2n0 n)

i2n4 (n + 1) = Succ (i2n4 n)
i2n4 0       = Zero

i2n5 (n + 1) = let m = i2n5 n in Succ m
i2n5 0       = Zero

i2n6 = head . m
  where {
        ; m 0 = [0]
        ; m (n + 1) = [sum [x | x <- (1 : m n)]]
        }

-- i2n7 :: Integer -> Nat
-- i2n7 = \n -> genericLength [c | c <- show n, isDigit c]

e1 :: [Test]
e1 = U.tt "e1"
     [ i2n0 (3::Int)
     , i2n4 (3::Int)
     , i2n5 (3::Int)
     ]
     (Succ (Succ (Succ Zero)))

------------------------------------------------------------------------------
-- EXERCISE 2

a0 Zero     n = n
a0 (Succ m) n = Succ (a0 n m)

a1 (Succ m) n = Succ (a1 n m)
a1 Zero     n = n

a2 Zero     n = Zero
a2 (Succ m) n = Succ (a2 m n)

a3 (Succ m) n = Succ (a3 m n)
a3 Zero     n = Zero

a4 n Zero     = Zero
a4 n (Succ m) = Succ (a4 n m)

a5 n (Succ m) = Succ (a4 n m)
a5 n Zero     = Zero

a6 n Zero = n
a6 n (Succ m) = Succ (a6 m n)

a7 n (Succ m) = Succ (a7 m n)
a7 n Zero = n

e2 :: [Test]
e2 = U.tt "e2"
     [ a0 (Succ (Succ Zero)) (Succ Zero)
     , a1 (Succ (Succ Zero)) (Succ Zero)
--     , a2 (Succ (Succ Zero)) (Succ Zero)
--     , a3 (Succ (Succ Zero)) (Succ Zero)
--     , a4 (Succ (Succ Zero)) (Succ Zero)
--     , a5 (Succ (Succ Zero)) (Succ Zero)
     , a6 (Succ (Succ Zero)) (Succ Zero)
     , a7 (Succ (Succ Zero)) (Succ Zero)
     ]
     (Succ (Succ (Succ Zero)))

------------------------------------------------------------------------------
-- EXERCISE 3

m' = (Succ (Succ (Succ Zero)))
n' =       (Succ (Succ Zero))

-- non exhaustive
m0 Zero Zero = Zero
m0 m (Succ n) = a0 m (m0 m n)

m1 m Zero = Zero
m1 m (Succ n) = a0 m (m1 m n)

e3 :: [Test]
e3 = U.t "e3"
     (n2i0 (m1   m'         n'))
     (n2i0       m' * n2i0  n')

------------------------------------------------------------------------------
-- EXERCISE 4

data Tree = Leaf Integer
          | Node Tree Integer Tree

o0 :: Integer -> Tree -> Bool
o0 m (Leaf n) = m == n
o0 m (Node l n r) =
    case compare m n of
        LT -> o0 m l
        EQ -> True
        GT -> o0 m r

o4 :: Integer -> Tree -> Bool
o4 m (Leaf n) = m == n
o4 m (Node l n r)
    | m == n    = True
    | m <  n    = o4 m l
    | otherwise = o4 m r

tr = (Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7)))

e4 :: [Test]
e4 = U.tt "e4"
     [ (o0 5 tr, o0 10 tr)
     , (o4 5 tr, o4 10 tr)
     ]
     (True,False)

------------------------------------------------------------------------------
-- EXERCISE 5

data BTree = BLeaf Integer
           | BNode BTree BTree
           deriving (Eq, Show)

ut = (BNode
      (BNode
       (BNode
        (BNode (BLeaf 1) (BLeaf 2))
        (BLeaf 3))
       (BLeaf 4))
      (BLeaf 5))

bt = (BNode
      (BNode
       (BNode
        (BNode (BLeaf 1) (BLeaf 2))
        (BLeaf 3))
       (BNode
        (BLeaf 4)
        (BLeaf 5)))
      (BNode
       (BNode
        (BLeaf 6)
        (BLeaf 7))
       (BNode
        (BLeaf 8)
        (BLeaf 8))))

l3 (BLeaf _) = 1
l3 (BNode l r) = l3 l + l3 r
b3 :: BTree -> Bool
b3 (BLeaf _) = True
b3 (BNode l r) = abs (l3 l - l3 r) <= 1 && b3 l && b3 r

e5 :: [Test]
e5 = U.t "e5"
     (b3 ut, b3 bt)
     (False, True)

------------------------------------------------------------------------------
-- EXERCISE 6

halve xs = splitAt (length xs `div` 2) xs
balance [x] = BLeaf x
balance xs  = BNode (balance ys) (balance zs)
  where
    (ys, zs) = halve xs

e6 :: [Test]
e6 = U.t "e6"
     (b3 (balance [0..100]))
     True

------------------------------------------------------------------------------
-- EXERCISE 9

e9 :: [Test]
e9 = U.t "e6"
     (b3 (balance [0..100]))
     True

------------------------------------------------------------------------------
-- EXERCISE 13

class Monoid a where
    mempty :: a
    (<>) :: a -> a -> a

instance Monoid Int where
    mempty = 0
    (<>)   = (+)

class (Functor f) => Foldable f where
    fold :: (Monoid m) => f m -> m

instance Foldable [] where
    fold = foldr (<>) mempty

e13 :: [Test]
e13 = U.t "e13"
     (fold [1::Int,2,3,4])
     10

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e9 ++ e13

-- End of file.


