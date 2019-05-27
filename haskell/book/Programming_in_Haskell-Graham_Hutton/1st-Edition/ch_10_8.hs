{-
Created       : 2015 Apr 22 (Wed) 12:01:14 by Harold Carr.
Last Modified : 2015 Apr 22 (Wed) 21:57:03 by Harold Carr.
-}

import           Ch_07_8         (int2bin1)
import           Ch_09_9         (rmdups)

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 10.8 Exercises

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add  Zero    n = n
add (Succ m) n = Succ (add m n)

------------------------------------------------------------------------------
-- 1

mult :: Nat -> Nat -> Nat
mult       Zero  _ = Zero
mult (Succ Zero) n = n
mult (Succ    m) n = add n (mult m n)

e1a :: [Test]
e1a = U.tt "e1a"
     [ nat2int (mult (int2nat 3) (int2nat 5))
     , nat2int (mult (int2nat 5) (int2nat 3))
     , nat2int (mult (int2nat 1) (int2nat 15))
     , nat2int (mult (int2nat 15) (int2nat 1))
     ]
     15

e1b :: [Test]
e1b = U.tt "e1b"
     [ nat2int (mult (int2nat 0) (int2nat 5))
     , nat2int (mult (int2nat 5) (int2nat 0))
     ]
     0

------------------------------------------------------------------------------
-- 2

data Tree a = Leaf a | Node (Tree a) a (Tree a)

tree :: Tree Int
tree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs1 :: Eq a => a -> Tree a -> Bool
occurs1 m (Leaf n) = m == n
occurs1 m (Node l n r) = m == n || occurs1 m l || occurs1 m r

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 m (Leaf n) = m == n
occurs2 m (Node l n r) =
    case compare m n of
        LT -> occurs2 m l
        EQ -> True
        GT -> occurs2 m r

e2t :: [Test]
e2t= U.tt "e2t"
     [ occurs1 5 tree
     , occurs2 5 tree
     ]
     True

e2f :: [Test]
e2f= U.tt "e2f"
     [ occurs1 10 tree
     , occurs2 10 tree
     ]
     False

------------------------------------------------------------------------------
-- 3

data TreeI = LeafI Int | NodeI TreeI TreeI deriving Show

balanced :: TreeI -> Bool
balanced (LeafI _) = True
balanced (NodeI l r) =
    let numL = countLeaves l
        numR = countLeaves r
    in abs (numL - numR) <= 1

countLeaves :: TreeI -> Int
countLeaves (LeafI _) = 1
countLeaves (NodeI l r) = countLeaves l + countLeaves r

treeiBalanced1 :: TreeI
treeiBalanced1 = NodeI (NodeI (LeafI 1) (LeafI 2))
                       (NodeI (LeafI 3) (LeafI 4))

treeiBalanced2 :: TreeI
treeiBalanced2 = NodeI (LeafI 1)
                       (NodeI (LeafI 3) (LeafI 4))


treeiUnbalanced :: TreeI
treeiUnbalanced = NodeI (NodeI (LeafI 1) (NodeI (LeafI 2) (LeafI 3)))
                        (LeafI 4)

e3b :: [Test]
e3b = U.tt "e3b"
     [ balanced (LeafI 1)
     , balanced treeiBalanced1
     , balanced treeiBalanced2
     ]
     True

e3u :: [Test]
e3u = U.t "e3u"
     (balanced treeiUnbalanced)
     False

------------------------------------------------------------------------------
-- 4

splitList :: [a] -> ([a], [a])
splitList xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

balance :: [Int] -> TreeI
balance [] = error "empty list"
balance [x] = LeafI x
balance xs = NodeI (balance l) (balance r)
  where
    (l, r) = splitList xs

instance Eq TreeI where
    (LeafI l)     == (LeafI r)     = l == r
    (NodeI ll lr) == (NodeI rl rr) = ll == rl && lr == rr
    (NodeI _ _)   == (LeafI _)     = False
    (LeafI _)     == (NodeI _ _)   = False

e4a :: [Test]
e4a = U.tt "e4a"
     [ balance [1,2,3,4]   == NodeI (NodeI (LeafI 1) (LeafI 2)) (NodeI (LeafI 3) (LeafI 4))
     , balance [1,2,3,4,5] == NodeI (NodeI (LeafI 1) (LeafI 2)) (NodeI (LeafI 3) (NodeI (LeafI 4) (LeafI 5)))
     , balanced              (NodeI (NodeI (LeafI 1) (LeafI 2)) (NodeI (LeafI 3) (NodeI (LeafI 4) (LeafI 5))))
     ]
     True

------------------------------------------------------------------------------
-- 5 : extend to handle disjunction (v) and equivalence (<=>) : see Or and Equiv and friends

data Prop = Const Bool
          | Var   Char
          | Not   Prop
          | And   Prop Prop
          | Or    Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop

p1,p2,p3,p4,pOr1,pOr2,pEqv1,pEqv2,pEqv3 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 = Imply (And (Var 'A')
                (Imply (Var 'A') (Var 'B')))
           (Var 'B')
pOr1 = Or (Var 'A') (Not (Var 'A'))
pOr2 = Or (Var 'A') (Var 'B')
pEqv1 = Equiv (Or (Var 'A') (Var 'A')) (Var 'A')
pEqv2 = Equiv (And (Var 'A') (Var 'B')) (And (Var 'B') (Var 'A'))
pEqv3 = Equiv (And (Var 'A') (Var 'B')) (Or (Var 'A') (Var 'A'))

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Char -> Subst -> Bool
find _    []  = error "var not found"
find c (x:xs) | fst x == c = snd x
              | otherwise  = find c xs

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var   x)   = find x s
eval s (Not   p)   = not (eval s p)
eval s (And   p q) = eval s p && eval s q
eval s (Or    p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s (And (Imply p q) (Imply q p))

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var   x)   = [x]
vars (Not   p)   = vars p
vars (And   p q) = vars p ++ vars q
vars (Or    p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

boolsInefficient :: Int -> [[Bool]]
boolsInefficient n0 = map (map conv . make n0 . int2bin1) [ 0 .. limit ]
                     where
                       limit = (2^n0) - 1
                       make n bs = take n (bs ++ repeat 0)
                       conv 0 = False
                       conv 1 = True

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [ (eval s p) | s <- substs p ]

e5t :: [Test]
e5t = U.tt "e5t"
     [ isTaut p2
     , isTaut p4
     ]
     True

e5f :: [Test]
e5f = U.tt "e5f"
     [ isTaut p1
     , isTaut p3
     ]
     False

e5Or1 :: [Test]
e5Or1 = U.t "e5Or1"
     (isTaut pOr1)
     True

e5Or2 :: [Test]
e5Or2 = U.t "e5Or2"
     (isTaut pOr2)
     False

e5Eqv12 :: [Test]
e5Eqv12 = U.tt "e5Eqv12"
     [ isTaut pEqv1
     , isTaut pEqv2
     ]
     True

e5Eqv3 :: [Test]
e5Eqv3 = U.t "e5Eqv3"
     (isTaut pEqv3)
     False

------------------------------------------------------------------------------
-- 6 : TODO : build interactive tautology checker (i.e., define grammar, interact, parse)

------------------------------------------------------------------------------
-- 7 : extend to support multiplication : see Mul and friend
-- TODO : the EVALADD, EVALMUL method is clumsy

data Expr = Val Int | Add Expr Expr | Mul Expr Expr

value1 :: Expr -> Int
value1 (Val n) = n
value1 (Add x y) = value1 x + value1 y
value1 (Mul x y) = value1 x * value1 y

data Op = EVALADD Expr | EVALMUL Expr | ADD Int | MUL Int
type Cont = [Op]

eval2 :: Expr -> Cont -> Int
eval2 (Val n)   c = exec c n
eval2 (Add x y) c = eval2 x (EVALADD y : c)
eval2 (Mul x y) c = eval2 x (EVALMUL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y : c) n = eval2 y (ADD n : c)
exec (EVALMUL y : c) n = eval2 y (MUL n : c)
exec (ADD  n : c) m = exec c (n + m)
exec (MUL  n : c) m = exec c (n * m)

value2 :: Expr -> Int
value2 e0 = eval2 e0 []

e7add :: [Test]
e7add = U.tt "e7add"
    [ value1 (Add (Add (Val 2) (Val 3)) (Val 4))
    , value2 (Add (Add (Val 2) (Val 3)) (Val 4))
    ]
    9

e7mul :: [Test]
e7mul = U.tt "e7mul"
    [ value1 (Mul (Add (Val 2) (Val 3)) (Mul (Val 4) (Val 5)))
    , value2 (Mul (Add (Val 2) (Val 3)) (Mul (Val 4) (Val 5)))
    ]
    100

------------------------------------------------------------------------------
-- 8

data Maybe2 a = Nothing2 | Just2 a deriving (Eq, Show)

instance Monad Maybe2 where
    return = Just2
    Nothing2  >>= _ = Nothing2
    (Just2 x) >>= f = f x

e8n :: [Test]
e8n = U.tt "e8n"
     [ (Nothing2 >>= \x -> Just2 x)  :: Maybe2 Int
     , (Just2 33 >>= \_ -> Nothing2) :: Maybe2 Int
     ]
     Nothing2

e8j :: [Test]
e8j = U.t "e8j"
     ((Just2 33 >>= \x -> Just2 (x * 2)) :: Maybe2 Int)
     (Just2 66)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

toList :: List a -> [a]
toList  Nil          = []
toList (Cons x xs) = x : toList xs

fromList :: [a] -> List a
fromList     [] = Nil
fromList (x:xs) = Cons x (fromList xs)

mapL :: (a -> b) -> List a -> List b
mapL _  Nil        = Nil
mapL f (Cons x xs) = Cons (f x) (mapL f xs)

appendL :: List a -> List a -> List a
appendL  Nil        ys  = ys
appendL         xs  Nil = xs
appendL (Cons x xs) ys  = Cons x (appendL xs ys)

concatL :: List (List a) -> List a
concatL  Nil        = Nil
concatL (Cons x xs) = appendL x (concatL xs)

instance Monad List where
    return x = Cons x Nil
    Nil >>= _ = Nil
    xs  >>= f = concatL (mapL f xs)

e8m1 :: [Test]
e8m1 = U.tt "e8m1"
     [ [1,2,3] >>= \x -> [x+1,x+2,x+3]
     , toList ((fromList [1,2,3]) >>= \x -> (fromList [x+1,x+2,x+3]))
     ]
     [2,3,4,3,4,5,4,5,6]

e8m2 :: [Test]
e8m2 = U.tt "e8m2"
     [ [] >>= \x -> [x+1,x+2,x+3]
     , toList ((fromList []) >>= \x -> (fromList [x+1,x+2,x+3]))
     ]
     []

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1a ++ e1b ++
                               e2t ++ e2f ++
                               e3b ++ e3u ++
                               e4a ++
                               e5t ++ e5f ++ e5Or1 ++ e5Eqv12 ++ e5Eqv3 ++
                               e7add ++ e7mul ++
                               e8n ++ e8j ++
                               e8m1 ++ e8m2

-- End of file.


