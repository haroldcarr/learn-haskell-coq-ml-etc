module Aop where

import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t, tt)

-- ch 1 1/18

-- 1.1 Datatypes 1/18

-- define data types

data Bool'   = False' | True'
data Char'   = AChar | BChar -- ...
data Either' = LBool Bool' | RChar Char'
type Both    = (Bool', Char')

-- define functions on those types

char :: Char' -> Either'
char = RChar

tuple :: Bool' -> Char' -> Both
tuple = (,)

not' :: Bool' -> Bool'
not' False' = True'
not' True'  = False'

switch :: Both -> Both
switch (b,c) = (not' b, c)

-- curried

and' :: (Bool', Bool') -> Bool'
and' (False', _) = False'
and' (True', b)  = b

-- uncurried

cand :: Bool' -> Bool' -> Bool'
cand False' _ = False'
cand True'  b = b

-- Book prefers non-curried functions because product type AxB is a simpler object
-- than the function space D -> C in an abstract setting.

curry :: ((b,c) -> a) -> (b -> c -> a)
curry f b c = f (b,c)

uncurry :: (b -> c -> a) -> ((b,c) -> a)
uncurry f (b,c) = f b c

-- parameterized data types

data Maybe' a = Nothing' | Just' a

-- 1.2 Natural Numbers

-- recursive data types

data Nat = Zero | Succ Nat deriving (Eq, Show)

-- structural recursion over natural numbers

plus :: (Nat, Nat) -> Nat
plus (m, Zero)   = m
plus (m, Succ n) = Succ (plus (m,n))

mult :: (Nat, Nat) -> Nat
mult (_, Zero)   = Zero
mult (m, Succ n) = plus (m, mult (m,n))

-- use built-in numbers rather than structural numbers

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- conversion between them

itn :: Integer -> Nat
itn 0 = Zero
itn n = Succ (itn (n - 1))

nti :: Nat -> Integer
nti Zero     = 0
nti (Succ n) = 1 + nti n

-- homomorphism with local functions
f :: Nat -> Nat
f x = case x of
  Zero   -> c
  Succ n -> h (f n)
 where
  c :: Nat
  c = undefined

  h :: Nat -> Nat
  h = undefined

-- more generic homomorphism of Nat based on above:

foldn :: (t, t -> t) -> Nat -> t
foldn (c, _) Zero     = c
foldn (c, h) (Succ n) = h ((foldn (c, h)) n)

foldi :: (t, t -> t) -> Integer -> t
foldi (c, _) 0 = c
foldi (c, h) n = h ((foldi (c, h)) $ n - 1)

-- curried version of Nat functions

plusf, multf, expnf :: Nat -> Nat -> Nat
plusf m = foldn (        m, Succ)
multf m = foldn (     Zero, plusf m)
expnf m = foldn (Succ Zero, multf m)

t001 :: [Test]
t001 = U.tt "t001"
  [ plusf (itn 62) (itn 2)
  , multf (itn 32) (itn 2)
  , expnf (itn  2) (itn 6)
  , expnf (itn  8) (itn 2)
  ]
  (itn 64)

-- | Computed in linear time.
-- Uses TABULATION:
-- results are cache for subsequent use:
-- foldn ((0,1) f) n returns (fibf n, fibf (n + 1))
fibf = {- fst . -} foldn ((Zero, Succ Zero), f)
 where
  f (m, n) = (n, plusf m n)

fibi = foldi ((0, 1), f)
 where
  f (m, n) = (n, m + n)

-- | explicit recursion instead of using fold
fibgo n = go n (0,1)
  where
    go n (a, b) | n==0      = a
                | otherwise = go (n-1) (b, a+b)

fibf' i =
  let (x,y) = fibf (itn i)
  in (nti x, nti y)

t002 :: [Test]
t002 = U.t "t002"
  (fibf' 11)
  (89, 144)

------------------------------------------------------------------------------

test :: IO Counts
test =
    runTestTT $ TestList $ t001 ++ t002
