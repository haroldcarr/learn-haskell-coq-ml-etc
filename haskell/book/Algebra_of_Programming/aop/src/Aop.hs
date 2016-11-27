module Aop where

import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

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
fib n = fib n + fib (n - 1)

f :: Nat -> Nat
f x = case x of
  Zero   -> c
  Succ n -> h (f n)
 where
  c :: Nat
  c = undefined

  h :: Nat -> Nat
  h = undefined

-- c, h and f capture in homomorphism of Nat:

foldn :: (Nat, Nat -> Nat) -> Nat -> Nat
foldn (c, _) Zero     = c
foldn (c, h) (Succ n) = h ((foldn (c, h)) n)

-- curried version of Nat functions

plusf, multf, expnf :: Nat -> Nat -> Nat
plusf m = foldn (m, Succ)
multf m = foldn (Zero, plusf m)
expnf m = foldn (Succ Zero, multf m)

i2n :: Int -> Nat
i2n 0 = Zero
i2n n = Succ (i2n (n - 1))

n2i :: Nat -> Int
n2i Zero     = 0
n2i (Succ n) = 1 + n2i n

t001 :: [Test]
t001 = U.t "t001"
       (n2i (expnf (i2n 8) (i2n 2)))
       64

------------------------------------------------------------------------------

test :: IO Counts
test =
    runTestTT $ TestList {- $ -} t001
