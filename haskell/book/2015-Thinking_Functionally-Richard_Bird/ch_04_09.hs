{-
Created       : 2015 Apr 25 (Sat) 10:39:06 by Harold Carr.
Last Modified : 2015 Apr 25 (Sat) 10:39:20 by Harold Carr.
-}

import           Debug.Trace

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 4.9 Exercises

------------------------------------------------------------------------------
-- A

subtract2 :: Num a => a -> a -> a
subtract2 = flip (-)

eA :: [Test]
eA = U.tt "eA"
    [ 3 + (-2)
    , subtract  2 3
    , subtract2 2 3
    ]
    1

------------------------------------------------------------------------------
-- B : DID NOT DO

-- from book:
exp0 :: (Fractional a, Integral b) => a -> b -> a
exp0 x n = if 0 <= n then x ^ n else 1/( x ^ (negate n))

------------------------------------------------------------------------------
-- C

-- NO - because (/) needs Fractional arguments

-- from book (but won't compile if BOOK type signature included)

-- div0 :: Integral a => a -> a -> a               -- BOOK
div0 :: Integral b => Integer -> Integer -> b      -- GHCI
div0 x y = floor (fromInteger x / fromInteger y)

------------------------------------------------------------------------------
-- D

floorD :: Float -> Integer
floorD = read . (takeWhile (/= '.')) . show

eD = U.tt "eD"
    [ floor       (-2.9) == (-3)
    , floorD      (-2.9) == (-2)
    , floor  12345678.0  == 12345678
    , floorD 12345678.0  == 1
    ]
    True

------------------------------------------------------------------------------
-- E

-- http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method

--      nonneg    floor of square root
isqrt :: Float -> Integer
isqrt = floor . isqrtBab

trace' = flip trace

isqrtBab :: Float -> Float
isqrtBab n = isqrtBab' (fromIntegral (length (show n) * 10^2))
  where
    isqrtBab' :: Float -> Float
    isqrtBab' e0 = let e' = 1/2 * (e0 + n/e0) -- `trace'` ("X: " ++ show e0)
                   in if e0 == e' then e0 else isqrtBab' e'

eE :: [Test]
eE = U.tt "eE"
    [ floor (sqrt 125348.0)       == isqrt 125348.0
    , floor (sqrt  39209.3249075) == isqrt  39209.3249075
    ]
    True

------------------------------------------------------------------------------
-- F : TODO

------------------------------------------------------------------------------
-- G

data Nat = Zero | Succ Nat deriving (Eq, Show)

sub1 :: Nat -> Nat
sub1  Zero    = Zero
sub1 (Succ m) = m

-- TODO: fix this def
sub :: Nat -> Nat -> Nat
sub        Zero            _        = Zero
sub             m          Zero     = m
sub (Succ (Succ m))       (Succ n)  = sub (Succ m)      n
sub       (Succ m)  (Succ (Succ n)) = sub       m (Succ n)

add :: Nat -> Nat -> Nat
add        Zero            n        = n
add             m          Zero     = m
add       (Succ m)         n        = Succ (add m n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

instance Ord Nat where
    compare       Zero       Zero  = EQ
    compare (Succ Zero)      Zero  = GT
    compare       Zero (Succ Zero) = LT
    compare          m          n  = compare (sub1 m) (sub1 n)

eGnat :: [Test]
eGnat = U.tt "eGnat"
    [ Zero < (Succ (Succ (Succ Zero)))
    , (Succ (Succ (Succ Zero))) > Zero
    ]
    True

{-
            m n d   m n
  divmodAux 5 2 0 = 5<2==False
  divmodAux 3 2 1 = 3<2==False
  divmodAux 1 2 2 = 1<2==True : (d,m)
-}

divModNat :: Nat -> Nat -> (Nat,Nat)
divModNat m0 n = divModNat' m0 Zero
  where
    divModNat' m d | m < n     = (d,m)
                   | otherwise = divModNat' (sub m n) (add d (Succ Zero))

-- book version:

divModBook :: Nat -> Nat -> (Nat, Nat)
divModBook x y = if x < y then (Zero, x)
                 else (Succ q, r)
  where (q, r) = divModBook (sub x y) y


eGdiv :: [Test]
eGdiv = U.tt "eGdiv"
    [ divModNat (int2nat 5) (int2nat 2) == (int2nat 2, int2nat 1)
    , divModNat (int2nat 2) (int2nat 5) == (int2nat 0, int2nat 2)
    ]
    True

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ eA ++ eD ++ eE ++ eGnat ++ eGdiv

-- End of file.
