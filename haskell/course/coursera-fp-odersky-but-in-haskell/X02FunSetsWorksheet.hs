{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Oct 07 (Mon) 19:20:56 by carr.
-}

module X02FunSetsWorksheet
( sum'
, product'
-- ...
-- note: we would not export `reduce` if this was a module for Rational
) where

import Test.HUnit
import AssertError

-- currying
sum' :: (Int -> Int) -> Int -> Int -> Int
sum' f a b =
    if a > b then 0
    else f a + sum'     f (a + 1) b

product' :: (Int -> Int) -> Int -> Int -> Int
product' f a b =
    if a > b then 1
    else f a * product' f (a +1 ) b

fact :: Int -> Int
fact = product' id 1

sp :: Int -> (Int -> Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
sp u spf f a b = iter a
  where iter x =
            if x > b then u
            else spf (f x) (iter (x+1))


-- higher-order functions: functions that take functions as args and/or return functions results
tolerance = 0.0001

isCloseEnough :: Double -> Double -> Bool
isCloseEnough x y =
    abs((x - y) / x) / x < tolerance

{-# ANN fixedPoint "HLint: ignore Eta reduce" #-}
fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f firstGuess = iter firstGuess
  where iter guess =
            let next = f guess
            in if isCloseEnough guess next then next
               else iter next

averageDamp :: (Double -> Double) -> Double -> Double
averageDamp f x = (x + f x) / 2


sqrt' :: Double -> Double
sqrt' x = fixedPoint (averageDamp (\y -> x / y)) 1.0

higherOrderFunctionTests = TestList
    [teq "product"    (product' (+ 1) 1 2)            6
    ,teq "fact 5"     (fact 5)                      120
    ,teq "sp+"        (sp 0 (+) (+ 1) 1 2)            5
    ,teq "sp*"        (sp 1 (*) (+ 1) 1 2)            6
    ,teq "fixedPoint" (fixedPoint (\x -> 1 + x/2) 1)  1.999755859375
    ,teq "sqrt"       (sqrt' 2)                       1.4142135623746899
    ]

-- Data

type Numer = Int
type Denom = Int
data Rational' = Rational' Numer Denom

add :: Rational' -> Rational' -> Rational'
add (Rational' n1 d1) (Rational' n2 d2) =
    Rational' (n1 * d2 + n2 * d1) (d1 * d2)
(+:) :: Rational' -> Rational' -> Rational'
(+:)  = add

neg :: Rational' -> Rational'
neg (Rational' n d) = Rational' (-n) d
-- custom unary prefix operator:
-- http://haskell.1045720.n5.nabble.com/Custom-unary-operator-extension-td3099118.html

sub :: Rational' -> Rational' -> Rational'
sub r1 r2 = add r1 $ neg r2
(-:) :: Rational' -> Rational' -> Rational'
(-:) = sub

less :: Rational' -> Rational' -> Bool
less (Rational' n1 d1) (Rational' n2 d2) = n1 * d2 < n2 * d1
(<:)  :: Rational' -> Rational' -> Bool
(<:) = less

instance Show Rational' where
    show (Rational' n d) = show n ++ "/" ++ show d

x = Rational' 1 3
y = Rational' 5 7
z = Rational' 3 2

-- http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Ratio.html
-- http://www.haskell.org/ghc/docs/7.4.2/html/libraries/base/src/GHC-Real.html

reduce :: Rational' -> Rational'
reduce (Rational' _ 0) = error "Rational': zero denominator"
reduce (Rational' n d) = Rational' (n `quot` g) (d `quot` g)
  where
    g = gcd n d
    gcd a b = if b == 0 then a else gcd b $ a `mod` b

dataTypeTests = TestList
    [teq "add rat"           (show $ add x y)           "22/21"
    ,teq "add rat infix"     (show $ x `add` y)         "22/21"
    ,teq "add rat operator"  (show $ x +:    y)         "22/21"
    ,teq "sub rat"           (show   (sub (sub x y) z)) "-79/42"
    ,teq "sub rat infix"     (show $ x `sub` y `sub` z) "-79/42"
    ,teq "sub rat op"        (show $ x -:    y -:    z) "-79/42"
    ,teq "add rat self"      (show $ add y y)           "70/49"
    ,teq "add rat self gcd"  (show $ reduce $ add y y)  "10/7"
    ,ter "rat zero denom"    (reduce (Rational' 1 0))   "Rational': zero denominator"
    ,teq "less rat"          (less x   y)               True
    ,teq "less rat infix"    (x `less` y)               True
    ,teq "less rat operator" (x <:     y)               True
    ]

main = do
    runTestTT higherOrderFunctionTests
    runTestTT dataTypeTests

-- End of file.
