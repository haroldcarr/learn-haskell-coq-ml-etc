{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Oct 02 (Wed) 21:53:09 by carr.
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

fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f = iter
  where iter guess =
            let next = f guess
            in if isCloseEnough guess next then next
               else iter next

averageDamp :: (Double -> Double) -> Double -> Double
averageDamp f x = (x + f x) / 2


sqrt' :: Double -> Double
sqrt' x = fixedPoint (averageDamp (\y -> x / y)) 1.0

higherOrderFunctionTests = TestList
    [TestCase $ assertEqual "product"      6                  (product' (+ 1) 1 2)
    ,TestCase $ assertEqual "fact 5"     120                  (fact 5)
    ,TestCase $ assertEqual "sp+"          5                  (sp 0 (+) (+ 1) 1 2)
    ,TestCase $ assertEqual "sp*"          6                  (sp 1 (*) (+ 1) 1 2)
    ,TestCase $ assertEqual "fixedPoint"   1.999755859375     (fixedPoint (\x -> 1 + x/2) 1)
    ,TestCase $ assertEqual "sqrt"         1.4142135623746899 (sqrt' 2)
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
    [TestCase $ assertEqual "add rat"           "22/21"     (show $ add x y)
    ,TestCase $ assertEqual "add rat infix"     "22/21"     (show $ x `add` y)
    ,TestCase $ assertEqual "add rat operator"  "22/21"     (show $ x +:    y)
    ,TestCase $ assertEqual "sub rat"          "-79/42"     (show   (sub (sub x y) z))
    ,TestCase $ assertEqual "sub rat infix"    "-79/42"     (show $ x `sub` y `sub` z)
    ,TestCase $ assertEqual "sub rat op"       "-79/42"     (show $ x -:    y -:    z)
    ,TestCase $ assertEqual "add rat self"      "70/49"     (show $ add y y)
    ,TestCase $ assertEqual "add rat self gcd"  "10/7"      (show $ reduce $ add y y)
    ,TestCase $ assertError "rat zero denom" "Rational': zero denominator"
                                                            (reduce (Rational' 1 0))
    ,TestCase $ assertEqual "less rat"          True        (less x   y)
    ,TestCase $ assertEqual "less rat infix"    True        (x `less` y)
    ,TestCase $ assertEqual "less rat operator" True        (x <:     y)
    ]

main = do
    runTestTT higherOrderFunctionTests
    runTestTT dataTypeTests

-- End of file.
