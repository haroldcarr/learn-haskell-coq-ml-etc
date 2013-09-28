import Test.HUnit
import AssertError

-- Currying
sum' :: (Int -> Int) -> Int -> Int -> Int
sum' f a b =
    if (a > b) then 0
    else f a + sum'     f (a+1) b

product' :: (Int -> Int) -> Int -> Int -> Int
product' f a b =
    if (a > b) then 1
    else f a * product' f (a+1) b

fact :: Int -> Int
fact n = product' (\x->x) 1 n

sp :: Int -> (Int -> Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
sp u spf f a b = iter a
  where iter x =
            if x > b then u
            else spf (f x) (iter (x+1))


-- Functions taking functions and returning functions
tolerance = 0.0001

isCloseEnough :: Double -> Double -> Bool
isCloseEnough x y =
    abs((x - y) / x) / x < tolerance

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

tests = TestList
    [
     TestCase $ assertEqual "product"      6                  (product' (\x->x+1) 1 2)
    ,TestCase $ assertEqual "fact 5"     120                  (fact 5)
    ,TestCase $ assertEqual "sp+"          5                  (sp 0 (\x y->x+y)(\x->x+1) 1 2)
    ,TestCase $ assertEqual "sp*"          6                  (sp 1 (\x y->x*y)(\x->x+1) 1 2)
    ,TestCase $ assertEqual "fixedPoint"   1.999755859375     (fixedPoint (\x -> 1 + x/2) 1)
    ,TestCase $ assertEqual "sqrt"         1.4142135623746899 (sqrt' 2)
    ]

main = runTestTT tests

-- End of file.
