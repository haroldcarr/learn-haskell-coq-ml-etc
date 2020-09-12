{-# LANGUAGE ImpredicativeTypes #-} -- for b7
{-
Created       : 2015 Apr 24 (Fri) 09:57:33 by Harold Carr.
Last Modified : 2015 Apr 24 (Fri) 22:07:08 by Harold Carr.
-}

import           Data.Char       (isAlpha, toLower, toUpper)

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 2.8 Exercises

------------------------------------------------------------------------------
-- A

-- three : correct answer is "Yes"

------------------------------------------------------------------------------
-- B

double :: Num a => a -> a
double = (2*)

-- b1 = [0,1) -- bad
-- b2 = double -3 -- bad
b3 :: Num a => a
b3 = double (-3)
-- b4 = double double 0 -- bad : app first
-- b4 = if 1 == 0 then 2==1 -- bad : no else

b6 :: Bool
b6 = "++" == "+" ++ "+"

b7 :: [Num a => a -> a -> a]
b7 = [(+),(-)]

b8 :: [[[[a]]]]
b8 = [[],[[]],[[[]]]] -- wrong answer: bad : elements are different types
-- b9 = concat [" tea"," for",'2'] -- bad : elements are different types

b10 :: String
b10 = concat ["tea","for","2"]

------------------------------------------------------------------------------
-- C

modernize :: String -> String
modernize = unwords . map (\(x:xs) -> toUpper x : xs) . words

eC :: [Test]
eC = U.t "eC"
     (modernize "The morphology of prex - an essay in meta-algorithmics")
     "The Morphology Of Prex - An Essay In Meta-algorithmics"

------------------------------------------------------------------------------
-- D

-- eager beaver : n
-- lazy  susan  : 1
-- f (head xs)

-- eager beaver : would have to evaluate entire list

first :: (a -> Bool) -> [a] -> a
first p xs | null xs   = error "Empty list"
           | p x       = x
           | otherwise = first p (tail xs)
  where x = head xs

-- f . first p

eD :: [Test]
eD = U.tt "eD"
    [ (head . filter odd) [2,4,6,7,8]
    , first odd           [2,4,6,7,8]
    ]
    7

------------------------------------------------------------------------------
-- E

firstMaybe :: (a -> Bool) -> [a] -> Maybe a
firstMaybe _ []     = Nothing
firstMaybe p (x:xs) | p x       = Just x
                    | otherwise = firstMaybe p xs

-- 4 : wrong : book says 6 - because includes undefined in both Just and Nothing cases

eE :: [Test]
eE = U.t "eE"
    (firstMaybe odd [2,4,6,7,8])
    (Just 7)

------------------------------------------------------------------------------
-- F

-- O(n)

{-
4 3 2 1
2*2*2*2
3*3*3*3
-}

exp1 :: Integer -> Integer -> Integer
exp1 x n | n == 0    = 1
         | n == 1    = x
         | otherwise = x * exp1 x (n-1)

-- http://rosettacode.org/wiki/Exponentiation_operator#ALGOL_68

exp2 :: Integer -> Integer -> Integer
exp2 b e | e == 0 = 1
         | e == 1 = b
         | b == 0 = 0
         | b == 1 = 1
         | even e = (b*b) `exp2` (e `div` 2)
         | odd  e = (b*b) `exp2` (e `div` 2) * b

-- O(log N)

eF :: [Test]
eF = U.t "eF"
    (and [ exp1 b e == exp2 b e | b <- [ 0 .. 99 ], e <- [ 0 .. 99 ] ])
    True

------------------------------------------------------------------------------
-- G

type Date = (Int, Int, Int)

showDate :: Date -> String
showDate (d, m, y) =
    show d ++ showTh d ++ " " ++ showMonth m ++ ", " ++ show y

showTh :: Int -> String
showTh n = case last (show n) of
               '1' -> if n == 11 then "th" else "st"
               '2' -> "nd"
               '3' -> "rd"
               _   -> "th"

showMonth :: Int -> String
showMonth 11 = "November"
showMonth 12 = "December"
showMonth  _ = "..."

eG :: [Test]
eG = U.tt "eG"
    [ showDate (10,12,2013) == "10th December, 2013"
    , showDate (21,11,2020) == "21st November, 2020"
    ]
    True

------------------------------------------------------------------------------
-- H

type CIN = String

addSum :: CIN -> CIN
addSum n = n ++ show (sumCin n)

sumCin :: CIN -> Int
sumCin = sum . map fromDigit

fromDigit :: Char -> Int
fromDigit c = read [c]

valid :: CIN -> Bool
valid xs = sumCin n == cs
  where
    (n,cs0) = splitAt 8 xs
    cs      = read cs0

eH :: [Test]
eH = U.tt "eH"
    [ addSum "63245134" == "6324513428"
    , valid "6324513428" == True
    ]
    True

------------------------------------------------------------------------------
-- I

onlyAlpha :: String -> String
onlyAlpha = filter isAlpha

toLowerCase :: String -> String
toLowerCase = map toLower

isPalindrome :: String -> Bool
isPalindrome s = toLowerCase (onlyAlpha s) == reverse (toLowerCase (onlyAlpha s))

palindrome :: IO ()
palindrome = do
    putStrLn "Enter a string:"
    s <- getLine
    putStrLn (if isPalindrome s then "Yes!" else "No!")
    return ()

eI :: [Test]
eI = U.tt "eI"
    [ isPalindrome "Madam, I'm Adam" == True
    , isPalindrome "A Man, a plan, a canal - Suez!" == False
    , isPalindrome "Doc, note I dissent. A fast never prevents a fatness.\nI diet on cod." == True
    ]
    True

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ eC ++ eD ++ eE ++ eF ++ eG ++ eH ++ eI

-- End of file.
