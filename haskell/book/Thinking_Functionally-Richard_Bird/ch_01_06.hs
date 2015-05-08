{-
Created       : 2015 Apr 23 (Thu) 15:44:45 by Harold Carr.
Last Modified : 2015 Apr 24 (Fri) 09:55:58 by Harold Carr.
-}

import           Data.Char       (toLower)
import           Data.List       (sort)
import qualified Data.Map        as M
import           Data.Maybe      (fromJust)
import           Data.Tuple      (swap)
import qualified GHC.Exts        as G (sortWith)

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 1.6 Exercises

------------------------------------------------------------------------------
-- A

double :: Int -> Int
double x = 2 * x

checkDoubleSum :: [Int] -> Bool
checkDoubleSum xs = (sum . map double) xs == (double . sum) xs

checkMapSumConcat :: [[Int]] -> Bool
checkMapSumConcat xs = (sum . map sum) xs == (sum . concat) xs

checkSumSort :: [Int] -> Bool
checkSumSort xs = (sum . sort) xs == sum xs

eA :: [Test]
eA = U.tt "eA"
     [ map           double  [1,4,4,3] == [2, 8, 8, 6]
     , map (double . double) [1,4,4,3] == [4,16,16,12]
     , map           double  []        == []
     , checkDoubleSum []
     , checkDoubleSum [(-1), 45, 200]
     , checkMapSumConcat []
     , checkMapSumConcat [[(-1), 45, 200], [1,2,3]]
     , checkSumSort []
     , checkSumSort [(-1), 45, 200,1,2,3]
     ]
     True

------------------------------------------------------------------------------
-- B

-- (sin theta)^2

-- (2 * theta) / (2 * pi)

------------------------------------------------------------------------------
-- C

hChar :: Char
hChar = 'H'
hString :: String
hString = "H"

eC :: [Test]
eC = U.tt "eC"
     [ [1,2,3] ++ [3,2,1]        == [1,2,3,3,2,1]
     , "Hello" ++ " World!"      == "Hello World!"
     , [1,2,3] ++ []             == [1,2,3]
     , "Hello" ++ "" ++ "World!" == "HelloWorld!"
     ]
     True

------------------------------------------------------------------------------
-- D

type Text = [Char]
type Word = [Char]

sortWords :: [Word] -> [Word]
sortWords = sort

countRuns :: [Word] -> [(Int, Word)]
countRuns = countRuns' M.empty
  where
    countRuns' m    []  = map swap (M.toList m)
    countRuns' m (x:xs) = countRuns' (M.insertWith f x 1 m) xs
    f a b = a + b

sortRuns :: [(Int, Word)] -> [(Int, Word)]
sortRuns = reverse . G.sortWith fst

showRun :: (Int, Word) -> String
showRun (i,w) = w ++ " " ++ show i ++ "\n"

commonWords :: Int -> Text -> String
commonWords  n = concat . map showRun . take n . sortRuns . countRuns . sortWords . words . map toLower

commonWords2 :: Int -> Text -> String
commonWords2 n = concat . map showRun . take n . sortRuns . countRuns . sortWords . map (map toLower) . words

eD :: [Test]
eD = U.tt "eD"
    [ sortWords ["to", "be", "or", "not", "to", "be"] == ["be", "be", "not", "or", "to", "to"]
    , countRuns ["be", "be", "not", "or", "to", "to"] == [(2,"be"),(1,"not"),(1,"or"),(2,"to")]
    , sortRuns [(2,"be"),(1,"not"),(1,"or"),(2,"to")] == [(2,"to"),(2,"be"),(1,"or"),(1,"not")] -- book: [(2,"be"),(2,"to"),(1,"not"),(1,"or")]
    , commonWords  20 "To be or NOT to be that is the Question" == "to 2\nbe 2\nthe 1\nthat 1\nquestion 1\nor 1\nnot 1\nis 1\n"
    , commonWords2 20 "To be or NOT to be that is the Question" == "to 2\nbe 2\nthe 1\nthat 1\nquestion 1\nor 1\nnot 1\nis 1\n"
    ]
    True

------------------------------------------------------------------------------
-- E

eE :: [Test]
eE = U.tt "eE"
    [    1  +  ( 2 +    3 )     ==   ( 1  +   2)  +   3
    ,   [1] ++ ([2] ++ [3])     ==   ([1] ++ [2]) ++ [3]
    , ((+1) . ((*2) . (+3))) 5  == (((+1) . (*2)) . (+3)) 5
    ,    1  -  ( 2 -    3 )     /=   ( 1  -   2)  -   3
    ,               1  +  0     ==  1  &&  0  +   1     == 1
    ,               2  *  1     ==  2  &&  1  *   2     == 2
    ,              [1] ++ []    == [1] &&  [] ++ [1]    == [1]
    ,            ((+1) .  id) 3 ==  4  && (id . (+1)) 3 == 4
    ]
    True

------------------------------------------------------------------------------
-- F

-- does not consider case

pickN :: Int -> [Word] -> [Word]
pickN n = filter (\x -> length x == n)

--       given    given, perms
perms :: Word ->  (Word,[Word])
perms = undefined

showPerms :: (Word,[Word]) -> String
showPerms = undefined

--     word len -> sorted -> display
anagrams :: Int -> [Word] -> String
anagrams n = concat . map showPerms . map perms . pickN n

------------------------------------------------------------------------------
-- G

{-
One man went to mow
Went to mow a meadow
One man and his dog
Went to mow a meadow

Two men ..
..
Two men, one man and his dog
..

Three men went to mow
Went to mow a meadow
Three men, two men, one man and his dog
Went to mow a meadow

<SUB1> went to mow
Went to mow a meadow
<SUB2> <SUB3> and his dog
Went to mow a meadow
-}

first :: String
first = " went to mow\nWent to mow a meadow\n"

second :: String
second = " and his dog\nWent to mow a meadow\n"

manmen :: M.Map Int String
manmen = M.fromList [(1,"One man"),(2,"Two men"),(3,"Three men"),(4,"Four men")]

song :: Int -> String
song n0 | n0 == 0              = ""
        | n0 > (M.size manmen) = error "not implemented"
        | otherwise            = song (n0-1) ++ "\n" ++ sub1 ++ first ++ sub2 n0 ++ second
  where
    sub1   = fromJust (M.lookup n0 manmen)
    sub2 1 = "One man"
    sub2 n = concat (reverse [ fromJust (M.lookup i manmen) ++ ", " | i <- [ 2 .. n ] ]) ++ "one man"

eG :: [Test]
eG = U.t "eG"
     (song 3)
     "\nOne man went to mow\nWent to mow a meadow\nOne man and his dog\nWent to mow a meadow\n\nTwo men went to mow\nWent to mow a meadow\nTwo men, one man and his dog\nWent to mow a meadow\n\nThree men went to mow\nWent to mow a meadow\nThree men, Two men, one man and his dog\nWent to mow a meadow\n"

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ eA ++
                               eC ++
                               eD ++
                               eE ++
                               eG

-- End of file.


