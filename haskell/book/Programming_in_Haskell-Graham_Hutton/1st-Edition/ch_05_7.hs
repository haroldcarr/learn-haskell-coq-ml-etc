{-
Created       : 2015 Apr 19 (Sun) 11:48:16 by Harold Carr.
Last Modified : 2015 Apr 20 (Mon) 09:18:28 by Harold Carr.
-}

import           Data.Char       (chr, isLower, isUpper, ord, toLower, toUpper)
import           Data.List       (sort)
import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 5.7 Exercises

------------------------------------------------------------------------------
-- 1

first100squared :: Int
first100squared = sum [ x^2 | x <- [1 .. 100] ]

e1 :: [Test]
e1 = U.t "e1"
     (first100squared)
     338350

------------------------------------------------------------------------------
-- 2

replicate' :: Int -> a -> [a]
replicate' n a = [ a | x <-[ 1 .. n ] ]

e2 :: [Test]
e2 = U.t "e2"
     (replicate' 3 1.0)
     [1.0,1.0,1.0]

------------------------------------------------------------------------------
-- 3


pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x,y,z) | x <- [1 .. n]
                    , y <- [1 .. n]
                    , z <- [1 .. n]
                    , z^2 == x^2 + y^2 ]


pyth2 :: Int -> [(Int, Int, Int)]
pyth2 n = [ (x,y,z) | x <- [1 .. n]
                    , y <- [x .. n]
                    , z <- [y .. n]
                    , z^2 == x^2 + y^2 ]

e3 :: [Test]
e3 = U.t "e3"
     (pyths 10)
     [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

e32 :: [Test]
e32 = U.t "e32"
     (pyth2 10)
     [(3,4,5),(6,8,10)]

------------------------------------------------------------------------------
-- 4

factors :: Int -> [Int]
factors n = [ x | x <- [ 1 .. n ], n `mod` x == 0 ]

perfects :: Int -> [Int]
perfects n = [ x | x <- [ 1 .. n ], (sum (init (factors x))) == x ]

e4 :: [Test]
e4 = U.t "e4"
     (perfects 500)
     [6,28,496]

------------------------------------------------------------------------------
-- 5

fiveA :: [(Int, Int)]
fiveA = [ (x, y) | x <- [1,2,3], y <- [4,5,6] ]

fiveB :: [(Int, Int)]
fiveB = sort $ concat [[ (x, y)
                       | x <- [1,2,3]]
                       | y <- [4,5,6]]

e5 :: [Test]
e5 = U.tt "e5"
     [ fiveA
     , fiveB
     ]
     [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

------------------------------------------------------------------------------
-- 6

-- define positions using find

positions1 :: Eq a => a -> [a] -> [Int]
positions1 x xs = [ i | (x', i) <- zip xs [0 .. n], x == x' ]
  where
    n = length xs - 1

e61 :: [Test]
e61 = U.t "e61"
     (positions1 False [True, False, True, False])
     [1,3]

find :: Eq k => k -> [(k,v)] -> [v]
find k t' = [ v | (k', v) <- t', k == k' ]

e62 :: [Test]
e62 = U.t "e62"
     (find 'b' [('a',1),('b',2),('c',3),('b',4)])
     [2,4]

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0 .. n])
  where
    n = length xs - 1

e63 :: [Test]
e63 = U.t "e61"
     (positions2 False [True, False, True, False])
     [1,3]

------------------------------------------------------------------------------
-- 7

scalarproduct1 :: [Int] -> [Int] -> Int
scalarproduct1 xs ys = sum [ xs !! i * ys !! i | i <- [ 0 .. length xs - 1 ] ]

scalarproduct2 :: [Int] -> [Int] -> Int
scalarproduct2 xs ys = sum [ x * y | (x, y) <- zip xs ys ]

e7 :: [Test]
e7 = U.tt "e7"
    [ (scalarproduct1 [1,2,3] [4,5,6])
    , (scalarproduct2 [1,2,3] [4,5,6])
    ]
    32

------------------------------------------------------------------------------
-- 8

-- TODO : reverse encode works for mixed case; crack does NOT

-- also see: http://rosettacode.org/wiki/Caesar_cipher#Haskell

-- modify caesar cipher to handle upper-case letters

{-
*Main> zip ['A' .. 'Z'] (map let2int ['A' .. 'Z'])
[('A',0),('B',1),('C',2),('D',3),('E',4),('F',5),('G',6),('H',7),('I',8),('J',9),('K',10),('L',11),('M',12),('N',13),('O',14),('P',15),('Q',16),('R',17),('S',18),('T',19),('U',20),('V',21),('W',22),('X',23),('Y',24),('Z',25)]

*Main> zip ['a' .. 'z'] (map let2int ['a' .. 'z'])
[('a',32),('b',33),('c',34),('d',35),('e',36),('f',37),('g',38),('h',39),('i',40),('j',41),('k',42),('l',43),('m',44),('n',45),('o',46),('p',47),('q',48),('r',49),('s',50),('t',51),('u',52),('v',53),('w',54),('x',55),('y',56),('z',57)]
-}

let2int :: Char -> Int
let2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isUpper c =          int2let ((let2int c           + n) `mod` 26)
          | isLower c = toLower (int2let ((let2int (toUpper c) + n) `mod` 26))
          | otherwise = c
{-
shift 1 'z'
shift (-1) 'a'
encode 1    "azAZ!"
encode (-1) "baBA!"
-}

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [ x | x <- xs, isLower x ]

uppers :: String -> Int
uppers xs = length [ x | x <- xs, isUpper x ]

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs, x == x' ]

freqs :: String -> [Float]
freqs xs = [ percent (count x (map toLower xs)) n | x <- ['a' .. 'z']]
  where
    n = lowers xs + uppers xs

-- freqs "abbcccddddeeeee"
-- freqs "ABBCCCDDDDEEEEE"
-- freqs "AbBCCcDDdDEEeEE"

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^2) / e | (o, e) <- zip os es ]

-- [ chisqr (rotate n (freqs "kdvnhoo lv ixq")) table | n <- [0 .. 25] ]
-- [ chisqr (rotate n (freqs "Kdvnhoo lv Fxq")) table | n <- [0 .. 25] ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- rotate 3 [1,2,3,4,5]

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions1 (minimum chitab) chitab)
    chitab = [ chisqr (rotate n table') table | n <- [0 .. 25] ]
    table' = freqs (map toLower xs)

-- encode   3  "haskell is fun!"
-- encode (-3) "kdvnhoo lv ixq!"
-- crack       "kdvnhoo lv ixq!"
-- encode   3  "Haskell is Fun! azAZ"
-- encode (-3) "Kdvnhoo lv Ixq! dcDC"
-- crack       "Kdvnhoo lv Ixq! dcDC"

e81 :: [Test]
e81 = U.t "e81"
     (lowers "Haskell")
     6

e82 :: [Test]
e82 = U.t "e82"
     (count 's' "Mississippi")
     4

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1 ++
                               e2 ++
                               e3 ++ e32 ++
                               e4 ++
                               e5 ++
                               e61 ++ e62 ++ e63 ++
                               e7 ++
                               e81 ++ e82

-- End of file.
