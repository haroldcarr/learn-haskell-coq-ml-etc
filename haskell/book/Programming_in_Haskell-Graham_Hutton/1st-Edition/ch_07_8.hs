{-
Created       : 2015 Apr 20 (Mon) 12:59:55 by Harold Carr.
Last Modified : 2015 Apr 22 (Wed) 17:52:44 by Harold Carr.
-}

module Ch_07_8 where

import           Data.Char       (chr, ord)

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 7.8 Exercises

------------------------------------------------------------------------------
-- 1

mf1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mf1 f p xs = [ f x | x <- xs, p x ]

mf2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mf2 f p = (map f) . (filter p)

e1 :: [Test]
e1 = U.tt "e1"
     [ mf1 (*2) (even) [1,2,3,4,5]
     , mf2 (*2) (even) [1,2,3,4,5]
     ]
     [4,8]

------------------------------------------------------------------------------
-- 2

all1 :: (a -> Bool) -> [a] -> Bool
all1 _     [] = True
all1 p (x:xs) = p x && all1 p xs

e2allt :: [Test]
e2allt = U.t "e2allt"
     (all1 (even) [2,4,6,8])
     True

e2allf :: [Test]
e2allf = U.t "e2allf"
     (all1 (even) [2,4,5,8])
     False

any1 :: (a -> Bool) -> [a] -> Bool
any1 _     [] = False
any1 p (x:xs) = p x || all1 p xs

e2anyt :: [Test]
e2anyt = U.t "e2anyt"
     (any1 (even) [2,4,6,8])
     True

e2anyf :: [Test]
e2anyf = U.t "e2anyf"
     (all1 (even) [2,4,5,8])
     False

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p xs | null xs || not (p (head xs)) = []
                | otherwise = head xs : takeWhile1 p (tail xs)

e2tw :: [Test]
e2tw = U.t "e2tw"
     (takeWhile1 (even) [2,4,6,7,8,10])
     [2,4,6]

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p ax@(x:xs) | p x       = dropWhile p xs
                       | otherwise = ax

e2dw :: [Test]
e2dw = U.t "e2dw"
     (dropWhile1 (odd) [1,3,5,8,9,10])
     [8,9,10]

------------------------------------------------------------------------------
-- 3

{-
f x : (foldr f [] xs)
-}

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\a b -> f a : b) []

e3map :: [Test]
e3map = U.t "e3map"
     (map1 (*2) [1,2,3,4])
     [2,4,6,8]
-- TODO bad definition
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\a b -> if p a then a : b else []) []

e3filter :: [Test]
e3filter = U.t "e3filter"
     (filter1 (\x -> x `mod` 17 /= 0) [1,5 .. ])
     [1,5,9,13]

e3filterbad :: [Test]
e3filterbad = U.t "e3filterbad"
     True True
{-
     (filter1 (==1) [0,0,1,1,0,0,1,0])
     [1,1,1]
-}

-----------------------------------------------------------------------------
-- 4

{-
foldl f a x:xs = foldl f (f a x) xs
-}

dec2int :: [Int] -> Int
dec2int = foldl (\a x -> (a * 10) + x) 0

e4 :: [Test]
e4 = U.t "e4"
     (dec2int [2,3,4,5])
     2345

------------------------------------------------------------------------------
-- 5

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

{-
This doesn't type check because non of the functions have the necessary a -> a type.
sumsqreven = compose [
                       sum         -- :: Num      a => [a] ->  a
                     , map (^2)    -- :: Num      a => [a] -> [a]
                     , filter even -- :: Integral a => [a] -> [a]
                     ]
-}

e5 :: [Test]
e5 = U.t "e5"
     (compose [(+1), (*2), (^2)] 2)
     9

------------------------------------------------------------------------------
-- 6

curry1 :: ((x,y) -> z) -> x -> y -> z
curry1 f x y = f (x, y)

fc :: Num a => (a, a) -> a
fc (x, y) = x + y

fc1 :: Num a => a -> a
fc1 = curry1 fc 1

e6curry = U.t "e6curry"
     (fc1 2)
     3

uncurry1 :: (x -> y -> z) -> (x,y) -> z
uncurry1 f (x,y) = f x y

fu :: Num a => a -> a -> a
fu x y = x + y

e6uncurry = U.t "e6uncurry"
     (uncurry1 fu (1,2))
     3

------------------------------------------------------------------------------
-- 7

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

-- least significant bit on left

int2bin1 :: Int -> [Int]
int2bin1 0 = []
int2bin1 n = n `mod` 2 : int2bin1 (n `div` 2)
int2bin2 :: Int -> [Int]
int2bin2 = unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int
chop81 :: [Bit] -> [[Bit]]
chop81 [] = []
chop81 bits = take 8 bits : chop81 (drop 8 bits)

chop82 :: [Bit] -> [[Bit]]
chop82 = unfold null (take 8) (drop 8)

e7chop = U.tt "e7chop"
     [ chop81 (int2bin2 62345)
     , chop82 (int2bin2 62345)
     ]
     [[1,0,0,1,0,0,0,1],[1,1,0,0,1,1,1,1]]

mapu :: (a -> b) -> [a] -> [b]
mapu f = unfold null (f . head) tail

e7map = U.t "e7map"
     (mapu (^2) [2,3,4])
     [4,9,16]

iterate1 :: (a -> a) -> a -> [a]
iterate1 f a = a : unfold (const False) f f a

e7iterate = U.tt "e7iterate"
     [ take 5 $ iterate  (+2) 1
     , take 5 $ iterate1 (+2) 1
     ]
     [1,3,5,7,9]

------------------------------------------------------------------------------
-- 8

bin2int1 :: [Bit] -> Int
bin2int1 bits = sum [ w * b | (w,b) <- zip weights bits ]
  where weights = iterate1 (*2) 1

bin2int2 :: [Bit] -> Int
bin2int2 = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

oddOnes :: [Bit] -> Bool
oddOnes = odd . sum . filter (==1)

addParity :: [Bit] -> [Bit]
addParity bits = (if oddOnes bits then 1 else 0) : bits

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin2 . ord)

checkParity :: [Bit] -> [Bit]
checkParity (x:xs) | x == 1 &&      oddOnes xs  = xs
                   | x == 0 && not (oddOnes xs) = xs
                   | otherwise                  = error "parity error"

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

decode :: [Bit] -> String
decode = map (chr . bin2int2) . (map checkParity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

e8 :: [Test]
e8 = U.t "e8"
     (transmit "Harold" == "Harold")
     True

------------------------------------------------------------------------------
-- 9

badtransmit :: String -> String
badtransmit = decode . badchannel . encode

badchannel :: [Bit] -> [Bit]
badchannel (x:xs) = (if x == 1 then 0 else 1) : xs

e9 :: [Test]
e9 = U.e "e9"
     (badtransmit "Harold" == "Harold")
     "parity error"

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1 ++
                               e2allt ++ e2allf ++
                               e2anyt ++ e2anyf ++
                               e2tw ++ e2dw ++
                               e3map ++ e3filter ++ e3filterbad ++
                               e4 ++
                               e5 ++
                               e6curry ++ e6uncurry ++
                               e7chop ++ e7map ++ e7iterate ++
                               e8 ++
                               e9

-- End of file.
