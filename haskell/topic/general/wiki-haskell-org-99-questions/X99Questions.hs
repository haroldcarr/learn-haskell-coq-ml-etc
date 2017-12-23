{-
Created       : 2013 Nov 11 (Mon) 20:51:18 by carr.
Last Modified : 2017 Dec 23 (Sat) 10:47:10 by Harold Carr.
-}

module X99Questions where

-- https://wiki.haskell.org/99_questions

import Control.Applicative ((<*>))
import Control.Arrow ((&&&))
import Control.Monad (liftM2)

import Test.HUnit
import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util

-- Problem 1

myLast, myLast', myLast'' :: [a] -> a
myLast     [] = error "myLast of empty"
myLast    [z] = z
myLast (_:xs) = myLast xs
myLast'       = foldr1 (const id)
myLast''      = foldr1 (flip const)

t1 = tt "t1" [ (myLast            "z")
             , (myLast'           "z")
             , (myLast''          "z")
             ]
             'z'
t2 = t  "t2"   (myLast          "xyz")  'z'
t3 = tt "t3" [ (myLast   ['a' ..  'z'])
             , (myLast'  ['a' ..  'z'])
             , (myLast'' ['a' ..  'z'])
             ]
             'z'

-- Problem 2

myButLast, myButLast' :: [a] -> a
myButLast    []  = error "myButLast of empty"
myButLast   [_]  = error "myButLast of single element list"
myButLast [y,_]  = y
myButLast (_:xs) = myButLast xs
myButLast' = last . init

t4 = t "t4" (myButLast         "yz")  'y'
t5 = t "t5" (myButLast        "xyz")  'y'
t6 = t "t6" (myButLast ['a' ..  'z']) 'y'

-- Problem 3

-- first element is number 1.
elementAt, elementAt', elementAt'' :: [a] -> Int -> a
elementAt    [] _ = error "out of bounds"
elementAt (h:_) 1 = h
elementAt (_:t) n
    | n < 1       = error "out of bounds"
    | otherwise   = elementAt t (n - 1)
elementAt'   xs i = xs !! (i-i)
elementAt''       = flip $ (last .) . take

t7 = tt "t7" [ (elementAt   [1,2,3] 2)
             , (elementAt'' [1,2,3] 2)
             ]
             2
t8 = tt "t8" [ (elementAt   "haskell" 5)
             , (elementAt'' "haskell" 5)
             ]
             'e'

-- Problem 4

myLength, myLength', myLength'' :: [a] -> Int
myLength   = foldl (\n _ -> n + 1) 0
myLength'  = fst . last . zip [1..]
myLength'' = sum . map (\_->1)

t9  = t  "t9" (myLength [123, 456, 789]) 3
t10 = t "t10" (myLength "Hello, world!") 13

-- Problem 5

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

t11 = t "t11" (myReverse "A man, a plan, a canal, panama!") "!amanap ,lanac a ,nalp a ,nam A"
t12 = t "t12" (myReverse [1,2,3,4]) [4,3,2,1]

-- Problem 6

isPalindrome, isPalindrome', isPalindrome'', isPalindrome''' :: Eq a => [a] -> Bool
isPalindrome   xs = xs == myReverse xs
isPalindrome'     = liftM2 (==) id reverse
isPalindrome''    = (==) <*> reverse
isPalindrome'''   = uncurry (==) . (id &&& reverse)


t13 = t "t13" (isPalindrome [1,2,3]) False
t14 = t "t14" (isPalindrome "madamimadam") True
t15 = t "t15" (isPalindrome [1,2,4,8,16,8,4,2,1]) True

-- Problem 7

data NestedList a = Elem a | List [NestedList a] deriving (Eq, Show)
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List xs) = flatten (head xs) ++ flatten (List (tail xs))

t16 = t "t16" (flatten (Elem 5)) [5]
t17 = t "t17" (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) [1,2,3,4,5]
t18 = t "t18" (flatten (List [])) ([] :: [Int])

-- Problem 8

compress :: Eq a => [a] -> [a]
compress [] = []
compress (h:t) = h : compress' t h
    where compress'    [] _ = []
          compress' (h:t) x = if h == x then     compress' t x
                                        else h : compress' t h

t19 = t "t19" (compress "aaaabccaadeeee") "abcade"

-- Problem 9

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (h:t) = pack' [[h]] t
    where pack' acc              []     = myReverse acc -- TODO: do version with reverse
          pack' acc@(cx@(c:_):t) (x:xs) = if c == x then pack' ((x:cx):t) xs
                                                    else pack'  ([x]:acc) xs

t20 = t "t20" (pack "aaaabccaadeeee") ["aaaa","b","cc","aa","d","eeee"]

-- Problem 10

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (length &&& head) (pack xs)

t21 = t "t21" (encode  "aaaabccaadeeee") [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

-- Problem 11

data Encoding a = Multiple Int a | Single a deriving (Eq, Show)
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified xs = map (\(i, a) -> if i == 1 then Single a else Multiple i a) (encode xs)

t22 = t "t22" (encodeModified "aaaabccaadeeee") [Multiple 4 'a',Single 'b',Multiple 2 'c',  Multiple 2 'a',Single 'd',Multiple 4 'e']

-- Problem 12

decodeModified :: Eq a => [Encoding a] -> [a]
decodeModified exs = foldr step [] exs
    where step (Multiple i a) acc = replicate i a ++ acc
          step (Single     a) acc =             a :  acc

t23 = t "t23" (decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']) "aaaabccaadeeee"

-- Problem 13

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect    [] = []
encodeDirect (h:t) = encodeDirect' [] 1 h t
    where encodeDirect' acc n c    []  =                               acc ++ mkEncoding n c
          encodeDirect' acc n c (x:xs) = if c == x then encodeDirect'  acc                   (n + 1) c xs
                                                   else encodeDirect' (acc ++ mkEncoding n c) 1      x xs
          mkEncoding 1 c = [Single     c]
          mkEncoding n c = [Multiple n c]

t24 = t "t24" (encodeDirect "aaaabccaadeeee") [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

-- Problem 14

dupli :: [a] -> [a]
dupli xs = foldr (\x acc -> x:x:acc) [] xs

t25 = t "t25" (dupli "abccd") "aabbccccdd"
t26 = t "t26" (dupli [1,2,3]) [1,1,2,2,3,3]

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = foldr (\x acc -> replicate n x ++ acc) [] xs

t27 = t "t27" (repli "abc" 3) "aaabbbccc"

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n
    where dropEvery'    [] _  = []
          dropEvery' (h:t) 1  =     dropEvery' t  n
          dropEvery' (h:t) n' = h : dropEvery' t (n' - 1)

t28 = t "t28" (dropEvery "abcdefghik" 3) "abdeghk"

-- Problem 17

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

t29 = t "t29" (split "abcdefghik" 3) ("abc", "defghik")

-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice   []     _  _ = []
slice (h:t) from to =
    if from <= 1 && to >= 1
    then h : slice t (from -1) (to - 1)
    else     slice t (from -1) (to - 1)

t30 = t "t30" (slice "abcdefghik" 3 7) "cdefg"

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate xs n
    | n > 0     = rotateLeft  xs n
    | otherwise = rotateRight xs n
  where
    rotateLeft     xs 0 = xs
    rotateLeft  (h:t) n = rotateLeft (t ++ [h]) (n - 1)
    rotateRight    xs 0 = xs
    rotateRight    xs n = rotateRight ((last xs) : (init xs)) (n + 1)

t31 = t "t31" (rotate ['a','b','c','d','e','f','g','h']   3 ) "defghabc"
t32 = t "t32" (rotate ['a','b','c','d','e','f','g','h'] (-2)) "ghabcdef"

-- Problem 20

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = removeAt' n [] xs
    where removeAt' _   _   []  = error "n too big"
          removeAt' 1 acc (h:t) = (h, acc ++ t)
          removeAt' n acc (h:t) = removeAt' (n - 1) (h:acc) t


t33 = t "t33" (removeAt  2 "abcd") ('b',"acd")
t34 = [ter "t34" (removeAt 10 "abcd") "exception: n too big"] -- list brackets just to satisfy types


-- ------------------------------------------------------------------------------

main = runTestTT $ TestList $  t1 ++  t2 ++  t3 ++  t4 ++  t5 ++  t6 ++  t7 ++  t8 ++  t9 ++ t10 ++
                              t11 ++ t12 ++ t13 ++ t14 ++ t15 ++ t16 ++ t17 ++ t18 ++ t19 ++ t20 ++
                              t21 ++ t22 ++ t23 ++ t24 ++ t25 ++ t26 ++ t27 ++ t28 ++ t29 ++ t30 ++
                              t31 ++ t32 ++ t33 ++ t34

