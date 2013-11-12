{-
Created       : 2013 Nov 11 (Mon) 20:51:18 by carr.
Last Modified : 2013 Nov 11 (Mon) 22:55:20 by carr.
-}

import Test.HUnit
import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util

myLast :: [a] -> a
myLast [] = error "myLast of empty"
myLast [z] = z
myLast (h:h2:t) = myLast (h2:t)

t1 = t "t1" (myLast         ['z']) 'z'
t2 = t "t2" (myLast ['x','y','z']) 'z'
t3 = t "t3" (myLast ['a' ..  'z']) 'z'

myButLast :: [a] -> a
myButLast [] = error "myButLast of empty"
myButLast [z] = error "myButLast of single element list"
myButLast [y,z] = y
myButLast (h:h2:t) = myButLast (h2:t)

t4 = t "t4" (myButLast     ['y','z']) 'y'
t5 = t "t5" (myButLast ['x','y','z']) 'y'
t6 = t "t6" (myButLast ['a' ..  'z']) 'y'

elementAt :: [a] -> Int -> a
elementAt [] _ = error "elementAt of emptyList"
elementAt (h:_) 1 = h
elementAt (_:t) n = elementAt t (n - 1)

t7 = t "t7" (elementAt   [1,2,3] 2) 2
t8 = t "t8" (elementAt "haskell" 5) 'e'

myLength :: [a] -> Int
myLength l = myLength' l 0
    where myLength' []    n = n
          myLength' (h:t) n = myLength' t (n + 1)

t9  = t  "t9" (myLength [123, 456, 789]) 3
t10 = t "t10" (myLength "Hello, world!") 13

myReverse :: [a] -> [a]
myReverse l = myReverse' l []
    where myReverse' []    r = r
          myReverse' (h:t) r = myReverse' t (h:r)

t11 = t "t11" (myReverse "A man, a plan, a canal, panama!") "!amanap ,lanac a ,nalp a ,nam A"
t12 = t "t12" (myReverse [1,2,3,4]) [4,3,2,1]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

t13 = t "t13" (isPalindrome [1,2,3]) False
t14 = t "t14" (isPalindrome "madamimadam") True
t15 = t "t15" (isPalindrome [1,2,4,8,16,8,4,2,1]) True

data NestedList a = Elem a | List [NestedList a] deriving (Eq, Show)
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List xs) = flatten (head xs) ++ flatten (List (tail xs))

t16 = t "t16" (flatten (Elem 5)) [5]
t17 = t "t17" (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) [1,2,3,4,5]
t18 = t "t18" (flatten (List [])) ([] :: [Int])

compress :: Eq a => [a] -> [a]
compress [] = []
compress (h:t) = h : compress' t h
    where compress'    [] _ = []
          compress' (h:t) x = if h == x then     compress' t x
                                        else h : compress' t h

t19 = t "t19" (compress "aaaabccaadeeee") "abcade"

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (h:t) = pack' [[h]] t
    where pack' acc              []     = myReverse acc -- TODO: do version with reverse
          pack' acc@(cx@(c:_):t) (x:xs) = if c == x then pack' ((x:cx):t) xs
                                                    else pack'  ([x]:acc) xs

t20 = t "t20" (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']) ["aaaa","b","cc","aa","d","eeee"]

encode :: Eq a => [a] -> [(Int, a)]
encode [] = error "encode empty list"
encode xs = map (\x -> (length x, head x)) (pack xs)

t21 = t "t21" (encode "aaaabccaadeeee") [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

main = runTestTT $ TestList $ t1 ++ t2 ++ t3 ++ t4 ++ t5 ++ t6 ++ t7 ++ t8 ++ t9 ++ t10 ++
                              t11 ++ t12 ++ t13 ++ t14 ++ t15 ++ t16 ++ t17 ++ t18 ++ t19 ++ t20 ++
                              t21

