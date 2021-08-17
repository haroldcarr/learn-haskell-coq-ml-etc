-- sets as lists

import Data.List (nub)

a = [1,2]
b = [2,3,4]
c = []
v = ['a', 'b', 'c']
w = ['b', 'c', 'd']

product'   a b   = [(a,b)   | a <- a, b <- b]
product3   a b c = [(a,b,c) | a <- a, b <- b, c <- c]
union'     a b   = nub (a ++ b)
intersect' a b   = [ x | x <- a, x `elem` b]
-- http://stackoverflow.com/questions/6428279/why-data-set-has-no-powerset-function
powerset' :: [a] -> [[a]]
powerset' [] = [[]]
powerset' (x:xs) = powerset' xs ++ map (x:) (powerset' xs)

e_9_1_a = product'   a b
e_9_1_b = union'     a b
e_9_1_c = intersect' a b
e_9_1_d = product'   b c
e_9_1_e = union'     b c
e_9_1_f = intersect' b c
e_9_1_g = product'   b b
e_9_1_h = product3   a a a
e_9_1_i = powerset'  a
e_9_1_j = powerset'  c
e_9_1_k = powerset'  [1]

