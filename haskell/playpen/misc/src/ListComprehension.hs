module ListComprehension where

{-
[i | i <- [0..20], i `mod` 2 == 0]

[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

take 10 [ 2*x | x <- [0..], x^2 > 3 ]

-- never gets past i==1 because 2nd generator is infinite
take 10  [ (i,j) | i <- [1,2],   j <- [1..]]
-- nested now cycles through i (but with nested lists)
take  5 [[ (i,j) | i <- [1,2]] | j <- [1..]]

take 10 [ (i,j) | i <- [1..], j <- [1..i-1], gcd i j == 1 ]
take 10 [ (i,j) | i <- [1..], let k = i*i, j <- [1..k]]

let noprimes = [j | i <- [2..7], let ii = i^2, j <- [ii, ii+i .. 50]] in [x | x <- [2 .. 49], x not `elem` noprimes]
let noprimes = [j | i <- [2..7], let ii = i^2, j <- [ii, ii+i .. 50]] in [x | x <- [2 .. 49], not (x `elem` noprimes)]


let length' xs = sum [1 | _ <- xs]
length' "harold"

let removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
removeNonUppercase "Harold Carr"

[ [ x | x <- xs, even x ] | xs <- [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  ]

-- a list comprehension that builds the list of all Pythagorean triples with elements between 1 and n
-- A Pythagorean triple is defined as three positive integers (a,b,c) where a < b < c, and a^2 + b^2 = c^2.
let pyth n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]
pyth 5
pyth 25


[             truncate (x**2)  | x <- [0..10]]
[(truncate x, truncate (x**2)) | x <- [0..10]]

[(x,y) | x <- [1,2,3], y <- [3,1,4], x/=y]

let nums = [1,2,3,4]; fruits = ["apples", "peaches", "pears", "bananas"] in [(n,f) | n <- nums, f <- fruits]
let nums = [1,2,3,4]; fruits = ["apples", "peaches", "pears", "bananas"] in [(n,f) | n <- nums, f <- fruits, f!!0=='p']

[x*2   | x <- [-4, -2, 0, 2, 4]]
[x     | x <- [-4, -2, 0, 2, 4], x >= 0]
[abs x | x <- [-4, -2, 0, 2, 4]]

import Data.String.Utils
[strip x | x <- ["    apples", "    peaches    ", "pears", "bananas    "]]

-- flatten list
[ n | e <- [[1,2,3],[4,5,6],[7,8,9]], n <- e]

let myround n s = fromIntegral (round (n * factor)) / factor  where factor = fromIntegral (10^s)
[ myround pi i | i <- [1..6]]

-- transpose rows/columns
[ [row!!i | row <- [[1,2,3,4],[5,6,7,8],[9,10,11,12]]] | i <- [0..3]]

-}
