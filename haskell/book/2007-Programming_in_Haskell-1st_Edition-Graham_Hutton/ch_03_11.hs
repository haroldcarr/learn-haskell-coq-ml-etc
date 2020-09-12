{-
Created       : 2015 Apr 19 (Sun) 05:51:17 by Harold Carr.
Last Modified : 2015 Apr 19 (Sun) 06:09:23 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 3.11 Exercises

------------------------------------------------------------------------------
-- 1

a1 :: [Char]
a1 = ['a','b','c']

a2 :: (Char, Char, Char)
a2 = ('a','b','c')

a3 :: [(Bool,Char)]
a3 = [(False, 'O'),(True,'1')]

a4 :: ([Bool],[Char])
a4 = ([False,True],['0','1'])

a5 :: [[a]->[a]]
a5 = [tail,init,reverse]

------------------------------------------------------------------------------
-- 2

second :: [a] -> a
second = head . tail

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

------------------------------------------------------------------------------
-- 3 : above

------------------------------------------------------------------------------
-- 4

{-
Two functions are equal if they give the same results for all arguments.
It is not possible to determine this without executing the functions for all arguments.
If arguments are infinite then infinite solution.
-}

-- End of file.
