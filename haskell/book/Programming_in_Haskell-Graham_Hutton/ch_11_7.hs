{-
Created       : 2015 Apr 22 (Wed) 22:00:47 by Harold Carr.
Last Modified : 2015 Apr 23 (Thu) 09:32:17 by Harold Carr.
-}

import           Data.List       (foldl')

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 11.7 Exercises

data Op = Add | Sub | Mul | Div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [ n | n > 0 ]
eval (App o l r) = [ apply o x y | x <- eval l,
                                   y <- eval r,
                                   valid o x y ]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

------------------------------------------------------------------------------
-- 1

choices2 :: [a] -> [[a]]
choices2 xs = concat [ perms x | x <- (subs xs) ]

choices3 :: [a] -> [[a]]
choices3 xs = [ ps | x  <- subs xs
                   , ps <- perms x ]

e1 :: [Test]
e1 = U.tt "e1"
     [ (choices  [1,2,3])
     , (choices2 [1,2,3])
     , (choices3 [1,2,3])
     ]
     [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1]
     ,[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]
     ]

------------------------------------------------------------------------------
-- 2

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice this that = isChoiceAux this that this
  where
    isChoiceAux      []     _ result = result == []
    isChoiceAux a@(x:xs) that result = isChoiceAux xs that (if x `elem` that then xs else a)

isChoice2 :: Eq a => [a] -> [a] -> Bool
isChoice2 this that = null $ foldl' (\a x -> if x `elem` that then tail a else a) this this

e2t :: [Test]
e2t = U.tt "e2t"
     [ isChoice  []      [1,2,3]
     , isChoice2 []      [1,2,3]
     , isChoice  [3,1]   [1,2,3]
     , isChoice2 [3,1]   [1,2,3]
     , isChoice  [3,1,2] [1,2,3]
     , isChoice2 [3,1,2] [1,2,3]
     ]
     True

e2f :: [Test]
e2f = U.tt "e2f"
     [ isChoice  [1,2,3,4] [1,2,3]
     , isChoice2 [1,2,3,4] [1,2,3]
     , isChoice  [4]       [1,2,3]
     , isChoice2 [4]       [1,2,3]
     ]
     False

------------------------------------------------------------------------------
-- 3 : TODO

------------------------------------------------------------------------------
-- 4 : TODO

------------------------------------------------------------------------------
-- 5 : TODO

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1 ++
                               e2t ++ e2f

-- End of file.


