{-
Created       : 2013 Sep 27 (Sat) 09:01:51 by carr.
Last Modified : 2013 Nov 06 (Wed) 18:22:40 by carr.
-}

module FP00Lists where

{-# ANN sum' "HLint: ignore Use sum" #-}
sum' :: [Int] -> Int
sum' = foldr (+) 0
{-
sum'     [] = 0
sum' (x:xs) = x + sum' xs
-}

max' :: [Int] -> Int
max'     [] = error "NoSuchElement"
max' (x:xs) = foldl (\ x x' -> if x > x' then x else x') x xs
{-
    where max'' x     []  = x
          max'' x (x':xs) = max'' (if x > x' then x else x') xs
-}

-- End of file.
