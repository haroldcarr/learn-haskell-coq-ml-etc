{-
Created       : 2013 Sep 27 (Sat) 09:01:51 by carr.
Last Modified : 2014 Mar 05 (Wed) 12:53:51 by Harold Carr.
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
max' (x:xs) = foldr (\el acc -> if el > acc then el else acc) x xs
{-
    where max'' x     []  = x
          max'' x (x':xs) = max'' (if x > x' then x else x') xs
-}

-- End of file.
