{-
Created       : 2013 Sep 27 (Sat) 09:01:51 by carr.
Last Modified : 2013 Oct 02 (Wed) 22:09:43 by carr.
-}

module X00Lists where

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
