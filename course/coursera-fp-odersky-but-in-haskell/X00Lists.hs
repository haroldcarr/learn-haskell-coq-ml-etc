{-
Created       : 2013 Sep 27 (Sat) 09:01:51 by carr.
Last Modified : 2013 Sep 29 (Sun) 09:02:48 by carr.
-}

module X00Lists where

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: [Int] -> Int
max' [] = error "NoSuchElement"
max' (x:xs) = max'' x xs
    where max'' x     []  = x
          max'' x (x':xs) = max'' (if x > x' then x else x') xs

-- End of file.
