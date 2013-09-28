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
