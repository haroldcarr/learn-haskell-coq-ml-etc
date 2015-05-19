module Const where

length' :: [a] -> Int
length' = foldl (const . succ) 0

-- End
