module Identity where

flatten :: [[a]] -> [a]
flatten = foldr (++) []

flt0 = flatten [[1],[2],[3]]

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap g = foldr (\x acc -> g x ++ acc) []

flm0 = flatMap (\x -> [x*2]) [1,2,3]

flm1 = flatMap id [[1],[2],[3]]

-- End
