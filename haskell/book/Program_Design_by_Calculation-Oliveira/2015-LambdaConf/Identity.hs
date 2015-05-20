module Identity where

flatten :: [[a]] -> [a]
flatten = foldr (++) []

flt0 = flatten [[1],[2],[3]]

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap g = foldr (\x acc -> g x ++ acc) []

flm0 = flatMap (\x -> [x*2]) [1,2,3]

flatten' = flatMap id

foldrr f z []     = z
foldrr f z (x:xs) = f x (foldr f z xs)

equationalReasoning :: [[Integer]]
equationalReasoning =
    [
      flatMap    id                                                      [[1],[2],[3]]
    ,                                 foldr (\x acc -> id x   ++ acc) [] [[1],[2],[3]]
    , (\x acc -> id x   ++ acc) [1] $ foldr (\x acc -> id x   ++ acc) []     [[2],[3]]
    ,            id [1] ++            foldr (\x acc -> id x   ++ acc) []     [[2],[3]]
    ,               [1] ++            foldr (\x acc -> id x   ++ acc) []     [[2],[3]]
    ,               [1] ++                             id [2] ++ foldr (\x acc -> id x   ++ acc) []  [[3]]
    ,               [1] ++                             id [2] ++                  id [3] ++ foldr (\x acc -> id x ++ acc) [] []
    ,               [1] ++                             id [2] ++                  id [3] ++      []
    ]

r = all (==[1,2,3]) equationalReasoning

-- End
