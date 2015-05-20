module Const where

length' :: [a] -> Int
length' = foldl (const . succ) 0

foldll f z []     = z
foldll f z (x:xs) = foldl f (f z x) xs

equationalReasoning :: [Int]
equationalReasoning =
    [
      length' ['a','b']
    , foldl (const . succ)                 0      ['a','b']
    , foldl (const . succ) ((const . succ) 0 'a')     ['b']
    , foldl (const . succ)                 1          ['b']
    , foldl (const . succ) ((const . succ) 1 'b')        []
    , foldl (const . succ)                 2             []
    ,                                      2
    ]

r = all (==2) equationalReasoning

-- End
