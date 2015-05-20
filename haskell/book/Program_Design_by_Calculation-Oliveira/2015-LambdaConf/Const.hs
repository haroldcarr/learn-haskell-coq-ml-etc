module Const where

length0 [] = 0
length0 (x:xs) = 1 + length0 xs

length' :: [a] -> Int
length' = foldl (const . succ) 0

foldll f z []     = z
foldll f z (x:xs) = foldl f (f z x) xs

equationalReasoning :: [Int]
equationalReasoning =
    [
      length'                                     ['a','b']
    , foldl (const . succ)                 0      ['a','b']
    , foldl (const . succ) ((const . succ) 0 'a')     ['b']
    , foldl (const . succ)  (const         1 'a')     ['b']
    , foldl (const . succ)                 1          ['b']
    , foldl (const . succ) ((const . succ) 1 'b')        []
    , foldl (const . succ)  (const         2 'b')        []
    , foldl (const . succ)                 2             []
    ,                                      2
    ]

r = all (==2) equationalReasoning

-- End
