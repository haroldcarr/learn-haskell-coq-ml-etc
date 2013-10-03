myDrop :: (Ord a, Num a) => a -> [a1] -> [a1]

myDrop n xs | n <= 0 || null xs = xs
            | otherwise         = myDrop (n - 1) (tail xs)
