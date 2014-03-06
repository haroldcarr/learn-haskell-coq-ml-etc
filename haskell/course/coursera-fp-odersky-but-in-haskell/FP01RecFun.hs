{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2014 Mar 05 (Wed) 12:58:19 by Harold Carr.
-}

module FP01RecFun where

import           Text.Printf

triangle :: IO ()
triangle = do
    putStrLn "Pascal's Triangle"
    mapM_ putStr [show (pascal c r) ++ " " ++ lf c r | r <- [0 .. 10], c <- [0 .. r]]

lf :: Eq a => a -> a -> String
lf c r = if c == r then "\n" else ""

pascal :: Int -> Int -> Int
pascal c0 r0 =
    if c0 < 0 || r0 < 0 || c0 > r0
    then error $ printf "IllegalArgumentException: not a legal position: c:%d, r:%d" c0 r0
    else pascal' c0 r0
  where
    pascal' c r =
        if c == 0 || r == 0 || c == r then 1
        else pascal' (c-1) (r-1) + pascal' c (r-1)

balance :: String -> Bool
balance chars = balance' chars (0 :: Int)
  where
    balance'           [] count = count == 0
    balance' ('(':tchars) count =              balance' tchars (count + 1)
    balance' (')':tchars) count = count > 0 && balance' tchars (count - 1)
    balance'   (_:tchars) count =              balance' tchars  count

countChange :: Int -> [Int] -> Int
countChange money coins
    | money == 0               = 1
    | money  < 0 || null coins = 0
    | otherwise = countChange money (tail coins) + countChange (money - head coins) coins

-- End of file.
