{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Sep 29 (Sun) 09:03:08 by carr.
-}

module X01RecFun where

import Control.Applicative
import Text.Printf

main = do
    putStrLn "Pascal's Triangle"
    mapM_ putStr [(show (pascal c r) ++ " " ++ (lf c r)) | r <- [0 .. 10], c <- [0 .. r]]

lf c r = if c == r then "\n" else ""

pascal :: Int -> Int -> Int
pascal c r =
    if (c < 0 || r < 0 || c > r)
    then error $ printf "IllegalArgumentException: not a legal position: c:%d, r:%d" c r
    else pascal' c r
  where
    pascal' c r =
        if (c == 0 || r == 0 || c == r ) then 1
        else pascal' (c-1) (r-1) + pascal' c (r-1)

balance :: [Char] -> Bool
balance chars = balance' chars 0
  where
    balance'           [] count = count == 0
    balance' ('(':tchars) count =              balance' tchars (count + 1)
    balance' (')':tchars) count = count > 0 && balance' tchars (count - 1)
    balance'   (_:tchars) count =              balance' tchars  count

countChange :: Int -> [Int] -> Int
countChange money coins =
    if      (money == 0)               then 1
    else if (money  < 0 || null coins) then 0
    else countChange money (tail coins) + countChange (money - (head coins)) coins

-- End of file.
