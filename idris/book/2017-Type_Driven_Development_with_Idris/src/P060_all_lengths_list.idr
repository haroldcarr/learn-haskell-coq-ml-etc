module Main

allLengths : List String -> List Nat
allLengths       []  = []
allLengths (x :: xs) = length x :: allLengths xs

{-
allLengths ["one", "two", "three"]
-}
