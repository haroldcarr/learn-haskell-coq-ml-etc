module Main

import Data.Vect

allLengths : Vect len String -> Vect len Nat
allLengths       []  = []
allLengths (x :: xs) = length x :: allLengths xs
