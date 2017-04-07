module Main

import Data.Vect

%default total

insert :  Ord elem
       => (x : elem)
       -> (xsSorted : Vect len elem)
       -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) =
  if x < y
    then x :: y :: xs
    else y :: insert x xs

insSort :  Ord elem
        => Vect n elem
        -> Vect n elem
insSort [] = []
insSort (x :: xs) =
  let xsSorted = insSort xs in insert x xsSorted

{-
insSort [1,3,2,9,7,6,4,5,8]
-}

