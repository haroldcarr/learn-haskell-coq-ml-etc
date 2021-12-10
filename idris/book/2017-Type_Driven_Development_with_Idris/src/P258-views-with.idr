module Main

import Data.List

------------------------------------------------------------------------------
-- VIEWS

{-
pattern matching deconsructs values

Views extend forms of patterns by defining data types.

Views are dependent types that are parameterized by data to be matched to enable
new ways of observing that data.
-}

-- A view of lists (i.e., an alternative means of viewing a List).
-- An ordinary data type
data ListLast : List a -> Type where
  Empty    :                             ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

-- 'listLast' is called a "covering function" of 'ListLast':
-- - describes how to covert into the output "view".
-- Convention : covering funs given same name as view type, with an initial lowercase.
total
listLast : (xs : List a) -> ListLast xs
listLast       []  = Empty
listLast (x :: xs) =
  case listLast xs of
    Empty         => NonEmpty [] x
    NonEmpty ys y => NonEmpty (x :: ys) y

-- example usage
describeHelper : (input : List Int) -> ListLast input -> String
describeHelper         []      Empty       = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "NonE init = " ++ show xs ++ " end = " ++ show x
--                  ^^^^^
--     this part impossible with the view

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

------------------------------------------------------------------------------
-- WITH : a construct for expressing extended pattern matching more concisely

describeListEnd' : List Int -> String
describeListEnd' input with (listLast input)
 describeListEnd'         []  |     Empty       = "Empty"
 describeListEnd' (xs ++ [x]) | (NonEmpty xs x) = "NonE init = " ++ show xs ++ " end = " ++ show x

{-
THE DIFFERENCE BETWEEN WITH AND CASE

WITH similar to CASE : matching on intermediate results

but WITH introduces a new pattern to match on the left side of a definition
- mean depen dent pattern matching can be used directly:
  in describeListEnd, enables matching on listLast result tells about form of input

WHAT CAN BE MATCHED

Cannot use just any expression in a pattern because not always possible
to decide what the inputs to a function must be, given only its result.

Idris handles patterns only when it can deduce those inputs.
- pattern consists of data constructor applied to args
  - args must be valid patterns
- value of pattern is forced by other valid pattern (e.g., describeListEnd)
-}

-- example : reversing a list

-- inefficient
-- idris cannot determine totality
myReverse : List a -> List a
myReverse input with (listLast input)
 myReverse         []  | Empty = []
 myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs

-- example : merge sort

-- view
-- note : type of SplitPair could carry proof that lefts and rights differ in size by at most one.
--        Data.List.Views has it : SplitBalanced
data SplitList : List a -> Type where
  SplitNil  :                                          SplitList []
  SplitOne  :                                          SplitList [x]
  SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

total
splitList : (input : List a) -> SplitList input
splitList input = splitListHelp input input
 where
  splitListHelp : List a -> (input : List a) -> SplitList input
  splitListHelp                  _              []  = SplitNil
  splitListHelp                  _             [x]  = SplitOne
  splitListHelp (_ :: _ :: counter) (item :: items) =
    case splitListHelp counter items of
      SplitNil               => SplitOne
      SplitOne  {x}          => SplitPair [item]          [x]
      SplitPair lefts rights => SplitPair (item :: lefts) rights
  splitListHelp                  _           items  = SplitPair [] items

-- note : Idris cannot determine totality of mergeSort
mergeSort : Ord a => List a -> List a
mergeSort input with (splitList input)
 mergeSort               []  | SplitNil                 = []
 mergeSort              [x]  | SplitOne                 = [x]
 mergeSort (lefts ++ rights) | (SplitPair lefts rights) = merge (mergeSort lefts)
                                                                (mergeSort rights)

------------------------------------------------------------------------------
-- exercises

-- enables traversal of a list several elements at a time
-- TakeN

-- groupByN : (n : Nat) -> (xs : List a) -> List (List a)
-- groupByN n xs with (takeN n xs)
--  groupByN n            xs  |  Fewer            = [xs]
--  groupByN n (n_xs ++ rest) | (Exact n_xs rest) = n_xs :: groupByN n rest

-- exgbn : (List (List Integer))
-- exgbn = groupByN 3 [1..10]
-- [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]] : List (List Integer)
