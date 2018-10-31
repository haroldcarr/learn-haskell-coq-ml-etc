module PatternSynonyms.PF1 where

{-
Pain Free Unfix with Pattern Synonyms

Posted on November 27, 2014

library using closed data types

Example
-}
data Expr
  = Lit Int
  | Add Expr Expr

-- typically operations are defined as traversals of syntax tree:

eval1 :: Expr -> Int
eval1 (Lit n)   = n
eval1 (Add n m) = eval1 n + eval1 m

test1 :: Int
test1  = eval1 (Add (Lit 3) (Lit 4))
