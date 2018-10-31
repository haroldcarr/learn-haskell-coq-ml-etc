{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE PatternSynonyms      #-}

module PatternSynonyms.PF2 where

{-
later need to annotate or pass additional info

a solution
- REdefine data type using open recursion
- then take the fixed point
- enables
  - flexibility; recursion schemes
- cons
  - if library exposed constructors, all user code breaks
  - PatternSynonyms enable changing underlying data type and keep original interface

-- replace PF1 Expr data type with an unfixed version
-}
data ExprF r
  = LitF Int
  | AddF r r
  deriving Functor

newtype Fix f = Fix { unFix :: f (Fix f) }

type Expr = Fix ExprF

-- can no longer use existing 'Lit' and 'Add' functions
-- use pattern synonyms to regain

pattern Lit    :: Int -> Fix ExprF
pattern Lit n   = Fix (LitF n)
pattern Add    :: Fix ExprF -> Fix ExprF -> Fix ExprF
pattern Add x y = Fix (AddF x y)

-- same eval as PF1, except
-- - compiled against new 'Expr' def
-- - has catch-all case to keep compier happy
eval2 :: Expr -> Int
eval2 (Lit n)   = n
eval2 (Add n m) = eval2 n + eval2 m
eval2 _         = error "non-exhaustive"

test2 :: Int
test2  = eval2 (Add (Lit 3) (Lit 4))
