{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ViewPatterns         #-}

module PatternSynonyms.PF3 where

import PatternSynonyms.PF2 (Expr, ExprF (..), Fix (..))

{-
------------------------------------------------------------------------------
bidirectional pattern synonyms
- enable specifing constructor besides pattern

feature: vary underlying type by providing "mapping" functions

example : Annotate AST
-}
type Label = String
data Annotate (f :: * -> *) = Annotate Label (f (Annotate f))

-- take the fixed point to get an annotated AST
type Annotated = Annotate ExprF
{-
want to use existing functions for both (un)annotated values
- but can only have one pattern per constructor
- workaround via view patterns and a type class
-}
class View a where
  proj :: a -> ExprF a
  inj  :: ExprF a -> a

instance View Expr where
  proj = unFix
  inj  = Fix

instance View (Annotate ExprF) where
  proj (Annotate _ e) = e
  inj v               = Annotate (mkLabel v) v

mkLabel :: ExprF Annotated -> Label
mkLabel  = undefined

-- 'pattern' is PatternSynonyms
pattern Lit :: forall a. View a => Int -> a
pattern Lit n <- (proj -> LitF n) where -- (proj -> ..) is ViewPatterns
  Lit n   = inj (LitF n)

pattern Add :: forall a. View a => a -> a -> a
pattern Add a b <- (proj -> AddF a b) where
  Add a b = inj (AddF a b)

-- test

eval3 :: View a => a -> Int
eval3 (Lit n)   = n
eval3 (Add a b) = eval3 a + eval3 b
eval3  _        = undefined

test3 :: (Int, Int)
test3 = ( eval3 (Add (Lit 5) (Lit 6) :: Expr)
        , eval3 (Add (Lit 5) (Lit 6) :: Annotated)
        )
