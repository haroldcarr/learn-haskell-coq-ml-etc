{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module PatternSynonyms.PF4 where

import PatternSynonyms.PF2 (ExprF (..))

{-
Annotated inj def
- case : annotate AFTER building AST
- separate View typeclass into two classes: projection and injection
  - both might not be needed for all data types
-}

-- represents syntax trees that might have holes
data Holey (f :: * -> *)
  = Hole
  | Expr (f (Holey f))

type ExprHole = Holey ExprF

-- there is no total definition of proj
class Project a where
  proj :: a -> ExprF a

class Inject a where
  inj :: ExprF a -> a

instance Project ExprHole where
  proj _e = undefined

instance Inject ExprHole where
  inj = Expr

-- construct trees that might contain holes; later fill in holes

hole :: ExprHole
hole  = Hole

fillHole
  :: (Holey (f1 :: * -> *) -> f2 (Holey f2))
  ->  Holey (f2 :: * -> *)
  -> f2 (Holey f2)
fillHole f Hole = f Hole
fillHole _ (Expr v) = v

p4 :: ExprHole
p4 = Add (Lit 5) hole

pattern Lit :: forall a. (Project a, Inject a) => Int -> a
pattern Lit n <- (proj -> LitF n) where
  Lit n   = inj (LitF n)

pattern Add :: forall a. (Project a, Inject a) => a -> a -> a
pattern Add a b <- (proj -> AddF a b) where
  Add a b = inj (AddF a b)

{-
Unfortunately we have pushed pattern synonyms a little bit too far.
get error message for each of the patterns:

Could not deduce (Inject a) arising from a use of ‘inj’
    from the context (Project a)
      bound by the type signature for Main.I :: Project a => Int -> a
      at patterns.hs:1:1
    Possible fix:
      add (Inject a) to the context of
        the type signature for Main.I :: Project a => Int -> a
    In the expression: inj' (IF n)
    In an equation for ‘I’: I n = inj (IF n)

GHC about having different class constraints for the pattern and constructor.
I don’t think they need to have the same constraints.
It might be desirable so constructors, even synonyms, match with patterns.

------------------------------------------------------------------------------
conclusion

do not expose data types
expose pattern synonyms
-}

