> {-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
> {-# LANGUAGE ConstraintKinds, KindSignatures, PolyKinds, TypeOperators #-}
> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
> {-# LANGUAGE FunctionalDependencies #-}
>
> module R3 where
>
> import Data.Proxy
> import Data.Monoid
> import Data.Reflection
> import Data.Constraint
> import Data.Constraint.Unsafe

Previous examples of creating type class instances
- have to manually 'annotate' values by wrapping in `newtype`
- newtype with phantom `s` param
  - keeps instance resolution happy
  - modicum of safety by not leaking `Proxy` values
- but inconvenient

Better: use `ConstraintKinds`, `TypeFamilies` and `unsafeCoerce`

machinery to help abstract out common patterns of creating dynamic instances
- Ed's package : constraints : http://hackage.haskell.org/package/constraints
  - see: http://neocontra.blogspot.com/2013/06/controlcategory-now-with-kind.html
  - concerning `:-` (called `|-` in article) and `Sub`/`Dict`.

--------------------------------------------------------------------------------
-- Lift/ReifiableConstraint machinery.

> newtype Lift (p :: * -> Constraint) (a :: *) (s :: *) = Lift { lower :: a }

> class ReifiableConstraint p where
>   data Def (p :: * -> Constraint) (a :: *) :: *
>   reifiedIns :: Reifies s (Def p a) :- p (Lift p a s)

> with :: Def p a -> (forall s. Reifies s (Def p a) => Lift p a s) -> a
> with d v = reify d (lower . asProxyOf v)
>   where
>     asProxyOf :: f s -> Proxy s -> f s
>     asProxyOf x _ = x

> using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
> using d m = reify d $ \(_ :: Proxy s) ->
>   let replaceProof :: Reifies s (Def p a) :- p a
>       replaceProof = trans proof reifiedIns
>         where proof = unsafeCoerceConstraint :: p (Lift p a s) :- p a
>   in m \\ replaceProof

--------------------------------------------------------------------------------
-- Examples of `ReifiableConstraint`

> instance ReifiableConstraint Eq where
>   data Def Eq a = Eq { eq_ :: a -> a -> Bool }
>   reifiedIns = Sub Dict

> instance Reifies s (Def Eq a) => Eq (Lift Eq a s) where
>   a == b = eq_ (reflect a) (lower a) (lower b)

> instance ReifiableConstraint Ord where
>   data Def Ord a = Ord { compare_ :: a -> a -> Ordering }
>   reifiedIns = Sub Dict

> instance Reifies s (Def Ord a) => Eq (Lift Ord a s) where
>   a == b = (compare a b == EQ)

> instance Reifies s (Def Ord a) => Ord (Lift Ord a s) where
>   compare a b = compare_ (reflect a) (lower a) (lower b)

> instance ReifiableConstraint Monoid where
>   data Def Monoid a = Monoid { mappend_ :: a -> a -> a, mempty_ :: a }
>   reifiedIns = Sub Dict

> instance Reifies s (Def Monoid a) => Monoid (Lift Monoid a s) where
>   mappend a b        = Lift $ mappend_ (reflect a) (lower a) (lower b)
>   mempty = a where a = Lift $ mempty_ (reflect a)
