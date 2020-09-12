> {-# LANGUAGE DataKinds      #-}
> {-# LANGUAGE GADTs          #-}
> {-# LANGUAGE KindSignatures #-}
>
> module Ch07_06_type_promotion where

Type promotion

introduced in same paper as kind polymorphism (Giving Haskell a Promotion, by Yorgey et al in 2012).

> data Zero   = Zero   deriving Show
> data Succ n = Succ n deriving Show

> one = Succ Zero
> two = Succ one

bad expressions allowed:

> badSucc1 = Succ 10    -- :: Succ Int
> badSucc2 = Succ False -- :: Succ Bool

Size-aware list

> data Vec :: (* -> * -> *) where
>   Nil  :: Vec a Zero
>   Cons :: a -> Vec a n -> Vec a (Succ n) -- increments size

> nil' = Nil :: Vec Int Zero

> cons1 = Cons 3 nil'  -- :: Vec Int       (Succ Zero)
> cons2 = Cons 5 cons1 -- :: Vec Int (Succ (Succ Zero))

Vec has two type parameters
- 1st : list data
- 2nd : size (but not enforced by type-checker)

each `Cons` returns a different type returned (size is not term-level, it is type-level function)

But bad stuff possible:

> badVec = Nil :: Vec Zero Zero

-- INVALID
-- badVec2 = Nil :: Vec Zero Bool

need more type-safety

DataKinds extension : datatypes at kind level

Promoting types to kinds

Vec above expresses type-level programming, where kinds only describe arity (and a little more)

DataKinds promotes datatypes to kind so can be used in kind signatures.
- enables type-safety at type-level

> data Nat = Z | S Nat deriving Show

more type-safety: badSuccD = SuccD 10 -- INVALID

DataKinds
- promotes Nat type to Nat kind
- promotes data-constructors ZeroD and SuccD to types

> data VecD :: * -> Nat -> * where
>   NilD  ::                  VecD a  'Z    -- uses type 'Z
>   ConsD :: a -> VecD a n -> VecD a ('S n) -- uses type 'S

> cons1D = ConsD  3  NilD -- :: VecD Integer ('SuccD 'ZeroD)
> cons2D = ConsD '5' NilD -- :: VecD Char    ('SuccD 'ZeroD)

' to unambiguously specify the promoted type or kind

> instance Show a => Show (VecD a n) where
>   show NilD         = "NilD"
>   show (ConsD l ls) = "(ConsD " ++ show l ++ " " ++ show ls ++ ")"

