> {-# LANGUAGE TypeFamilies #-}
>
> module Part2_type_families where
>
> import Part1_1_MPTC (Z, S)

 https://byorgey.wordpress.com/2010/07/06/typed-type-level-programming-in-haskell-part-ii-type-families/

multi-parameter type classes + functional dependencies
- enable type-level programming in logic programming style

2007 : type families

type families enable writing functions on types


> -- | for any types m and n, Plus m n is type of kind *
> -- it is not a new type
> -- it is an alias for some existing type
> type family   Plus    m  n :: *
> type instance Plus  Z    n  = n
> type instance Plus (S m) n  = S (Plus m n)

difference between type family and type synonyms
- type synonyms are parametric: the don't look at arguments

type synonyms we cannot do pattern matching on arguments, e.g., where Foo acts differently depending on its second argument

  type Foo m Int  =      [m]
  type Foo m Char = Maybe m

type families
- type synonyms that can do pattern-matching on arg
- e.g., Plus : evaluates to different types depending on first arg being Z or S n

type family and instances above are ~ identical to impl of value-level natural numbers
-  pattern-matching on the first argument and a recursive call in the successor case:

> data Nat = Z | S Nat

> plus :: Nat -> Nat -> Nat
> plus  Z    n =           n
> plus (S m) n = S (plus m n)


ghci does not expand type family:

  :t undefined :: Plus (S Z) (S Z)
  =>                               :: Plus (S Z) (S Z)


but can force it via error message:

  undefined :: Plus (S Z) (S Z)

  ...No instance for (Show (S (S Z)))

type families
- type-level functions in functional style
- cases where logic programming style useful
  - with additional functional dependency can use Plus type class from last post to compute both addition and subtraction

FDs and TFs are equivalent in expressiveness

