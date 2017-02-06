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
> type family   Plus  m    n :: *
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


  :t undefined :: Plus (S Z) (S Z)
  =>                               :: Plus (S Z) (S Z)

ghci does not expand type family

get via error message

  *Main> undefined :: Plus (S Z) (S Z)

  ...No instance for (Show (S (S Z)))...

This is ugly, but it works: S (S Z) is the reduced form of Plus (S Z) (S Z).

So type families let us program in a functional style. This is nice — I daresay most Haskell programmers will be more comfortable only having to use a single coding style for both the value level and the type level. There are a few cases where a logic programming style can be quite convenient (for example, with an additional functional dependency we can use the Plus type class from the last post to compute both addition and subtraction), but in my opinion, the functional style is a huge win in most cases. (And, don’t worry, FDs and TFs are equivalent in expressiveness.)

Of course, there is a lot more to all of this; for example, I haven’t even mentioned data families or associated types. For more, I recommend reading the excellent tutorial by Oleg Kiselyov, Ken Shan, and Simon Peyton Jones, or the page on the GHC wiki. For full technical details, you can look at the System FC paper.

Nothing is ever perfect, though — in my next post, I’ll explain what type families still leave to be desired, and what we’re doing to improve things.

