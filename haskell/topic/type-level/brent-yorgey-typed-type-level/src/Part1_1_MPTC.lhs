> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
>
> module Part1_1_MPTC where

 https://byorgey.wordpress.com/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies/
 https://www.reddit.com/r/haskell/comments/ck459/typed_typelevel_programming_in_haskell_part_i/

Haskell initially kept values and types separated.

1997 : multi-parameter type classes, then functional dependencies.

normal type classes represent predicates on types
- a type is an instance of a type class or not

multi-parameter type classes represent relations on types

Uninhabited types to represent natural numbers:

> data Z
> data S n

multi-parameter type class that encodes addition relation on natural numbers:

> -- | for any types m, n, and r
> class PlusMPTC m n r

> -- | (Z,n,n) are in the Plus relation
> instance PlusMPTC Z n n
> -- | If (m,n,r) are in the Plus relation then so are (S m, n, S r)
> instance (PlusMPTC m n r) => PlusMPTC (S m) n (S r)

GHC does not simplify (i.e., calculate) the following since  type classes are open
- there could be many instances of the form PlusMPTC (S Z) (S Z) r for many different types r
- ghci can't pick

:set -XFlexibleContexts
:t undefined :: (PlusMPTC (S Z) (S Z) r) => r
=>                                            :: PlusMPTC (S Z) (S Z) r => r
