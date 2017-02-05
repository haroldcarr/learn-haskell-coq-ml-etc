> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module Lib where

 https://byorgey.wordpress.com/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies/

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

functional dependencies enable specifying that some type class parameters are determined by others
— i.e., the relation determined by a multi-parameter type class is a function

m n -> r
- says knowing m and n determines r (i.e., function from m,n to r)
- practice: may only have single instance of Plus for any particular combination of m and n
- if ghc can determine m and n and finds instance matching them, will assume it is only one and pick r to match

> class Plus m n r | m n -> r
> instance Plus Z n n
> instance (Plus m n r) => Plus (S m) n (S r)

now GHC can calculate 1+1=2 at type-level

:t undefined :: (Plus (S Z) (S Z) r) => r
=>                                        :: S (S Z)

MPTC+FD Type-level programming resembles logic programming (e.g., Prolog)
- declare rules defining a number of relations
- "running"" program does search of rules to find solutions for unconstrained variables  in a given relation
- (note: GHC type class instance search does not backtrack)
