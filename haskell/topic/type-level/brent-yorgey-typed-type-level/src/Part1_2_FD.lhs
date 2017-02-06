> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module Part1_2_FD where
>
> import Part1_1_MPTC (Z, S)

functional dependencies enable specifying that some type class parameters are determined by others
— i.e., the relation determined by a multi-parameter type class is a function

m n -> r
- says knowing m and n determines r (i.e., function from m,n to r)
- practice: may only have single instance of Plus for any particular combination of m and n
- if ghc can determine m and n and finds instance matching them, will assume it is only one and pick r to match

> -- | represents three-place addition *relation*
> -- e.g., (3,5,8) are in the relation, because 3 + 5 = 8.
> -- If there is an instance Plus m n r, then m + n = r.
> class     Plus m n r | m n -> r
> instance  Plus Z n n
> instance (Plus m n r) => Plus (S m) n (S r)

now GHC can calculate 1+1=2 at type-level

:t undefined :: (Plus (S Z) (S Z) r) => r
=>                                        :: S (S Z)

MPTC+FD Type-level programming resembles logic programming (e.g., Prolog)
- declare rules defining a number of relations
- "running"" program does search of rules to find solutions for unconstrained variables  in a given relation
- (note: GHC type class instance search does not backtrack)
