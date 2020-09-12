> {-# LANGUAGE DataKinds      #-}
> {-# LANGUAGE GADTs          #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies   #-}
>
> module Ch07_07_type_level_programming where
>
> import Ch07_06_type_promotion

Type-level programming
- TypeFamilies               : functions at type-level
- PolyKinds                  : polymorphism to kind-level
  - kind system too restrictive (because it lacks polymorphism)
- DataKinds (type promotion) : datatypes and type-safety to kind-level
  - kind system too permissive (kinds are vague) : Promote datatypes to kinds to simulate type-system at kind-level

Haskell98 carried seed for type-level programming with multiparameter type-classes.
Then functional dependencies, GADTs, TypeFamilies, PolyKinds, DataKinds

type-level fun that computes type-level numbers.

> vappend :: VecD e n -> VecD e m -> VecD e (Add n m)
> vappend NilD         l   = l
> vappend (ConsD x xs) ys  = ConsD x (vappend xs ys)

Add is type level function

> -- | kind signature constrains types in analogous way to regular type signatures constraining terms
> type family Add (n :: Nat) (m :: Nat) :: Nat

Instances use pattern matching on types instead of pattern matching on data-constructors:

> type instance Add  'Z    m = m
> type instance Add ('S n) m = 'S (Add n m)

> -- xs0 :: VecD Integer ('S ('S ('S 'Z)))
> xs0 = vappend (ConsD 3 (ConsD 5 NilD))
>               (ConsD 7 NilD)

Add : type-level arithmetic (executed during type-checking)

-- https://wiki.haskell.org/Type_arithmetic

Promoting term-level programs to type-level

still missing at type-level
- case expressions
- anonymous functions
- partially applied functions
- let expressions

changed in 2014
- Promoting functions to type families in Haskell, by Eisenberg and Stolarek
- added above features using template-programming
  - culminated in singletons library : https://hackage.haskell.org/package/singletons

can write code that operates at both term and type levels

Closed type families
- all instances of type family declared in one place
- brings benefit of type-inference (lost with open type families)

> -- closed-type family
> -- type-level function
> -- pattern matches against types
> type family IsZero (n::Nat) :: Bool where
>   IsZero  'Z    = 'True
>   IsZero ('S n) = 'False


History of type-level programming in Haskell

Year Language extension                                     Advance in Type-level programming
1997 MultiParamTypeClasses                                  Basis for Type functions
2000 FunctionalDependencies                                 Type-level functions (relational-style)
2003 GADTs                                                  Type refinement
2005 TypeFamilies — Associated Types                        Type-level functions, type-class bound
2008 TypeFamilies — top-level and associated types unified  Type-level functions, top level and type-class bound
2012 PolyKinds — Kind polymorphism                          Type-level polymorphism
2012 DataKinds — Type Promotion                             Type-level datatypes, type-safety
2014 TypeFamilies — Closed-type families                    Type families with type-inference
2014 Singletons library                                     Type-level: case, let, lambdas, currying

Type-level and generic programming

Type-level programming can be viewed as pattern of generic programming.
- TL programming useful in implementing datatype generic programming
- some generic programs can be written at type-level instead of term-level

also : template metaprogramming used in type-level programming and datatype generic programming

template metaprogramming, datatype generic programming, and type-level programming
- happen at different levels of abstraction and different phases of program execution

