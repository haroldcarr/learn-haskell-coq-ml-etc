> {-# LANGUAGE FlexibleInstances      #-} -- TODO: why is this necessary?
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE TypeSynonymInstances   #-} -- TODO: why is this necessary?
>
> module Ch05_12_functional_dependencies where

"functional dependencies" make multiparameter classes useful

Mark Jones, 2000, Type Classes with Functional Dependencies
- http://www.cs.tufts.edu/~nr/cs257/archive/mark-jones/fundeps.ps

gives a way to constrain ambiguity created by multiple type parameters

E.g., can constrain relationship between a and b:

> class Coerce2 a b | b -> a where
>   coerce2 :: a -> b
>
> instance Coerce2 Int String where
>   coerce2 = show
>
> instance Coerce2 Int [Int] where
>   coerce2 x = [x]

relation `b -> a` tells compiler
- if it can infer `b`
- then look up corresponding `a` in type-class instances

> ch05_12_e1 = coerce2 12 :: String

compiler is given `b :: String`
- so find uniquely corresponding `a :: Int`

Also, compiler can now prevent adding conflicting instance declarations:

 -- INVALID
 instance Coerce2 Float String where
    coerce2 = show

 Functional dependencies conflict between instance declarations:
      instance Coerce2 Int String
      instance Coerce2 Float String

`b :: String` could imply either `Int` or `Float`

The functional dependency `b -> a` tells the compiler that `b` uniquely determines `a`
