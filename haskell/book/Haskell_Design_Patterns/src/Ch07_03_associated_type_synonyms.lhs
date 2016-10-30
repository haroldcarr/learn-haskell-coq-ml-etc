> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE TypeFamilies           #-}
>
> module Ch07_03_associated_type_synonyms where

Associated type synonyms

unifying example (from Chapter 6: Patterns of Generic Programming)

List' object and its type representation RList:

> data List' a
>   = Nil' | Cons' a (List' a)
>   deriving (Show)
>
> data U = U
>   deriving (Show)
>
> data Choice a b = L a | R b
>   deriving (Show)
>
> data Combo a b = Combo a b
>   deriving (Show)
>
> type RList a = Choice U (Combo a (List' a))

functions fromL and toL to convert between the type and representation:

> fromL :: List' a -> RList a
> fromL  = undefined
> toL   :: RList a -> List' a
> toL    = undefined

We embedded this in the container type EP as follows:

> data EP d r = EP { from_ :: d -> r
>                  , to_   :: r -> d
>                  }

Using functional dependencies

Instead of container type EP, can use multiparameter type-class with functional dependencies:

-- requires FlexibleInstances, FunctionalDependencies

> class GenericFD d r | d -> r where
>     from :: d -> r
>     to   :: r -> d
>
> instance GenericFD (List' a) (RList a) where
>  -- from :: GenericFD d r => d -> r
>     from Nil'            = L U
>     from (Cons' x xs)    = R (Combo x xs)
>  -- to :: GenericFD d r => r -> d
>     to (L U)             = Nil'
>     to (R (Combo x xs))  = Cons' x xs

> -- ch07_03_e1 :: RList [Char]
> ch07_03_e1  = from (Cons' "1" Nil')
>
> -- ch07_03_e2 :: Num a => RList a
> ch07_03_e2  = from (Cons' 1 Nil')
>
> -- ch07_03_e3 :: RList Integer
> ch07_03_e3  = from (Cons' 1 (Cons' 2 Nil'))
>
> -- this type signature necessary to compile
> ch07_03_e4 :: GenericFD (List' Integer) (RList Integer) => (List' Integer)
> ch07_03_e4  = to ch07_03_e3

`from` constrained to functionally-related types d and r.
- functional dependency `d -> r` tells compiler to only accept one `r` for every `d`
- e.g., can not declare an alternative target representation for (List' a):

  instance GenericFD (List' a) (AltRList a) ...

Multiparameter type-classes more useful when constraining relationship between parameters
- otherwise type inference not possible

Functional Dependencies by Mark Jones, 2000
- first solution
- introduced notion of type function (implicitly, through relations)
- type functions enabled more type-level programming in Haskell

2002 (five years after functional dependencies) ASSOCIATED TYPE SYNONYMS introduced
- alternative way to specify a relationship between multiple type-class parameters as explicit type functions

Associated type synonyms

key observation
- `GenericFD d r` type-class does not really have two parameters
- it has one: `d``, that uniquely determines `r`

-- requires TypeFamilies

> class GenericA d where
>     type Rep d :: *
>
>     fromA ::     d -> Rep d
>     toA   :: Rep d ->     d

`Rep` is a type function (aka "type family", "associated type").
- In contrast to functional dependencies, the associated type synonym makes the type function explicit.

`fromA` and `toA`
- generic functions
- indexed against types that are themselves indexed by types
- type families extend type-classes by enabling type-indexed behavior

instance must specify a value for the type function `Rep`
- i.e., instance mixes type functions with type-class functions

> instance GenericA (List' a) where
>     -- Rep type params must match the class params
>     type  Rep (List' a)    = RList a
>
>     fromA Nil'             = L U
>     fromA (Cons' x xs)     = R (Combo x xs)
>     toA   (L U)            = Nil'
>     toA   (R (Combo x xs)) = Cons' x xs

> ch07_03_e5 = fromA (Cons' 1 Nil')
>
> -- needs signature to compile
> -- gets error if try to show
> ch07_03_e6 :: (Rep d ~ Choice U (Combo Integer (List' Integer)), GenericA d) => d
> ch07_03_e6  = toA ch07_03_e5

This is how generics are implemented in the GHC
- https://wiki.haskell.org/GHC.Generics
- GHC.Generics provides automatic instance generation with deriving Generic.

Associated types versus functional dependencies

associated types and functional dependencies have similar expressive power

associated types have clear benefits:

- Associated types provide explicit type functions contrary to the implicit relations of functional dependencies
- Type functions enable reducing number of type parameters
- Type functions are more idiomatically functional than relational-style functional dependencies

