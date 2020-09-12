> {-# LANGUAGE TypeFamilies #-}
>
> module Ch07_04_type_synonym_families where
>
> import Ch07_03_associated_type_synonyms

-- https://wiki.haskell.org/GHC/Type_families

2008, 3 years after associated types, they are subsumed by broader type families.

Associated types are type families where the type function is attached to a type-class.

type-families are top-level not associated to a type-class

requires TypeFamilies

> type family RepF d
> type instance RepF (List' a) = (RList a)

RepF declares a type function family

Each instance (member of family) declares a specific function.

use with type-class:

> class GenericF d where
>   fromF :: d         -> (RepF d)
>   toF   :: (RepF d)  -> d

> instance GenericF (List' a) where
>   fromF Nil'             = L U
>   fromF (Cons' x xs)     = R (Combo x xs)
>   toF   (L U)            = Nil'
>   toF   (R (Combo x xs)) = (Cons' x xs)

> ch07_04_e1 = fromF (Cons' 1 Nil')

associated types
- need to align type function parameters with type-class parameters

top-level type families
- do not have that restriction
- are more general

fundamental difference
- scope of type function

Analogy:
- type class methods are to regular functions : polymorphism over value
- Type families      are to regular types     : polymorphism over datatypes

type families are open : can add new instances at any time (like type class methods)

key use-cases
- Generic programming
- parameterized libraries

Data families
-------------

type families can be defined for datatypes as well
- enables families of datatypes

datatype families can be associated to type-class or top-level:

> {- TODO
> -- associated data family
> class GMap k where
>   data GMap k :: * -> *
>   empty       :: GMap k v
>   lookup      :: k -> GMap k v -> Maybe v
>   insert      :: k -> v -> GMap k v -> GMap k v
> -}

> -- | top-level data family
> -- generic interface for maps where type of key k determines type of value (via Gmap type function)
> data family GMap k :: * -> *
