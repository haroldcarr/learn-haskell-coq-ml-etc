> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
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

> data EP d r = EP { from_ :: (d -> r)
>                  , to_   :: (r -> d)
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
>     to (R (Combo x xs))  = (Cons' x xs)

> ch07_03_e1 :: RList [Char]
> ch07_03_e1  = from (Cons' "1" Nil')
>
> ch07_03_e2 :: Num a => RList a
> ch07_03_e2  = from (Cons' 1 Nil')

 ch07_03_e3 :: Choice a (Combo a1 (List' a2))
 ch07_03_e3  = to (R (Combo "1" Nil'))

The from function is constrained to functionally-related types d and r. The functional dependency (d -> r) tells the compiler to only accept one r for every d; for example, we can't declare an alternative target representation for (List' a):

  instance GenericFD (List' a) (AltRList a) ...
Multiparameter type-classes became more useful once there was a way to constrain the relationship between the parameters (in the absence of which, type inference is not possible).

Functional Dependencies by Mark Jones, 2000, was the first solution to this problem. They introduced the notion of type function, albeit implicitly, through relations. Type functions, in turn, unleashed a wave of type-level programming in the Haskell community.

In 2002, five years after the introduction of functional dependencies, associated type synonyms were introduced as an alternative way to specify a relationship between multiple type-class parameters as explicit type functions.

Let's rewrite our Generic type-class using the associated types.

Associated type synonyms

The key observation that leads us from functional dependencies to associated type synonyms is that the (GenericFD d r) type-class doesn't really have two parameters, but rather one parameter d, which uniquely determines the other parameter r:

-- {-# LANGUAGE TypeFamilies #-}
class GenericA d where
  type Rep d :: *
  
  fromA :: d        -> (Rep d)
  toA   :: (Rep d)  -> d
The Rep is a type function (or type family, or associated type). In contrast to functional dependencies, the associated type synonym makes the type function explicit.

The fromA and toA are generic functions that are indexed against types that are themselves indexed by types! In this way, associated type synonyms extend type-classes by allowing for type-indexed behavior.

The type-class instance needs to specify a value for the type function Rep, that is, the instance mixes type functions with type-class functions.

instance GenericA (List' a) where
  type Rep (List' a) = (RList a)
  -- Rep type params must match the class params
  
 fromA Nil'                   = L U
 fromA (Cons' x xs)     = R (Combo x xs)
 toA (L U)                    = Nil'
 toA (R (Combo x xs)) = (Cons' x xs)

main = print $ fromA (Cons' 1 Nil')
This is precisely how generics are implemented in the GHC (https://wiki.haskell.org/GHC.Generics). Moreover, GHC.Generics provides automatic instance generation with deriving Generic.

Associated types versus functional dependencies

It turns out that associated types and functional dependencies have similar expressive power. Having said that, associated types have some clear benefits:

Associated types provide explicit type functions contrary to the implicit relations of functional dependencies
Type functions allow us to reduce the number of type parameters
Type functions are more idiomatically functional than relational-style functional dependencies

