> {-# LANGUAGE PolyKinds #-}
>
> module Ch07_05_kind_polymorphism where

Kind polymorphism

Type families : type-level functions

Polykinds : polymorphism to kinds

ref: Giving Haskell a Promotion, Yorgey et al 2012

Enables more generic data and functions.

E.g., type-class that handles various kind-orders:

> {-
> class Typeable (a :: * ) where
>   typeOf :: a -> TypeRep
>
> class Typeable1 (a :: * -> *) where
>   typeOf1 :: forall b. a b -> TypeRep
> -}

(similar to Generic type-class earlier: requires different type-class for each kind arities.

To explore kind polymorphism, make simple version of Typeable
- where `typeOf` returns a string (instead of TypeRep function)

> class T0 a where
>   f0 :: a -> String
>
> instance T0 Int where
>   f0 _ = "T0 Int"
>
> instance T0 Char where
>   f0 _ = "T0 Char"

> ch07_05_e1 = f0 (10::Int) == "T0 Int"
> ch07_05_e2 = f0 'x'       == "T0 Char"

instances with higher-kinded types:

> instance T0 (Maybe a) where
>   f0 _  = "T0 Maybe a"

> ch07_05_e3 = f0 Nothing   == "T0 Maybe a"

But must specify parameter `a` for `Maybe :: * -> *`
- to match required kind * of T0

To create "instance T0 Maybe"
- create alternative type-class

> class T1 m where -- m :: * -> *
>   f1 :: Show a => m a -> String
>
> instance T1 Maybe where
>   f1 _ = "T1 Maybe"

> ch07_05_e4 = f1 (Just 1) == "T1 Maybe"

but can't do:
--  instance T1 Int where
--    f1 _ = "T1 Int"

Polymorphic kinds enables unifying these type-classes into one.

Extension: PolyKinds

> class T a where -- (a::k) : Type variable a has kind variable k.
>   f :: Proxy a -> String

With PolyKinds : k is polymorphic
- k is a polymorphic placeholder for possible kind arities
- *
- * -> *
- * -> * -> *

Proxy variable is a kind-polymorphic phantom-type:

> data Proxy a = Proxy -- (a::k)
>   deriving Show

'a' has polymorphic kind
- (Proxy Int)    -- Proxy :: *        -> *
- (Proxy Maybe)  -- Proxy :: (* -> *) -> *

Proxy variable used to generalize kind of function argument:

 f :: T a => Proxy a -> String -- types
 --    k  =>   k     -> *      -- kinds

1st arg of f can take a type with any kind-order (arity).

Note: type a in Proxy a is constrained by type-class T a, which is also kind polymorphic in type a.

kinds before/after PolyKinds language extension (e.g., ghci> :k f)
  -- before
  f     :: *           -> *
  -- after
  f     :: forall k. k -> *

  -- before
  Proxy :: *           -> *
  -- after
  Proxy :: forall k. k -> *

1st arg of f is kind-polymorphic (via kind-polymorphic phantom type Proxy)

forall in type-level similar to use in RankN types (to describe nested function polymorphism).

verify type parameter has polymorphic kind:

> instance T Int   where -- Int :: *
>   f _ = "T Int"
>
> instance T Maybe where -- Maybe :: * -> *
>   f _  = "T Maybe"

  -- f (Proxy :: Proxy Maybe) -- "T Maybe"
  -- f (Proxy :: Proxy Int)   -- "T Int"

Type of Proxy passed to f determines type-class where function will be invoked.

In this example, kind-polymorphism appears in three times:
- T is a kind-polymorphic type-class
- Proxy datatype is kind-polymorphic (takes types of any kind-order, returns *)
- Proxy data-constructor is kind-polymorphic function

Regular polymorphism over functions and types increases opportunities for abstraction.
Same for kind polymorphism at type level.
