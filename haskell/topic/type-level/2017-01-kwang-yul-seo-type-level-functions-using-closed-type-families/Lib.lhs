> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE PolyKinds            #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Lib where
>
> import GHC.TypeLits  -- provides type-level natural numbers and symbols
> import Data.Proxy

a kind is the type of a type constructor

  http://kseo.github.io//posts/2017-01-16-type-level-functions-using-closed-type-families.html

------------------------------------------------------------------------------
Data Literals via DataKinds

:kind Bool            -- Bool is a non-parameterized (i.e., arity 0) type constructor
=> Bool :: *

:type Bool            -- Bool is not a typed value
=> error: Data constructor not in scope: Bool

:kind True            -- True is not a type constructor
=> error: Not in scope: type constructor or class ‘True’

:type True            -- True constructs a Bool value
=> True :: Bool

:set -XDataKinds

:kind True            -- True is a non-parameterized type constructor that constructs a Bool kind
=> 'True :: Bool      -- note the single quote (sometimes necessary to distinguish between type/kind)

:kind 1               -- 1 is a type constructor that constructs a Nat kind
=> 1 :: Nat

:type 1               -- 1 is also a value constructor of Num type
=> 1 :: Num t => t

------------------------------------------------------------------------------
type-level function

> type family If c t e where
>   If 'True  t e = t
>   If 'False t e = e

Takes a 'Bool kind and two more * kinds, returns a * kind -- TODO see Maybe below

:kind If
=> If :: Bool -> * -> * -> *

:kind! If 'True Bool Char
=> If 'True Bool Char :: *
   = Bool

:kind! If 'False Bool Char
=> If 'False Bool Char :: *
   = Char

:kind! If 'True (Maybe Int) Char
=> If 'True (Maybe Int) Char :: *
   = Maybe Int

:kind! If 'True Maybe Char
=> error: Expected kind ‘* -> *’, but ‘Char’ has kind ‘*’

:kind! If 'True Maybe Maybe
=> If 'True Maybe Maybe :: * -> *
   = Maybe

------------------------------------------------------------------------------
type-level list

[] as a kind constructor (instead of type constructor)
[] and (:) as types      (instead of values)

:kind (:)                 -- `a` means it is kind polymorphic
=> (:) :: a -> [a] -> [a]

:kind [True, False]
=> [True, False] :: [Bool]

:kind [1, 2]
=> [1, 2] :: [Nat]

------------------------------------------------------------------------------
type-level list function

> -- | 0 / 1 : types of Nat kind
> -- (+) : type-level fun defined in GHC.TypeLits
> -- (:) : used as pattern
> type family Length xs where
>   Length '[]       = 0
>   Length (x ': xs) = 1 + Length xs

:kind! Length [Char,Bool,Int]
=> Length [Char,Bool,Int] :: Nat
   = 3

:kind Length
=> Length :: [*] -> Nat

:kind Char
=> Char :: *
:kind Bool
=> Bool :: *
:kind Int
=> Int :: *
:kind Nat
=> Nat :: *

not kind-polymorphic by default (comment out PolyKind extension before next):

type family Length (xs :: [k]) where
   Length '[]       = 0
   Length (x ': xs) = 1 + Length xs

:kind! Length [1,2,3]
=> error: Expected kind ‘[*]’, but ‘'[1, 2, 3]’ has kind ‘[Nat]’

:kind [Char,Char,Char]
=> [Char,Char,Char] :: [*]
:kind [1,2,3]
=> [1,2,3] :: [Nat]

with PolyKind:

> -- | with PolyKind explicit kind `k` not necessary - can be inferred (so above Length is equivalent to this one)
> type family LengthPK (xs :: [k]) where
>   LengthPK '[]       = 0
>   LengthPK (x ': xs) = 1 + LengthPK xs

:kind! LengthPK [1,2,3]
=> LengthPK [1,2,3] :: Nat
   = 3

> type family Head (xs :: [*]) where
>   Head (x ': xs) = x

> type family Tail (xs :: [*]) where
>   Tail (x ': xs) = xs

:kind! Head [Char, Bool]
=> Head [Char, Bool] :: *
   = Char

:kind! Head [1,2,3]
=> error: Expected kind ‘[ghc-prim-0.5.0.0:GHC.Types.*]’, but ‘'[1, 2, 3]’ has kind ‘[Nat]’

> type family HeadK (xs :: [k]) where
>   HeadK (x ': xs) = x

> type family TailK (xs :: [k]) where
>   TailK (x ': xs) = xs

:kind! HeadK [Char, Bool]
=> HeadK [Char, Bool] :: *
   = Char

:kind! HeadK [1,2,3]
=> HeadK [1,2,3] :: Nat
   = 1

Head and Tail are partial functions. But:

:kind! Head '[]
=> Head '[] :: ghc-prim-0.5.0.0:GHC.Types.*
   = Head '[]

GHC treats Head '[] as valid type (no type error)
- behavior not intuitive
- see Richard Eisenberg’s "What are type families?" for more details
- https://typesandkinds.wordpress.com/2015/09/09/what-are-type-families/

------------------------------------------------------------------------------
higher-order type-level list functions

> type family Map (f :: * -> *) (xs :: [*]) where
>   Map f      '[]  = '[]
>   Map f (x ': xs) = f x ': Map f xs

> type family MakePair (x :: *) where
>   MakePair x = (x, x)

:kind! Map MakePair [Char,Bool,Int]
=> Map MakePair [Char,Bool,Int] :: [ghc-prim-0.5.0.0:GHC.Types.*]
   = '[(Char, Char), (Bool, Bool), (Int, Int)]

