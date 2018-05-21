> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
>
> {-# LANGUAGE ConstraintKinds           #-}
> {-# LANGUAGE DataKinds                 #-}
> {-# LANGUAGE FlexibleInstances         #-}
> {-# LANGUAGE GADTs                     #-}
> {-# LANGUAGE KindSignatures            #-}
> {-# LANGUAGE MultiParamTypeClasses     #-}
> {-# LANGUAGE PolyKinds                 #-}
> {-# LANGUAGE RankNTypes                #-}
> {-# LANGUAGE ScopedTypeVariables       #-}
> {-# LANGUAGE StandaloneDeriving        #-}
> {-# LANGUAGE TypeFamilies              #-}
> {-# LANGUAGE TypeOperators             #-}
> {-# LANGUAGE UndecidableInstances      #-}
> {-# LANGUAGE UndecidableSuperClasses   #-}
>
> module HC where
>
> import           Data.Char             (digitToInt)
> import           GHC.Exts              (Constraint)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, e)

CUFP 2015
http://staff.mmcs.sfedu.ru/~ulysses/Edu/SSGEP/loh/loh-repo/Lecture1.pdf

Feb 2018
https://github.com/kosmikus/SSGEP/
https://github.com/kosmikus/SSGEP/raw/master/LectureNotes.pdf

------------------------------------------------------------------------------
Type-level programming in Haskell.

outline
- normal list
- length-indexed vectors
- heterogeneous lists
- n-ary products (aka "Environments")

------------------------------------------------------------------------------
-- p7  1.2 Kinds and data kinds

layered type system
- kinds : type of types
- types : type of terms

-- p8

kinds
- '*' : types that classify terms (can be uninhabited)
    - fully applied `data` constructor is of kind `*`
    - `Int`, `Double`, ...
    - `Maybe Int`, `[Double]`, ...
- `k -> l` : type of kind `l` parameterized by type of kind `k`
    - `Maybe`, `[]`, `IO`, ...
    - `Either`, `(,)`   : `* -> * -> *`
    - `Either Int`      : `* -> *`
    - `Either Int char` : `*`

-- p8 1.2.2 Promoted data kinds

data type promotion

define new data type:

> data XBool = XFalse | XTrue

- `XBool`
    - new *type*  `XBool` with *term* constructors  `XFalse`,  `XTrue`
    - new *kind* `'XBool` with *type* constructors `'XFalse`, `'XTrue` (promotion via DataKinds)

All kinds from promoted datatypes are uninhabited (e.g., there are *no* terms/values of type `'XFalse`).

kinds from promoted datatypes, although uninhabited, may appear as
- args/results of type-level functions
- parameters of datatypes and classes (most used in this paper)

------------------------------------------------------------------------------
-- p9 1.3 GADTs

-- p9 1.3.1 Vectors

length-indexed lists (aka vectors)

> data Nat = Zero | Suc Nat

used in promoted form:

~~~{.haskell}
:set -XDataKinds
:k 'Zero
--   ... :: Nat
:k  'Suc
--   ... :: Nat -> Nat
:t  Zero
     ... :: Nat
:t   Suc
     ... :: Nat -> Nat
~~~

GADT enables restricting
- `VNil`  to `Zero
- `VCons` to non-zero

> --      KindSignatures DataKinds GADT
> --          v        v   v       v
> data Vec (a :: *) (n :: Nat) where
>                 -- DataKinds
>                 -- v
>     VNil  ::                 Vec a  'Zero
>     VCons :: a -> Vec a n -> Vec a ('Suc n)
> infixr 5 `VCons`
>
> -- StandaloneDeriving
> deriving instance Eq   a => Eq   (Vec a n)
> deriving instance Show a => Show (Vec a n)

`Nat` used as a type "index" (it is not inhabited) into family of types.
Used to specify more info about vectors.

> type Two   =      'Suc ('Suc 'Zero)
> type Three = 'Suc Two

> vbc  :: Vec Char Two
> vbc   = 'b' `VCons` 'c' `VCons` VNil
> vabc :: Vec Char Three
> vabc  = 'a' `VCons` vbc

-- p10

Power of GADT apparent in pattern matching.  Type-safe/total:

> vtail :: Vec a ('Suc n) -> Vec a n
> vtail (VCons _ xs) = xs

Do not need `VNil` case.  Impossible and guaranteed by type checking:

~~~{.haskell}
vtail VNil = VNil

Couldn't match type ‘'Suc n’ with ‘'Zero’
Inaccessible code in
  a pattern with constructor
    VNil :: forall a. Vec a 'Zero,
  in an equation for ‘vtail’
Relevant bindings include
  vtail :: Vec a ('Suc n) -> Vec a n
In the pattern: VNil
In an equation for ‘vtail’: vtail VNil = VNil
~~~

In `vmap`, matching
- `VNil`  means `n ~ 'Zero`
- 'VCons' means `n ~ 'Suc n'` for some `n'`

> vmap :: (a -> b) -> Vec a n -> Vec b n
> vmap _ VNil = VNil
> vmap f (VCons x xs) = f x `VCons` vmap f xs

Given
- `VNil case "returns" `'Zero`
- recursive call in `VCons` case "returns" `n'`
- `VCons`ing element onto recursive result returns 'Suc n'`
- therefore `vmap` preserves length

-- X BEGIN

Use the type system to avoid:

> -- no explicit [] case, so will get runtime error
> xsum :: [Int] -> Int
> xsum xs = head xs + xsum (tail xs)
> txsum = U.e "txsum" (xsum [1,2,3]) "Prelude.head: empty list"

via:

> -- does not need Nil case because of `('Suc n)`
> vhead :: Vec a ('Suc n) -> a
> vhead (VCons x _) = x

~~~{.haskell}
vsum :: Vec Int ('Suc n) -> Int
vsum xs = vhead xs + vsum (vtail xs)

Couldn't match type ‘n’ with ‘'Suc n0’
  ‘n’ is a rigid type variable bound by
      the type signature for vsum :: Vec Int ('Suc n) -> Int
Expected type: Vec Int ('Suc ('Suc n0))
  Actual type: Vec Int ('Suc n)
Relevant bindings include
  xs :: Vec Int ('Suc n)
  vsum :: Vec Int ('Suc n) -> Int
In the first argument of ‘vtail’, namely ‘xs’
~~~

-- X END

-- p10 1.3.2 Functions on vectors

Motivation: move more info to type level.

~~~{.haskell}
-- term-level replicate
replicate :: Int -> a -> [a]
replicate n x
    | n <= 0    = []
    | otherwise = x : replicate (n - 1) x
~~~

vector size known at type-level.
Assume number of copies to `replicate` is also known at type-level.
How is that number passed?

Cannot write

~~~{.haskell}
vreplicate :: a -> Vec a n
~~~

because that type says the function is parametric in n
- i.e., that no need to look at n at all

But a compile-time distinction based on the type of n if necessary.

-- p11

Option: use a type class (they work with data kinds):

> class VReplicateTC (n :: Nat) where
>     vreplicateC :: a -> Vec a n
>
> instance VReplicateTC 'Zero where
>     vreplicateC _ = VNil
>
> instance VReplicateTC n => VReplicateTC ('Suc n) where
>     vreplicateC x = x `VCons` vreplicateC x

> v3 :: Vec Char Three
> v3  = vreplicateC 'x'
> tv3 = U.t "tv3" v3 (VCons 'x' (VCons 'x' (VCons 'x' VNil)))

PROS:
- compiletime resolution of type-level values

CONS:
- have to define new type classes for each function
- class constraints appear in types of all functions that use them
- means: leak implementation details
- means: changing signature percolates throughout code
- might produce lots of code

------------------------------------------------------------------------------
-- p11 1.4 Singleton types

Another option: create value to let GHC know value of `n`
- create a GADT `SNat n` such that it has exactly one value for each `n`
- pattern match on `SNat n` at runtime to use `n`

One way:

> data SNatX (n :: Nat) where
>     SZeroX ::            SNatX  'Zero
>     SSucX  :: SNatX n -> SNatX ('Suc n)
>
> deriving instance Show (SNatX n)

> sThreeX :: SNatX Three
> sThreeX  = SSucX (SSucX (SSucX SZeroX))

-- p12

Another way: mutually-recursive (used in remainder of paper):

> data SNat (n :: Nat) where
>     SZero ::            SNat  'Zero
>     SSuc  :: SNatI n => SNat ('Suc n)
>
> class SNatI (n :: Nat) where
>     sNat  :: SNat n
>
> instance SNatI 'Zero where
>     sNat   = SZero
>
> instance SNatI n => SNatI ('Suc n) where
>     sNat   = SSuc

> --                      RankNTypes (or ExistentialQuantification, ...)
> --                       v
> vreplicate :: forall a n . SNatI n => a -> Vec a n
> --                  ScopedTypeVariables
> --                               v
> vreplicate x = case sNat :: SNat n of -- choice of sNat to run at runtime is made via type at compiletime
>     SZero -> VNil
>     SSuc  -> x `VCons` vreplicate x

> vr3 :: Vec Char Three
> vr3  = vreplicate 'x'
> tvr3 = U.t "tvr3" vr3 (VCons 'x' (VCons 'x' (VCons 'x' VNil)))

-- p12 1.4.2

PROS of Singletons (compared to one class per function)

- only need one class per type used in pattern matching
    - do not need that if singleton passed explicitly
    - more interface stability; exposes less of impl

CONS
- type class resolution happens at compile time
- value of `SNat` is runtime value used in pattern matching at runtime
- so, unnecessary case distinctions at runtime since value know at compiletime
- usually produce less code

-- p12 1.4.2 Applicative vectors

vreplicate can be used to create the equivalent of the applicative interface

~~~{.haskell}
class Functor f => Applicative f where
    pure  :: a -> fa
    (<*>) :: f (a -> b) -> f a -> f b
~~~

-- p13

one possible applicative instance for lists is
- pure  : produces infinite list of copies
- (<*>) : zips list, applying list of functions pointwise to list of args.

 https://en.wikibooks.org/wiki/Haskell/Applicative_functors

~~~{.haskell}
newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
    -- infinite list
    pure x                        = ZipList (repeat x)

import Control.Applicative
ZipList [(2*),(5*),(9*)] <*> ZipList [1,4,7]
-- ZipList {getZipList = [2,20,63]}

       (,,) <$> ZipList [1,4,9] <*> ZipList [2,8,1] <*> ZipList [0,0,9]
-- ZipList {getZipList = [(1,2,0),(4,8,0),(9,1,9)]}

liftA3 (,,)    (ZipList [1,4,9])   (ZipList [2,8,1])   (ZipList [0,0,9])
-- ZipList {getZipList = [(1,2,0),(4,8,0),(9,1,9)]}
~~~

implement same idea for vectors
- length of lists known at compiletime
- `vreplicate` is `pure`
- `vapply` is `<*>`

> vapply :: Vec (a -> b) n -> Vec a n -> Vec b n
> vapply           VNil           VNil = VNil
> vapply (f `VCons` fs) (x `VCons` xs) = f x `VCons` vapply fs xs

> va :: Vec Integer ('Suc ('Suc ('Suc 'Zero)))
> va  = ((2*) `VCons` (5*) `VCons` (9*) `VCons` VNil) `vapply`
>       ( 1   `VCons`  4   `VCons`  7   `VCons` VNil)
> tva = U.t "tva" va
>       ( 2   `VCons` 20   `VCons` 63   `VCons` VNil)

Note: `Vec` cannot be made `Applicative` instance because its parameters are in the wrong order.

-- X BEGIN

> vff :: Vec (Char -> Int) Two
> vff  = digitToInt `VCons` digitToInt `VCons` VNil

> vfff :: Vec (Char -> Int) Three
> vfff  = digitToInt `VCons` vff

> tvfffvabc = U.t "tvfffvabc"
>     (vapply vfff vabc)
>     (VCons 10 (VCons 11 (VCons 12 VNil)))

~~~{.haskell}
vapply vff vabc

    Couldn't match type ‘'Suc 'Zero’ with ‘'Zero’
    Expected type: Vec Char Two
      Actual type: Vec Char Three
    In the second argument of ‘vapply’, namely ‘vabc’

vapply vfff vbc

    Couldn't match type ‘'Zero’ with ‘'Suc 'Zero’
    Expected type: Vec Char Three
      Actual type: Vec Char Two
    In the second argument of ‘vapply’, namely ‘vbc’
~~~

-- X END

------------------------------------------------------------------------------
-- p13 1.5 Heterogeneous lists

Like `Vec` but also each element can have a different (known) type.

-- p13 15.1.1 Promoted lists and kind polymorphism

Can use
-  `[]`         as kind constructor
- `'[]`, `(':)` as types
    - *kind-polymorphic* : `a` can be any kind
    - `(':) :: a -> [a] -> [a]`

Type-level list of promoted `Bool`, `Nat`, `[*]`

~~~{.haskell}
:set -XDataKinds
:kind ['True, 'False]
--                  ... :: [Bool]

:kind ['Zero, Three]
--                  ... :: [Nat]

:kind [Char, Bool, Int]
--                  ... :: [*]
~~~

Need kind `[*]` for heterogeneous lists.

-- p14

> -- TODO : when/where are values, length and types handled?
> data HList (xs :: [*]) where
>     HNil  :: HList '[]
>     HCons :: x -> HList xs -> HList (x ': xs)
> --                                     ^
> --                                     TypeOperators
> infixr 5 `HCons`

> hhead :: HList (x ': xs) -> x
> hhead (x `HCons`  _) = x

> htail :: HList (x ': xs) -> HList xs
> htail (_ `HCons` xs) = xs

example: represent

> data Group = Group Char Bool Int

as an `HList`:

> -- sig can be inferred
> group :: HList '[Char, Bool, Int]
> group  = 'x' `HCons` False `HCons` 3 `HCons` HNil

~~~{.haskell}
bh :: Int
bh = hhead group

Couldn't match type ‘Char’ with ‘Int’
    Expected type: HList '[Int, Bool, Int]
      Actual type: HList '[Char, Bool, Int]
    In the first argument of ‘hhead’, namely ‘group’
~~~

-- p14 1.5.2 n-ary products (aka "Environments")

`NP` means "n-ary product"

`HList` variant
- also abstracted over type constructor f
    - each element determined by applying a type constructor to types in the index list
    - e.g., `Maybe`, `IO`
- has elements of type f x
    - where x is a member of the index list

> --            PolyKinds
> --            v
> data NP (f :: k -> *) (xs :: [k]) where
>     Nil  ::                   NP f      '[]
>     (:*) :: f x -> NP f xs -> NP f (x ': xs)
>
> infixr 5 :*

(see end of file for Eq, Show definitions)

- list `xs` is signature
- kind polymorphic
    - not required to be `[*]`
    - is arbitrary type-level list of kind `[k]`
        - as long as `f` maps `k` to `*`
    - possible because elements of signature do not appear in environment
        - they appear as arg to `f` instead

case where `k` is `*`: shows that NP is generalization of `HList`

> -- identity function on types
> newtype I a = I {unI :: a} deriving (Eq, Read, Show)

`NP I` isomorphic to `HList`:

-- p15

> fromHList :: HList xs  -> NP I  xs
> fromHList           HNil =  Nil
> fromHList (x `HCons` xs) = I x :*      fromHList xs

> toHList   :: NP I  xs  -> HList xs
> toHList              Nil = HNil
> toHList (I x :*      xs) =   x `HCons` toHList xs

No constraints on `xs` so no need for type class.

> groupNPI :: NP I '[Char, Bool, Int]
> groupNPI  = fromHList group
> gnpi = U.t "gnpi" groupNPI (I {unI = 'x'} :* I {unI = False} :* I {unI = 3} :* Nil)
>
> groupHL :: HList '[Char, Bool, Int]
> groupHL  = toHList groupNPI
> ghl = U.t "ghl" groupHL ('x' `HCons` False `HCons` 3 `HCons` HNil)

> groupNPM :: NP Maybe '[Char, Bool, Int]
> groupNPM  = Just 'x' :* Just False :* Just (3::Int) :* Nil

> j3 :: Num x => NP Maybe '[x]
> j3  = Just 3 :* Nil

`NP` also a generalization of homogeneous vectors:

> -- constant function on types
> -- for any types a b, K a b isomorphic to a
> newtype K a b = K {unK :: a} deriving (Eq, Read, Show)

~~~{.haskell}
NP (K Double) ’[Char, Bool, Int]
~~~
is a list of three elements of type Double
- length of type-level list determines number of elements
- the types in the index list are irrelevant
    - they are ignored by K

> -- useful: NP of K into normal list
> hcollapse :: NP (K a) xs -> [a]
> hcollapse         Nil = []
> hcollapse (K x :* xs) = x : hcollapse xs

> groupNPK :: NP (K Char) '[Char, Bool, Int]
> groupNPK  = K 'x' :* K 'y' :* K 'z' :* Nil
>
> gchar = U.t "gchar" (hcollapse groupNPK) "xyz"

> k2 :: K Integer b
> k2  = K 2
> groupK2 = k2 :* k2 :* k2 :* Nil
> g2 = U.t "g2" (hcollapse groupK2) [2,2,2]

> xx :: NP ((->) a) '[K a b]
> xx  = K :* Nil

> xxx :: NP ((->) a) '[K a b1, K a b2]
> xxx  = K :* K :* Nil

------------------------------------------------------------------------------
-- p15 1.6 Higher-rank types

Define functions on `NP`

For vectors above
- implemented vmap, vreplicate and vapply
- gave Applicative-like capabilities for vectors

Goal here is to provide a similar interface for NP.

-- p15 1.6.1 map

~~~{.haskell}
vmap :: (a -> b) -> Vec a n -> Vec b n
~~~
- preserves length
- changes type

NP
- no single type of elements
- goal: *predictably* change types
- type constructor `f` says how to interpret each element

-- p16

Generalize `vmap` to `hmap`

In HList to NP conversion, have a type constructor `f` that says how to interpret each element in signature.
So,

`hmap`
- preserve index
- change type constructor

use the flexibility of `f`
- can plug any type constructors into an NP (including GADTs)
- can express "rather strange maps"---just need to explain them to the type system

~~~{.haskell}
-- type preserving map
NP I xs -> NP I xs

-- type-unifying map
MP I xs -> NP (K a) xs

-- produces values of different types from homogeneous list
NP (K a) xs -> NP I xs
~~~

polymorphic Haskell function:

- caller free to choose
- callee make no assumptions

if arg function is polymorphic:
- caller *must* pass in polymorphic function
- callee may flexibly use it on any type

need `f x -> g x` :  for all `x` types in `xs`

> -- The f arg to `hmap` must be polymorphic
> --                  Rank2Types
> --                  v
> hmap :: (forall x . f x -> g x) -> NP f xs -> NP g xs
> hmap _      Nil  = Nil
> hmap m (x :* xs) = m x :* hmap m xs

Example

> groupI :: NP I '[Char, Bool, Integer]
> groupI = I 'x' :* I False :* I 3 :* Nil

-- p17

> example :: NP Maybe '[Char, Bool, Integer]
> example = hmap (Just . unI) groupI
> exj = U.t "exj" example (Just 'x' :* Just False :* Just 3 :* Nil)

-- p17 1.6.2 Applicative n-ary products

define functions corresponding to pure and <*>

~~~{.haskell}
vreplicate :: forall a n . SNatI n => a -> Vec a n
vreplicate x = case sNat :: SNat n of
    SZero -> VNil
    SSuc  -> x `VCons` vreplicate x
~~~

applicative for `NP` compared to `vreplicate`:
- role of `n` is now the signature `xs`
- role of 'a' is now the type constructor `f`
    - `f` must accept any type in the signature `xs`

need a singleton for lists (following same pattern of `SNat`):

> data SList (xs :: [k]) where
>     SNil  :: SList '[]
>     SCons :: SListI xs => SList (x ': xs)
>
> class SListI (xs :: [k]) where
>     -- | Get the explicit singleton --- the one can then pattern match on
>     sList :: SList xs
>
> instance SListI '[] where
>     sList = SNil
>
> instance SListI xs => SListI (x ': xs) where
>     sList = SCons

above slist def not ideal:
- says nothing about elements of the list
- would like `SCons` to include singleton for head and singleton for tail
- but fine for this library

need polymorphic value of type `f x` so it can be used for any x in the signature of xs

> hpure :: forall f xs . SListI xs => (forall a . f a) -> NP f xs
> hpure x = case sList :: SList xs of
>     SNil  -> Nil
>     SCons -> x :* hpure x

-- p18

~~~{.haskell}
:t hpure Nothing
-- hpure Nothing :: SListI xs => NP Maybe xs

:t hpure (K 0)
-- hpure (K 0) :: (Num a, SListI xs) => NP (K a) xs
~~~

> hpn = U.t "hpn" (hpure Nothing :: NP Maybe '[Char, Bool, Int])
>                 (Nothing :* Nothing :* Nothing :* Nil)
>
> hpk = U.t "hpk" (hpure (K 0) :: NP (K Int) '[Char, Bool, Int])
>                 (K {unK = 0} :* K {unK = 0} :* K {unK = 0} :* Nil)

-- p18 1.6.3 Lifted functions

`hap` : analogous to `vapply` : similar to `(<*>)`

need ype-level lambda `\x -> (f x -> g x)`
- but Haskell has no type-level lambda, so:

> newtype (f -.-> g) a = Fn {apFn :: f a -> g a}
> infix 1 -.->

then use to represent type-level function in:

> hap :: NP (f -.-> g) xs -> NP f xs -> NP g xs
> hap      Nil       Nil  = Nil
> hap (f :* fs) (x :* xs) = apFn f x :* hap fs xs

examples

> lists :: NP [] '[String, Int]
> lists = ["foo", "bar", "baz"] :* [1 .. ] :* Nil
>
> numbers :: NP (K Int) '[String, Int]
> numbers = K 2 :* K 5 :* Nil
>
> {-# ANN fn_2 "HLint: ignore Avoid lambda" #-}
> fn_2 :: (f a -> f' a -> f''  a)
>      -> (f -.-> (f' -.-> f'')) a
> fn_2 f = Fn (\x -> Fn (\y -> f x y))
>
> take' :: (K Int -.-> ([] -.-> [])) a
> take' = fn_2 (\(K n) xs -> take n xs)
>
> -- the Ints from numbers are used a the `(K n)` in take'
> -- the first  number, 2, takes 2 from the          list of String
> -- the second number, 5, takes 5 from the infinite list on Int
> hpt = U.t "hpt" (hpure take' `hap` numbers `hap` lists)
>                 (["foo","bar"] :* [1,2,3,4,5] :* Nil)

-- p19 1.6.4 Another look at `hmap`

for normal lists (assuming `ZipList` applicative) the following "identity" property is true:

~~~{.haskell}
map f = pure f <*> xs
~~~

also holds for n-ary products:

> hmap' :: SListI xs => (forall a . f a -> g a) -> NP f xs -> NP g xs
> hmap' f xs = hpure (Fn f) `hap` xs

> hmi = U.t "hmi'" (hmap  (Just . unI) groupI)
>                  (hmap' (Just . unI) groupI)

------------------------------------------------------------------------------
-- p 19 1.7 Abstracting from class and type functions

Cannot do `NP I -> NP (K String)` because

~~~{.haskell}
hmap (K . show . unI) group'
-- because no match due to the class constraint on `Show`
:t K . show . unI
--            ... :: Show a => I a -> K String b
                               f x -> g        x -- type of fun arg to hmap
~~~

It is useful to enable this to be able to use other functions with other class constraints
to map over an NP, provided that all elements in NP are instances of the class.

-- p19 1.7.1 The kind `Constraint`

Classes can be seen as types with a different kind.

- `data` produces types of kind `*`
- class  produces types of kind `Constraint`

Examples

- `Show`, `Eq`, `Ord`        have kind            `*  -> Constraint`
- `Functor`, `Monad`         have kind      `(* -> *) -> Constraint`
- `MonadReader` (multiparam) has  kind `* -> (* -> *) -> Constraint`

-- p20

Use tuple syntax to create empty constraints

> --                         GHC.Exts (Constraint)
> --                         ConstraintKinds
> --                         v
> type NoConstraint = (() :: Constraint)

or to combine constraints

> type SomeConstraints a   = (Eq a, Show a)
> type MoreConstraints f a = (Monad f, SomeConstraints a)

-- p20 1.7.2 Type functions

To write `hmap` variant compatible with constrained functions,
need to express that a constraint holds for all elements of a type-level list.

Use *type family* : a function on types.

Combine parameterized constraint and list of parameters into a single constraint.

> --   TypeFamilies
> --   v
> type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
>     All c      '[]  = ()
>     All c (x ': xs) = (c x, All c xs)

To see `All` in action, "expand" type families:

~~~{.haskell}
:kind! All Eq '[Int, Bool]
--                     ... :: Constraint = (Eq Int, (Eq Bool, (() :: Constraint)))
~~~

Note: the above looks nested, but is really a flat union.

> hToString :: All Show xs => HList xs -> String
> hToString        HNil  = ""
> hToString (HCons x xs) = show x ++ hToString xs
>
> htos = U.t "htos" (hToString group) "'x'False3"

Pattern matching on `HCons` says

- `xs ~ y ': ys`
- so: `All Show xs ~ All Show (y ': ys) ~ (Show y, All Show ys)`

-- p21 1.7.3 Composing constraints

To do same for `NP` (e.g., `NP f '[Int, Bool]`) need

- `(Show (f Int), Show (f Bool))`

or, if type-level function composition existed

- `((Show `Compose` f) Int, (Show `Compose` f) Bool)`

but

- no type-level lambda
- newtype wrapping produces kind `*` but need kind `Constraint`

For case of composition where result is `Constraint`:

> -- UndecidableSuperClasses
> -- UndecidableInstances
> --       MultiParamTypeClasses
> -- v     v
> class    (f (g x)) => (f `Compose` g) x
> --                    FlexibleInstances
> --                    v
> instance (f (g x)) => (f `Compose` g) x
> infixr 9 `Compose`

Inferred kind for `Compose`:

~~~{.haskell}
:kind Compose
--        ... :: (k1 -> Constraint) -> (k2 -> k1) -> k2 -> Constraint
~~~

`class` defined constraints (unlike type synonyms and type families)
can be partially applied.

~~~{.haskell}
:kind! All (Show `Compose` I) '[Int, Bool]
-- ... :: Constraint
--     = (Compose Show I Int, (Compose Show I Bool, (() :: Constraint)))
~~~

> hToString' :: All (Show `Compose` f) xs => NP f xs -> String
> hToString'      Nil  = ""
> hToString' (x :* xs) = show x ++ hToString' xs
>
> htos' = U.t "htos'" (hToString' groupNPM) "Just 'x'Just FalseJust 3"

Now can derive `Show` because it has same type requirements as `hToString'`:

~~~{.haskell}
deriving instance (All (Compose Show f) xs) => Show (NP f xs)
~~~

See end of file for manual version.

> sgnpm = U.t "sgnpm" (show groupNPM) "Just 'x' :* Just False :* Just 3 :* Nil"
> -- auto derived version would print : "Just 'x' :* (Just False :* (Just 3 :* Nil))"

-- p21 1.7.4 Proxies

use abstraction over constraints to define `hpure` variant

~~~{.haskell}
-- takes (for some parameterized constraint `c`)
forall a . c a => f a
-- instead of
forall a .        f a
~~~

-- p22

~~~{.haskell}
hpure  :: forall f xs . SListI xs  => (forall a .        f a) -> NP f xs
hcpure ::   (All c xs , SListI xs) => (forall a . c a => f a) -> NP f xs
~~~

A dummy parameter must be provided to the function (a "proxy")
to help GHC decide type.

Proxy
- simple runtime rep
- purpose is to fix the value of a type variable
- GHC, knowing `Proxy a` can infer `a`

> -- works for args of any kind (e.g., * -> Constraint)
> data Proxy (a :: k) = Proxy

> hcpure :: forall c f xs . (All c xs, SListI xs)
>        => Proxy c -> (forall a . c a => f a) -> NP f xs
> hcpure p x = case sList :: SList xs of
>     SNil  -> Nil
>     SCons -> x :* hcpure p x

> hcp1 = U.t "hcp0" (hcpure (Proxy :: Proxy Bounded) (I minBound) :: NP I '[Char, Bool])
>                   (I {unI = '\NUL'} :* (I {unI = False} :* Nil))
>
> hcp2 :: NP (K String) '[Char, Bool, Integer] -- inferred
> hcp2 = hcpure (Proxy :: Proxy Show) (Fn (K . show . unI)) `hap` groupI
> hcp2t = U.t "hcp2t" hcp2 (K {unK = "'x'"} :* K {unK = "False"} :* K {unK = "3"} :* Nil)

Composing `hcpure` with `hap` also provides mapping of constrained functions over NP.

> hcmap :: (SListI xs, All c xs)
>       => Proxy c -> (forall a . c a => f a -> g a) -> NP f xs -> NP g xs
> hcmap p f xs = hcpure p (Fn f) `hap` xs

------------------------------------------------------------------------------
Eq, Show support

> deriving instance Eq   (SList (xs :: [k]))
> deriving instance Ord  (SList (xs :: [k]))
> deriving instance Show (SList (xs :: [k]))
>
> -- manual, because built-in deriving does not use associativity info
> instance All (Show `Compose` f) xs => Show (NP f xs) where
>   showsPrec _ Nil       = showString "Nil"
>   showsPrec d (f :* fs) = showParen (d > 5)
>     $ showsPrec (5 + 1) f
>     . showString " :* "
>     . showsPrec 5 fs
>
> deriving instance All (Eq `Compose` f) xs => Eq   (NP f xs)
>
> instance All Show xs => Show (HList xs) where
>   showsPrec _ HNil           = showString "HNil"
>   showsPrec d (x `HCons` xs) = showParen (d > 5)
>     $ showsPrec (5 + 1) x
>     . showString " `HCons` "
>     . showsPrec 5 xs
>
> deriving instance All Eq xs => Eq (HList xs)

------------------------------------------------------------------------------

> test :: IO Counts
> test  =
>   runTestTT $ TestList $
>   txsum ++ tv3 ++ tvr3 ++ tva ++ tvfffvabc ++
>   gnpi ++ ghl ++ gchar ++ g2 ++ exj ++ hpn ++ hpk ++ hpt ++ hmi ++ htos ++ htos' ++ sgnpm ++
>   hcp1 ++ hcp2t


