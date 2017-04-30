-- http://staff.mmcs.sfedu.ru/~ulysses/Edu/SSGEP/loh/loh-repo/Lecture1.pdf

> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}

> {-# LANGUAGE ConstraintKinds           #-}
> {-# LANGUAGE DataKinds                 #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE FlexibleInstances         #-}
> {-# LANGUAGE GADTs                     #-}
> {-# LANGUAGE KindSignatures            #-}
> {-# LANGUAGE MultiParamTypeClasses     #-}
> {-# LANGUAGE PolyKinds                 #-}
> {-# LANGUAGE Rank2Types                #-}
> {-# LANGUAGE ScopedTypeVariables       #-}
> {-# LANGUAGE StandaloneDeriving        #-}
> {-# LANGUAGE TypeFamilies              #-}
> {-# LANGUAGE TypeOperators             #-}
> {-# LANGUAGE UndecidableInstances      #-}
> {-# LANGUAGE UndecidableSuperClasses   #-}

> module HC where
> import           Data.Char             (digitToInt)
> import           GHC.Exts              (Constraint)
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, e)

-- p 7

Type-level programming in Haskell.

step-by-step exposition
- normal list
- length-indexed vectors
- heterogeneous lists
- n-ary products (aka "Environments")

-- 1.2 Kinds and data kinds

layered type system
- kinds : type of types
- types : type of terms

-- p 8

kinds
- '*' : types that terms (can be uninhabited)
    - fully applied `data` constructor is of kind `*`
    - `Int`, `Double`, ...
    - `Maybe Int`, `[Double]`, ...
- `k -> l` : type of kind `l` parameterized by type of kind `k`
    - `Maybe`, `[]`, `IO`, ...
    - `Either`, `(,)`   : `* -> * -> *`
    - `Either Int`      : `* -> *`
    - `Either Int char` : `*`

-- 1.2.2 p 8 Promoted data kinds

Data type promotion

Define new data type:

> data XBool = XFalse | XTrue

`XBool` is a new *type* with *term* constructors `XFalse`, `XTrue`

Promotion enables using this one level up.

`XBool` is a new *kind* with *type* constructors `'XFalse`, `'XTrue`

There are *no* terms/values of type `'XFalse`.

All kinds from promoted datatypes are uninhabited.

But they may appear as
- args/results of type-level functions
- parameters of datatypes and classes (most used in this paper)

-- 1.3.1 p 9 Vectors

length-indexed lists (aka vectors)

> data Nat = Zero | Suc Nat

used in promoted form:

~~~{.haskell}
'Zero :: Nat
'Suc  :: Nat -> Nat
~~~

GADT enables restricting
- `VNil`  to `Zero
- `VCons` to non-zero

> --       KindSignatures      GADT
> --             v             v
> data Vec (a :: *) (n :: Nat) where
>                 -- DataKinds
>                 -- v
>     VNil  ::                 Vec a  'Zero
>     VCons :: a -> Vec a n -> Vec a ('Suc n)
>
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

-- p 10

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
- `VNil` means `n ~ 'Zero`
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

> xsum :: [Int] -> Int
> xsum xs = head xs + xsum (tail xs)
> txsum = U.e "txsum" (xsum [1,2,3]) "Prelude.head: empty list"

via:

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

-- 1.3.2 p 10 Functions on vectors

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
QUESTION: How is it passed?

-- p 11

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
> v3 = vreplicateC 'x'
> tv3 = U.t "tv3" v3 (VCons 'x' (VCons 'x' (VCons 'x' VNil)))

PROS:
- compiletime resolution of type-level values

CONS:
- have to define new type classes for each function
- class constraints appear in types of all functions that use them
- means: leak implementation details
- means: changing signature percolates throughout code
- might produce lots of code

-- 1.4 p 11 Singleton types

Another option: create value to let GHC know value of `n`
- create a GADT `SNat n` such that it has exactly one value for each `n`
- pattern match on `SNat n` at runtime to use `n`

One way:

> data SNatX (n :: Nat) where
>     SZeroX :: SNatX 'Zero
>     SSucX  :: SNatX n -> SNatX ('Suc n)
>
> deriving instance Show (SNatX n)

> sThreeX :: SNatX Three
> sThreeX = SSucX (SSucX (SSucX SZeroX))

-- p 12

Another way: mutually-recursive (used in remainder of paper):

> data SNat (n :: Nat) where
>     SZero :: SNat 'Zero
>     SSuc  :: SNatI n => SNat ('Suc n)
>
> class SNatI (n :: Nat) where
>     sNat :: SNat n
>
> instance SNatI 'Zero where
>     sNat = SZero
>
> instance SNatI n => SNatI ('Suc n) where
>     sNat = SSuc

> --                      ExistentialQuantification
> --                       v
> vreplicate :: forall a n . SNatI n => a -> Vec a n
> --                  ScopedTypeVariables
> --                  v
> vreplicate x = case sNat :: SNat n of -- choice of sNat to run at runtime is made via type at compiletime
>     SZero -> VNil
>     SSuc  -> x `VCons` vreplicate x

> vr3 :: Vec Char Three
> vr3 = vreplicate 'x'
> tvr3 = U.t "tvr3" vr3 (VCons 'x' (VCons 'x' (VCons 'x' VNil)))

-- 1.4.2 p 12

PROS of Singletons (compared to one class per function)

- only need one class per type used in pattern matching
    - do not need that if singleton passed explicitly
    - more interface stability; exposes less of impl

CONS
- type class resolution happens at compile time
- value of `SNat` is runtime value used in pattern matching at runtime
- so, unnecessary case distinctions at runtime  since value know at compiletime
- usually produce less code

-- 1.4.2 p 12 Applicative vectors

-- p 13

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

liftA3 (,,) (ZipList [1,4,9]) (ZipList [2,8,1]) (ZipList [0,0,9])
-- ZipList {getZipList = [(1,2,0),(4,8,0),(9,1,9)]}
~~~

`vreplicate` is `pure`

`vapply` is `<*>`

> vapply :: Vec (a -> b) n -> Vec a n -> Vec b n
> vapply           VNil           VNil = VNil
> vapply (f `VCons` fs) (x `VCons` xs) = f x `VCons` vapply fs xs

> va :: Vec Integer ('Suc ('Suc ('Suc 'Zero)))
> va = ((2*) `VCons` (5*) `VCons` (9*) `VCons` VNil) `vapply`
>      ( 1   `VCons`  4   `VCons`  7   `VCons` VNil)

> tva = U.t "tva" va (VCons 2 (VCons 20 (VCons 63 VNil)))

Note: `Vec` cannot be made `Applicative` instance because its parameters are in the wrong order.

-- X BEGIN

> vff :: Vec (Char -> Int) Two
> vff  = digitToInt `VCons` digitToInt `VCons` VNil

> vfff :: Vec (Char -> Int) Three
> vfff  = digitToInt `VCons` vff

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

> tvfffvabc = U.t "tvfffvabc"
>     (vapply vfff vabc)
>     (VCons 10 (VCons 11 (VCons 12 VNil)))

-- X END

-- 1.5 p 13 Heterogeneous lists

Like `Vec` but also know type of each element.

-- 15.1.1 p 13 Promoted lists and kind polymorphism

Can use
-  `[]`         as kind constructor
- `'[]`, `(':)` as types
    - *kind-polymorphic*
    - `(':) :: a -> [a] -> [a]`

Type-level list of promoted `Bool`, `Nat`, `[*]`

~~~{.haskell}
:kind [True, False]
--              ... :: [Bool]

:kind [Zero, Three]
--              ... :: [Nat]

:kind [Char, Bool, Int]
--                  ... :: [*]
~~~

Need kind `[*]` for heterogeneous lists.

-- p 14

> -- TODO : when/where are values, length and types handled
> data HList (xs :: [*]) where
>     HNil  :: HList '[]
> --                                     TypeOperators
> --                                     v
>     HCons :: x -> HList xs -> HList (x ': xs)
>
> infixr 5 `HCons`

> hhead :: HList (x ': xs) -> x
> hhead (x `HCons`  _) = x

> htail :: HList (x ': xs) -> HList xs
> htail (_ `HCons` xs) = xs

Represent

> data Group = Group Char Bool Int

as an `HList`:

> -- sig can be inferred
> group :: HList '[Char, Bool, Int]
> group = 'x' `HCons` False `HCons` 3 `HCons` HNil

~~~{.haskell}
bh :: Int
bh = hhead group

Couldn't match type ‘Char’ with ‘Int’
    Expected type: HList '[Int, Bool, Int]
      Actual type: HList '[Char, Bool, Int]
    In the first argument of ‘hhead’, namely ‘group’
~~~

-- 1.5.2 p 14 n-ary products (aka "Environments")

`HList` variant where each element is determined by applying a type constructor
to one of the types in the index list.
- e.g., `Maybe`, `IO`

`NP` means *n-ary product

> --            PolyKinds
> --            v
> data NP (f :: k -> *) (xs :: [k]) where
>     Nil  ::                   NP f      '[]
>     (:*) :: f x -> NP f xs -> NP f (x ': xs)
>
> infixr 5 :*

`NP` means *n-ary product ("environment")
- list `xs` is signature
- kind polymorphic
    - not required to be `[*]`
    - is arbitrary type-level list of kind `[k]`
        - as long as `f` maps `k` to `*`
    - possible because elements of signature do not appear in environment
        - they appear as arg to `f` instead

Case where `k` is `*`:

> -- identity function on types
> newtype I a = I {unI :: a}

`NP I` isomorphic to `HList`:

-- p 15

> fromHList :: HList xs -> NP I xs
> fromHList           HNil = Nil
> fromHList (x `HCons` xs) = I x :* fromHList xs

> toHList :: NP I xs -> HList xs
> toHList         Nil = HNil
> toHList (I x :* xs) = x `HCons` toHList xs

No constraints on `xs` so no need for type class.

> npgroup :: NP I '[Char, Bool, Int]
> npgroup = fromHList group
>
> group' :: HList '[Char, Bool, Int]
> group' = toHList npgroup

> groupNPM :: NP Maybe '[Char, Bool, Int]
> groupNPM = Just 'x' :* Just False :* Just (3::Int) :* Nil

> j3 :: Num x => NP Maybe '[x]
> j3 = Just 3 :* Nil

`NP` also a generalization of homogeneous vectors:

> -- constant function on types
> -- for any types a b, K a b isomorphic to a
> newtype K a b = K {unK :: a}

> -- useful: NP of K into normal list
> hcollapse :: NP (K a) xs -> [a]
> hcollapse         Nil = []
> hcollapse (K x :* xs) = x : hcollapse xs

> k2 :: K Integer b
> k2 = K 2

> xx :: NP ((->) a) '[K a b]
> xx = K :* Nil

> xxx :: NP ((->) a) '[K a b, K a b1]
> xxx = K :* K :* Nil

-- 1.6 p 15 Higher-rank types

Define functions on `NP`

-- 1.6.1 p 15 map

`vmap`
- preserves length
- changes type

~~~{.haskell}
vmap :: (a -> b) -> Vec a n -> Vec b n
~~~

- no single type of elements
- *predictably* change types
- type constructor `f` says how to interpret each element

-- p 16

Generalize `vmap` to `hmap`

- preserve index
- change type constructor

~~~{.haskell}
-- type preserving map
NP I xs -> NP I xs

-- type-unifying map
MP I xs -> NP (K a) xs

-- produces values of different types from homogeneous list
NP (K a) xs -> NP I xs
~~~

Polymorphic Haskell function:

- caller free to choose
- callee make no assumptions

If arg function is polymorphic:
- caller *must* pass in polymorphic function
- called may flexibly use it on any type

> -- The arg to `hmap` must be polymorphic
> --                  Rank2Types
> --                  v
> hmap :: (forall x . f x -> g x) -> NP f xs -> NP g xs
> hmap _      Nil  = Nil
> hmap m (x :* xs) = m x :* hmap m xs

Example

> groupI :: NP I '[Char, Bool, Integer]
> groupI = I 'x' :* I False :* I 3 :* Nil

-- p 17

> example :: NP Maybe '[Char, Bool, Integer]
> example = hmap (Just . unI) groupI

-- 1.6.2 p 17 Applicative n-ary products

applicative for `NP` compared to `vreplicate`:
- role of `n` is now the signature `xs`
- role of 'a' is now the type constructor `f`
    - `f` must accept any type in the signature `xs`

Need a singleton for lists (following same pattern of `SNat`):

> data SList (xs :: [k]) where
>     SNil  :: SList '[]
>     SCons :: SListI xs => SList (x ': xs)
>
> class SListI (xs :: [k]) where
>     sList :: SList xs
>
> instance SListI '[] where
>     sList = SNil
>
> instance SListI xs => SListI (x ': xs) where
>     sList = SCons

Above not ideal:

- says nothing about elements of the list
- would like `SCons` to include singleton for head and singleton for tail

> hpure :: forall f xs . SListI xs => (forall a . f a) -> NP f xs
> hpure x = case sList :: SList xs of
>     SNil  -> Nil
>     SCons -> x :* hpure x

-- p 18

~~~{.haskell}
:t hpure Nothing
-- hpure Nothing :: SListI xs => NP Maybe xs

:t hpure Nothing :: NP Maybe '[Char, Bool, Int]
--            .. :: NP Maybe '[Char, Bool, Int]

:t hpure (K 0)
-- hpure (K 0) :: (Num a, SListI xs) => NP (K a) xs

:t hpure (K 0) :: NP (K Int) '[Char, Bool, Int]
-- ..          :: NP (K Int) '[Char, Bool, Int]
~~~

-- 1.6.3 p 18 Lifted functions

Need `vap` (analogous to `vapply` : a kind of (`<*>`) )

To define, need a type-level `\x -> (f x -> g x)`
but Haskell has no type-level lambda, so:

> newtype (f -.-> g) a = Fn {apFn :: f a -> g a}
> infix 1 -.->

Then use to represent type-level function in:

> hap :: NP (f -.-> g) xs -> NP f xs -> NP g xs
> hap      Nil       Nil  = Nil
> hap (f :* fs) (x :* xs) = apFn f x :* hap fs xs

Examples

> lists :: NP [] '[String, Int]
> lists = ["foo", "bar", "baz"] :* [1 .. 10] :* Nil

> numbers :: NP (K Int) '[String, Int]
> numbers = K 2 :* K 5 :* Nil

> fn_2 :: (f a -> f' a -> f''  a)
>      -> (f -.-> (f' -.-> f'')) a
> fn_2 f = Fn (\x -> Fn (\y -> f x y))

> take' :: (K Int -.-> ([] -.-> [])) a
> take' = fn_2 (\(K n) xs -> take n xs)

~~~{.haskell}
:t hpure take' `hap` numbers `hap` lists
-- ... :: NP [] '[String, Int]
~~~{.haskell}

-- 1.6.4 p 19 Another look at `hmap`

Identity, for normal lists (assuming `ZipList` applicative):

~~~{.haskell}
map f = pure f <*> xs
~~~

also holds for n-ary products:

> hmap' :: SListI xs => (forall a . f a -> g a) -> NP f xs -> NP g xs
> hmap' f xs = hpure (Fn f) `hap` xs

-- 1.7 p 19 Abstracting from class and type functions

Cannot do:

~~~{.haskell}
-- try to do: NP I -> NP (K String)
hmap (K . show . unI) group'
~~~

because

~~~{.haskell}
:t K . show . unI
--            ... :: Show a => I a -> K String b
~~~

does not match

~~~{.haskell}
                               f x -> g        x
~~~

the class constraint on `Show`.

But useful to enable functions with class constraints to be used with `hmap`
(assuming elements meet constraint).

-- 1.7.1 p 19 The kind `Constraint`

Classes can be seen as types with a different kind.

- `data` produces types of kind `*`
- class produces types of kind `Constraint`

Examples

- `Show`, `Eq`, `Ord` have kind `* -> Constraint`
- `Functor`, `Monad`  have kind `(* -> *) -> Constraint`
- multiparameter `MonadReader` has kind `* -> (* -> *) -> Constraint`

-- p 20

Use tuple syntax to create empty constraints

> --                         GHC.Exts (Constraint)
> --                         ConstraintKinds
> --                         v
> type NoConstraint = (() :: Constraint)

or to combine constraints

> type SomeConstraints a   = (Eq a, Show a)
> type MoreConstraints f a = (Monad f, SomeConstraints a)

-- 1.7.2 p 20 Type functions

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

~~~{.haskell}
hToString group
-- "'x'False3"
~~~

Pattern matching on `HCons` says

- `xs ~ y ': ys`
- so: `All Show xs ~ All Show (y ': ys) ~ (Show y, All Show ys)`

-- 1.7.3 p 21 Composing constraints

To do same for `NP` (e.g., `NP f '[Int, Bool]`) need

- `(Show (f Int), Show (f Bool))`

or, if type-level function composition existed

- `((SHow `Compose` f) Int, (Show `Compose` f) Bool)`

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

Inferred kind for `Compose`:

~~~{.haskell}
Compose :: (b -> Constraint) -> (a -> b) -> a -> Constraint
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

~~~{.haskell}
hToString' groupNPM
-- "Just 'x'Just FalseJust 3"
~~~

Now can derive `Show` because it has same type requirements as `hToString'`:

> deriving instance (All (Compose Show f) xs) => Show (NP f xs)

~~~{.haskell}
show groupNPM
-- "Just 'x' :* (Just False :* (Just 3 :* Nil))"
~~~

-- 1.7.4 p 22 Proxies

Use abstraction over constraints to define `hpure` variant

~~~{.haskell}
-- takes (for some parameterized constraint `c`)
forall a . c a => f a
-- instead of
forall a .        f a
~~~

~~~{.haskell}
hpure  :: forall f xs . SListI xs => (forall a .        f a) -> NP f xs

hcpure ::  (SListI xs,  All c xs) => (forall a . c a => f a) -> NP f xs
~~~

A dummy parameter must be provided to the function (a "proxy")
to help GHC decide type.

Proxy
- simple runtime rep
- purpose is to fix the value of a type variable
- GHC, knowing `Proxy a` can infer `a`

> -- works for args of any kind (e.g., * -> Constraint)
> data Proxy (a :: k) = Proxy

> hcpure :: forall c f xs . (SListI xs, All c xs)
>        => Proxy c -> (forall a . c a => f a) -> NP f xs

-- p 23

> hcpure p x = case sList :: SList xs of
>     SNil  -> Nil
>     SCons -> x :* hcpure p x

> hcp1 :: NP I '[Char, Bool] -- inferred
> hcp1 = hcpure (Proxy :: Proxy Bounded) (I minBound) :: NP I '[Char, Bool]

> hcp2 :: NP (K String) '[Char, Bool, Int] -- inferred
> hcp2 = hcpure (Proxy :: Proxy Show) (Fn (K . show . unI)) `hap` npgroup

Composing `hcpure` with `hap` also provides mapping of constrained functions over environments.

> hcmap :: (SListI xs, All c xs)
>       => Proxy c -> (forall a . c a => f a -> g a) -> NP f xs -> NP g xs
> hcmap p f xs = hcpure p (Fn f) `hap` xs

------------------------------------------------------------------------------

> test :: IO Counts
> test  =
>     runTestTT $ TestList $ txsum ++ tv3 ++ tvr3 ++ tva ++ tvfffvabc
