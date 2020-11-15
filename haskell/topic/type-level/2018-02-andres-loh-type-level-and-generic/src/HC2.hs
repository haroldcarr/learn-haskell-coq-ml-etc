{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
-- {-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module HC2 where

import           GHC.Exts              (Constraint)
import qualified Test.HUnit.Util       as U (t, e)

-- p9 1.3.1 Vectors

data Nat = Zero | Suc Nat

--      KindSignatures DataKinds GADT
--          v        v   v       v
data Vec (a :: *) (n :: Nat) where
                -- DataKinds
                -- v
    VNil  ::                 Vec a  'Zero
    VCons :: a -> Vec a n -> Vec a ('Suc n)
infixr 5 `VCons`

-- StandaloneDeriving
deriving instance Eq   a => Eq   (Vec a n)
deriving instance Show a => Show (Vec a n)

type Two   =      'Suc ('Suc 'Zero)
type Three = 'Suc Two

vbc  :: Vec Char Two
vbc   = 'b' `VCons` 'c' `VCons` VNil
vabc :: Vec Char Three
vabc  = 'a' `VCons` vbc

vtail :: Vec a ('Suc n) -> Vec a n
vtail (VCons _ xs) = xs

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ VNil = VNil
vmap f (VCons x xs) = f x `VCons` vmap f xs

-- no explicit [] case, so will get runtime error
xsum :: [Int] -> Int
xsum xs = head xs + xsum (tail xs)
txsum = U.e "txsum" (xsum [1,2,3]) "Prelude.head: empty list"

vhead :: Vec a ('Suc n) -> a
vhead (VCons x _) = x

-- vsum :: Vec Int ('Suc n) -> Int
-- vsum xs = vhead xs + vsum (vtail xs)

------------------------------------------------------------------------------
-- p11 1.4 Singleton types

data SNat (n :: Nat) where
    SZero ::            SNat  'Zero
    SSuc  :: SNatI n => SNat ('Suc n)

class SNatI (n :: Nat) where
    sNat  :: SNat n

instance SNatI 'Zero where
    sNat   = SZero

instance SNatI n => SNatI ('Suc n) where
    sNat   = SSuc

--                      RankNTypes (or ExistentialQuantification, ...)
--                       v
vreplicate :: forall a n . SNatI n => a -> Vec a n
--                  ScopedTypeVariables
--                               v
vreplicate x = case sNat :: SNat n of -- choice of sNat to run at runtime is made via type at compiletime
    SZero -> VNil
    SSuc  -> x `VCons` vreplicate x

vr3 :: Vec Char Three
vr3  = vreplicate 'x'
tvr3 = U.t "tvr3" vr3 (VCons 'x' (VCons 'x' (VCons 'x' VNil)))

-- p12 1.4.2 Applicative vectors

vapply :: Vec (a -> b) n -> Vec a n -> Vec b n
vapply           VNil           VNil = VNil
vapply (f `VCons` fs) (x `VCons` xs) = f x `VCons` vapply fs xs

va :: Vec Integer ('Suc ('Suc ('Suc 'Zero)))
va  = ((2*) `VCons` (5*) `VCons` (9*) `VCons` VNil) `vapply`
      ( 1   `VCons`  4   `VCons`  7   `VCons` VNil)
tva = U.t "tva" va
      ( 2   `VCons` 20   `VCons` 63   `VCons` VNil)

------------------------------------------------------------------------------
-- p13 1.5 Heterogeneous lists

-- TODO : when/where are values, length and types handled?
data HList (xs :: [*]) where
    HNil  :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)
--                                     ^
--                                     TypeOperators
infixr 5 `HCons`

hhead :: HList (x ': xs) -> x
hhead (x `HCons`  _) = x

htail :: HList (x ': xs) -> HList xs
htail (_ `HCons` xs) = xs

-- sig can be inferred
group :: HList '[Char, Bool, Int]
group  = 'x' `HCons` False `HCons` 3 `HCons` HNil

-- p14 1.5.2 NP: n-ary products (aka "Environments")

--            PolyKinds
--            v
data NP (f :: k -> *) (xs :: [k]) where
    Nil  ::                   NP f      '[]
    (:*) :: f x -> NP f xs -> NP f (x ': xs)

infixr 5 :*

-- identity function on types
newtype I a = I { unI :: a } deriving (Eq, Read, Show)

-- p15

fromHList :: HList xs  -> NP I  xs
fromHList           HNil =  Nil
fromHList (x `HCons` xs) = I x :*      fromHList xs

toHList   :: NP I  xs  -> HList xs
toHList              Nil = HNil
toHList (I x :*      xs) =   x `HCons` toHList xs

groupNPI :: NP I '[Char, Bool, Int]
groupNPI  = fromHList group
gnpi = U.t "gnpi" groupNPI (I {unI = 'x'} :* I {unI = False} :* I {unI = 3} :* Nil)

groupHL :: HList '[Char, Bool, Int]
groupHL  = toHList groupNPI
ghl = U.t "ghl" groupHL ('x' `HCons` False `HCons` 3 `HCons` HNil)

groupNPM :: NP Maybe '[Char, Bool, Int]
groupNPM  = Just 'x' :* Just False :* Just (3::Int) :* Nil

j3 :: Num x => NP Maybe '[x]
j3  = Just 3 :* Nil

-- constant function on types
-- for any types a b, K a b isomorphic to a
newtype K a b = K {unK :: a} deriving (Eq, Read, Show)

-- useful: NP of K into normal list
hcollapse :: NP (K a) xs -> [a]
hcollapse         Nil = []
hcollapse (K x :* xs) = x : hcollapse xs

groupNPK :: NP (K Char) '[Char, Bool, Int]
groupNPK  = K 'x' :* K 'y' :* K 'z' :* Nil

gchar = U.t "gchar" (hcollapse groupNPK) "xyz"

k2 :: K Integer b
k2  = K 2
groupK2 = k2 :* k2 :* k2 :* Nil
g2 = U.t "g2" (hcollapse groupK2) [2,2,2]

xx :: NP ((->) a) '[K a b]
xx  = K :* Nil

xxx :: NP ((->) a) '[K a b1, K a b2]
xxx  = K :* K :* Nil

------------------------------------------------------------------------------
-- p15 1.6 Higher-rank types

-- The f arg to `hmap` must be polymorphic
--                  Rank2Types
--                  v
hmap :: (forall x . f x -> g x) -> NP f xs -> NP g xs
hmap _      Nil  = Nil
hmap m (x :* xs) = m x :* hmap m xs

groupI :: NP I '[Char, Bool, Integer]
groupI  = I 'x' :* I False :* I 3 :* Nil

-- p17

example :: NP Maybe '[Char, Bool, Integer]
example  = hmap (Just . unI) groupI
exj = U.t "exj" example (Just 'x' :* Just False :* Just 3 :* Nil)

-- p17 1.6.2 Applicative n-ary products

data SList (xs :: [k]) where
    SNil  :: SList '[]
    SCons :: SListI xs => SList (x ': xs)

class SListI (xs :: [k]) where
    -- | Get the explicit singleton --- the one can then pattern match on
    sList :: SList xs

instance SListI '[] where
    sList = SNil

instance SListI xs => SListI (x ': xs) where
    sList = SCons

hpure :: forall f xs . SListI xs => (forall a . f a) -> NP f xs
hpure x = case sList :: SList xs of
    SNil  -> Nil
    SCons -> x :* hpure x

-- p18

hpn = U.t "hpn" (hpure Nothing :: NP Maybe '[Char, Bool, Int])
                (Nothing :* Nothing :* Nothing :* Nil)

hpk = U.t "hpk" (hpure (K 0) :: NP (K Int) '[Char, Bool, Int])
                (K {unK = 0} :* K {unK = 0} :* K {unK = 0} :* Nil)

-- p18 1.6.3 Lifted functions

newtype (f -.-> g) a = Fn { apFn :: f a -> g a }
infix 1 -.->

hap :: NP (f -.-> g) xs -> NP f xs -> NP g xs
hap      Nil       Nil  = Nil
hap (f :* fs) (x :* xs) = apFn f x :* hap fs xs

lists :: NP [] '[String, Int]
lists  = ["foo", "bar", "baz"] :* [1 .. ] :* Nil

numbers :: NP (K Int) '[String, Int]
numbers  = K 2 :* K 5 :* Nil

{-# ANN fn_2 "HLint: ignore Avoid lambda" #-}
fn_2 :: (f a -> f' a -> f''  a)
     -> (f -.-> (f' -.->f'')) a
fn_2 f = Fn (\x -> Fn (\y -> f x y))

take' :: (K Int -.-> ([] -.->[])) a
take'  = fn_2 (\(K n) xs -> take n xs)

-- the Ints from numbers are used a the `(K n)` in take'
-- the first  number, 2, takes 2 from the          list of String
-- the second number, 5, takes 5 from the infinite list on Int
hpt = U.t "hpt" (hpure take' `hap` numbers `hap` lists)
                (["foo","bar"] :* [1,2,3,4,5] :* Nil)

-- p19 1.6.4 Another look at `hmap`

hmap' :: SListI xs => (forall a . f a -> g a) -> NP f xs -> NP g xs
hmap' f xs = hpure (Fn f) `hap` xs

hmi = U.t "hmi'" (hmap  (Just . unI) groupI)
                 (hmap' (Just . unI) groupI)

------------------------------------------------------------------------------
-- p 19 1.7 Abstracting from class and type functions

-- p19 1.7.1 The kind `Constraint`

--                         GHC.Exts (Constraint)
--                         ConstraintKinds
--                         v
type NoConstraint = (() :: Constraint)

type SomeConstraints   a = (Eq a, Show a)
type MoreConstraints f a = (Monad f, SomeConstraints a)

-- p20 1.7.2 Type functions

--   TypeFamilies
--   v
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    All c      '[]  = ()
    All c (x ': xs) = (c x, All c xs)

hToString :: All Show xs => HList xs -> String
hToString        HNil  = ""
hToString (HCons x xs) = show x ++ hToString xs

htos = U.t "htos" (hToString group) "'x'False3"

-- p21 1.7.3 Composing constraints

-- UndecidableSuperClasses
-- UndecidableInstances
--       MultiParamTypeClasses
-- v     v
class    (f (g x)) => (f `Compose` g) x
--                    FlexibleInstances
--                    v
instance (f (g x)) => (f `Compose` g) x
infixr 9 `Compose`

hToString' :: All (Show `Compose` f) xs => NP f xs -> String
hToString'      Nil  = ""
hToString' (x :* xs) = show x ++ hToString' xs

htos' = U.t "htos'" (hToString' groupNPM) "Just 'x'Just FalseJust 3"

sgnpm = U.t "sgnpm" (show groupNPM) "Just 'x' :* Just False :* Just 3 :* Nil"
-- auto derived version would print : "Just 'x' :* (Just False :* (Just 3 :* Nil))"

-- p21 1.7.4 Proxies

-- works for args of any kind (e.g., * -> Constraint)
data Proxy (a :: k) = Proxy

hcpure :: forall c f xs . (All c xs, SListI xs)
       => Proxy c -> (forall a . c a => f a) -> NP f xs
hcpure p x = case sList :: SList xs of
    SNil  -> Nil
    SCons -> x :* hcpure p x

hcp1 = U.t "hcp0" (hcpure (Proxy :: Proxy Bounded) (I minBound) :: NP I '[Char, Bool])
                  (I {unI = '\NUL'} :* (I {unI = False} :* Nil))

hcp2 :: NP (K String) '[Char, Bool, Integer] -- inferred
hcp2  = hcpure (Proxy :: Proxy Show) (Fn (K . show . unI)) `hap` groupI
hcp2t = U.t "hcp2t" hcp2 (K {unK = "'x'"} :* K {unK = "False"} :* K {unK = "3"} :* Nil)

hcmap :: (SListI xs, All c xs)
      => Proxy c -> (forall a . c a => f a -> g a) -> NP f xs -> NP g xs
hcmap p f xs = hcpure p (Fn f) `hap` xs

------------------------------------------------------------------------------

deriving instance Eq   (SList (xs :: [k]))
deriving instance Ord  (SList (xs :: [k]))
deriving instance Show (SList (xs :: [k]))

-- manual, because built-in deriving does not use associativity info
instance All (Show `Compose` f) xs => Show (NP f xs) where
  showsPrec _ Nil       = showString "Nil"
  showsPrec d (f :* fs) = showParen (d > 5)
    $ showsPrec (5 + 1) f
    . showString " :* "
    . showsPrec 5 fs

deriving instance All (Eq `Compose` f) xs => Eq   (NP f xs)

instance All Show xs => Show (HList xs) where
  showsPrec _ HNil           = showString "HNil"
  showsPrec d (x `HCons` xs) = showParen (d > 5)
    $ showsPrec (5 + 1) x
    . showString " `HCons` "
    . showsPrec 5 xs

deriving instance All Eq xs => Eq (HList xs)
