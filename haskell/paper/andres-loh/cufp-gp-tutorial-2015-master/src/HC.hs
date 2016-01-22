{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}

{-
http://staff.mmcs.sfedu.ru/~ulysses/Edu/SSGEP/loh/loh-repo/Lecture1.pdf
-}

module HC where

import           Data.Char (digitToInt)

-- 1.3 p 9

data Nat = Zero | Suc Nat

--       KindSignatures      GADT
--             v             v
data Vec (a :: *) (n :: Nat) where
                -- DataKinds
                -- v
    VNil  :: Vec a 'Zero
    VCons :: a -> Vec a n -> Vec a ('Suc n)

infixr 5 `VCons`

type Two   =      'Suc ('Suc 'Zero)
type Three = 'Suc Two

vbc  :: Vec Char Two
vbc   = 'b' `VCons` 'c' `VCons` VNil
vabc :: Vec Char Three
vabc  = 'a' `VCons` vbc

-- StandaloneDeriving
deriving instance Show a => Show (Vec a n)

-- p 10

vtail :: Vec a ('Suc n) -> Vec a n
vtail (VCons _ xs) = xs

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap _ VNil = VNil
vmap f (VCons x xs) = f x `VCons` vmap f xs

-- HC BEGIN
vhead :: Vec a ('Suc n) -> a
vhead (VCons x _) = x

-- hcsum [1,2,3]
-- *** Exception: Prelude.head: empty list
hcsum :: [Int] -> Int
hcsum xs = head xs + hcsum (tail xs)

{-
Couldn't match type ‘n’ with ‘'Suc n0’
  ‘n’ is a rigid type variable bound by
      the type signature for vsum :: Vec Int ('Suc n) -> Int
      at /var/folders/gl/qvfwc6bs79gf3mx224fx1qr00000gn/T/flycheck3386Hgu/HC.hs:50:9
Expected type: Vec Int ('Suc ('Suc n0))
  Actual type: Vec Int ('Suc n)
Relevant bindings include
  xs :: Vec Int ('Suc n)
    (bound at /var/folders/gl/qvfwc6bs79gf3mx224fx1qr00000gn/T/flycheck3386Hgu/HC.hs:51:6)
  vsum :: Vec Int ('Suc n) -> Int
    (bound at /var/folders/gl/qvfwc6bs79gf3mx224fx1qr00000gn/T/flycheck3386Hgu/HC.hs:51:1)
In the first argument of ‘vtail’, namely ‘xs’

vsum :: Vec Int ('Suc n) -> Int
vsum xs = vhead xs + vsum (vtail xs)
-}
-- HC END

-- p 11

class VReplicateC (n :: Nat) where
    vreplicateC :: a -> Vec a n

instance VReplicateC 'Zero where
    vreplicateC _ = VNil

instance VReplicateC n => VReplicateC ('Suc n) where
    vreplicateC x = x `VCons` vreplicateC x

v3 :: Vec Char Three
v3 = vreplicateC 'x'

-- 1.4 p 11

data SNatX (n :: Nat) where
    SZeroX :: SNatX 'Zero
    SSucX  :: SNatX n -> SNatX ('Suc n)

sThreeX :: SNatX Three
sThreeX = SSucX (SSucX (SSucX SZeroX))

deriving instance Show (SNatX n)

-- p 12

data SNat (n :: Nat) where
    SZero :: SNat 'Zero
    SSuc  :: SNatI n => SNat ('Suc n)

class SNatI (n :: Nat) where
    sNat :: SNat n

instance SNatI 'Zero where
    sNat = SZero

instance SNatI n => SNatI ('Suc n) where
    sNat = SSuc

--                      ExistentialQuantification
--                       v
vreplicate :: forall a n . SNatI n => a -> Vec a n
--                  ScopedTypeVariables
--                  v
vreplicate x = case sNat :: SNat n of
    SZero -> VNil
    SSuc  -> x `VCons` vreplicate x

vr3 :: Vec Char Three
vr3 = vreplicate 'x'

-- p 13

vapply :: Vec (a -> b) n -> Vec a n -> Vec b n
vapply VNil           _              = VNil
vapply _              VNil           = VNil
vapply (f `VCons` fs) (x `VCons` xs) = f x `VCons` vapply fs xs


vff :: Vec (Char -> Int) Two
vff  = digitToInt `VCons` digitToInt `VCons` VNil

vfff :: Vec (Char -> Int) Three
vfff  = digitToInt `VCons` vff
{-
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

vapply vfff vabc
VCons 10 (VCons 11 (VCons 12 VNil))
-}

-- 1.5 p 14

data HList (xs :: [*]) where
    HNil :: HList '[]
--                                     TypeOperators
--                                     v
    HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 `HCons`

data Group = Group Char Bool Int

-- group :: HList '[Char, Bool, Int]
group = 'x' `HCons` False `HCons` (3::Int) `HCons` HNil

--            PolyKinds
--            v
data NP (f :: k -> *) (xs :: [k]) where
    Nil  :: NP f '[]
    (:*) :: f x -> NP f xs -> NP f (x ': xs)

infixr 5 :*

newtype I a = I {unI :: a}

-- p 15

fromHList :: HList xs -> NP I xs
fromHList HNil = Nil
fromHList (x `HCons` xs) = I x :* fromHList xs

toHList :: NP I xs -> HList xs
toHList Nil = HNil
toHList (I x :* xs) = x `HCons` toHList xs

npgroup :: NP I '[Char, Bool, Int]
npgroup = fromHList group

group' :: HList '[Char, Bool, Int]
group' = toHList npgroup

newtype K a b = K {unK :: a}

type NPT = NP (K Double) '[Char, Bool, Int]

hcollapse :: NP (K a) xs -> [a]
hcollapse Nil = []
hcollapse (K x :* xs) = x : hcollapse xs

doublegroup :: HList '[Double, Double, Double]
doublegroup = (1.0::Double) `HCons` (2.0::Double) `HCons` (3.0::Double) `HCons` HNil
