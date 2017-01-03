module DataVect

import public Data.Fin
import Language.Reflection

%access public export
%default total

infixr 7 :>

data Vec : Nat -> Type -> Type where
  V0   :                 Vec  Z    a
  (:>) : a -> Vec n a -> Vec (S n) a

implementation Eq a => Eq (Vec n a) where
  (==)       V0        V0  = True
  (==) (l :> ls) (r :> rs) = l == r && ls == rs

implementation Show a => Show (Vec n a) where
  show       V0  = "V0"
  show (x :> xs) = show x ++ " :> " ++ show xs

vappend : Vec m a -> Vec n a -> Vec (m + n) a
vappend       V0  ys = ys
vappend (x :> xs) ys = x :> vappend xs ys

vtake : (m : Nat) -> Vec (m + n) a -> Vec m a
vtake  Z          xs  = V0
vtake (S k) (x :> xs) = x :> vtake k xs

vdrop : (n : Nat) -> Vec (n + m) a -> Vec m a
vdrop  Z          xs  =        xs
vdrop (S k) (x :> xs) = vdrop k xs

vlength : Vec n _ -> Nat
vlength {n} _ = n

vchop : (m : Nat) -> Vec (m + n) a -> (Vec m a, Vec n a)
vchop m xs = (vtake m xs, vdrop m xs)
{-
vchop' : (m : Nat) -> Vec (m + n) a -> (Vec m a, Vec n a)
vchop'  Z          xs  = (V0, xs)
vchop' (S k) (x :> xs) = (x :> Prelude.Basics.fst yszs, Prelude.Basics.snd yszs)
  where yszs = vchop' k xs
-}

-- TODO

arity : (n : Nat) -> (a : Type) -> Type
arity  Z    x = x
arity (S n) x = x -> arity n x

-- varity :: Arity n x -> Vec n x -> x
-- varity x       V0  =           x
-- varity f (x :> xs) = varity (f x) xs

