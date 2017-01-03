module Main

import public Data.Fin

%default total

{-
Terminology: parameters and indices

Vec defines a family of types.
- indexed by length
- parameterized by element type

Distinction between parameters and indices:
- parameter is unchanged across the entire structure (e.g, every element of the vector has the same type)
- index may change across a structure (e.g., every subvector has a different length)

Distinction useful when looking at function type
- specific value of a parameter can play no part in a function’s definition
- index might change definition
-}

infixr 7 :>

data Vec : Nat -> Type -> Type where
    V0   :                              Vec  Z    a
    (:>) : (x : a) -> (xs : Vec k a) -> Vec (S k) a

%name Vec xs, ys, zs

append : Vec n elem -> Vec m elem -> Vec (n + m) elem
append       V0  ys = ys
append (x :> xs) ys = x :> append xs ys

zip : Vec n a -> Vec n b -> Vec n (a, b)
zip       V0        V0  = V0
zip (x :> xs) (y :> ys) = (x, y) :> zip xs ys

{-
Fin n : unsigned number with non-inclusive upper bound of n.
name "Fin" says number is finitely bounded.

FZ and FS are constructors of Fin, corresponding to Z and S Nat constructors.
Can use numeric literals, as with Nat.
-}
index : Fin n -> Vec n a -> a
index  FZ    (x :>  _) = x
index (FS i) (_ :> ys) = index i ys

tryIndex' : Integer -> Vec n a -> Maybe a
tryIndex' _       V0  = Nothing
tryIndex' 0 (x :>  _) = Just x
tryIndex' i (_ :> xs) = tryIndex' (i - 1) xs

tryIndex : Integer -> Vec n a -> Maybe a
tryIndex {n} i xs =
    case integerToFin i n of
        Nothing    => Nothing
        (Just idx) => Just (index idx xs)

vectTake : (m : Nat) -> Vec (m + n) a -> Vec m a
vectTake  Z           _  = V0
vectTake (S k) (x :> xs) = x :> vectTake k xs

tvt : Vec (S Z) Integer
tvt = vectTake (S Z) (1:>2:>3:>4:>5:>6:>7:>V0)
