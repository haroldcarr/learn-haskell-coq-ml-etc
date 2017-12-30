> {-# LANGUAGE DataKinds      #-}
> {-# LANGUAGE GADTs          #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE PolyKinds      #-}
> {-# LANGUAGE TypeFamilies   #-}
> {-# LANGUAGE TypeOperators  #-}
>
> module DataVect where
>
> import           GHC.Types       hiding (Nat)
> import           Test.HUnit      (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util as U (t)

2. A Variety of Quantifiers

> data Nat = Z | S Nat deriving (Eq, Ord, Show)

`DataKinds` : makes Nat a kind (besides a type)
- Nat is a TYPE with VALUE CONSTRUCTORS  Z and  S
- Nat is a KIND with TYPE  CONSTRUCTORS 'Z and 'S

used below in `Nat` definition to "index" the return type
- note: haskell separation of types and kinds requires separate kind-level replica

`KindSignatures` benefits:
- style: does not introduce ununsed names
  - e.g., 1. `data MarkedList    a    b      where`
  -       2. `data MarkedList :: * -> * -> * where`
  - 1 uses names a and b that are not used in body (each constructor binds its own names).
  - 2 avoids names
- enables specifying an alternate kind
  - e.g., `data X a = X` says X has kind * -> * and its parameter must be nullary.
  - can specify a unary kind for its parameter via `data X (a :: * -> *) = X`
    where X would have kind (* -> *) -> *
    so can parameterize X over functors or monads or something not nullary

`GADTs` : enables more precise return type of constructors

> infixr 7 :>
>
> data Vec :: Nat -> Type -> Type where
>   V0   ::                 Vec  Z    a
>   (:>) :: a -> Vec n a -> Vec (S n) a

Choose order of arguments carefully since Haskell supports partial application at type level but not λ-abstraction.

> instance Eq a => Eq (Vec n a) where
>   (==)        V0       V0  = True
>   (==) (l :> ls) (r :> rs) = l == r && ls == rs

> instance Show a => Show (Vec n a) where
>   show V0        = "V0"
>   show (l :> ls) = show l ++ " :> " ++ show ls

Example departs from dependently typed tradition
- length index to left of payload type parameter
  (to develop the functorial structure of each Vec n below)

> t1 = U.t "t1"
>    (3 :> 2 :> 1 :> V0)
>    (3 :> 2 :> 1 :> V0)

TypeFamilies, TypeOperators : compute at type level
- define "families" (meaning "functions") of "types"
  in the sense of "things at the type level",
  not just the sense of "things of kind ⋆".

> type family (m :: Nat) :+ (n :: Nat) :: Nat
> type instance Z   :+ n =         n
> type instance S m :+ n = S (m :+ n)

Haskell : type equality is syntactic.

Above type family : axioms for propositional equality.
Programs that rely on type-level computation must be elaborated in terms of explicit appeal to evidence.
The translation from surface language to kernel attempts to generate evidence by a constraint solving heuristic.

> vappend :: Vec m a -> Vec n a -> Vec (m :+ n) a
> vappend       V0  ys =                 ys
> vappend (x :> xs) ys = x :> vappend xs ys

The type-level numbers in vappend are compiletime: flow of control determined from constructors of first vector.

> t2 = U.t "t2"
>    (vappend (6 :> 5 :> 4 :> V0) (3 :> 2 :> 1 :> V0))
>    (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0)

Sometimes numbers needed at runtime. E.g., chopping a vector in two:

vchop :: Vec (m :+ n) x -> (Vec m x, Vec n x)

Need m at runtime.

But Haskell’s dependent ∀ quantifier is for implicit and static things.

Standard solution : define runtime replica of static data as a singleton GADT.

> data Natty :: Nat -> * where
>   Zy :: Natty Z
>   Sy :: Natty n -> Natty (S n)

Each type level n in Nat kind has corresponding type Natty n.

The ‘Π-types’, often written (x : S) -> T, of dependent type theory abstract dependently over explicit dynamic things.

Simulate in Haskell by abstracting dependently at the type level and non-dependently over singleton representative.

> vchop :: Natty m
>       -> Vec (m :+ n) a
>       -> (Vec m a, Vec n a)
> vchop  Zy          xs  = (V0     , xs)
> vchop (Sy k) (x :> xs) = (x :> ys, zs)
>   where (ys, zs) = vchop k xs

> t3 = U.t "t3"
>    (vchop (Sy (Sy Zy)) (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0))
>    ( 6 :> 5 :> V0
>    , 4 :> 3 :> 2 :> 1 :> V0
>    )

To write:

vtake :: Nat -> Vec (m :+ n) a -> Vec m a
vtake  Z          xs  = V0
vtake (S k) (x :> xs) = x :> vtake k xs

Need to tell GHC how to instantiate n in the recursive call
(to reason about addition : (m :+ ) is injective.
- To GHC, it is just an unknown axiomatised function.
- Not a problem in vchop, because relaying the suffix, zs, from the recursive output to the result makes clear that the same n is needed in both places.
- n is not needed at run time, but without it there is no way to see that the program makes sense.
- summary: there are data that, despite being static, must be made explicit. One way : ‘proxy types’

PolyKinds : TODO

> data Proxy :: k -> * where
>   Proxy :: Proxy i

> vtake :: Natty m -> Proxy n -> Vec (m :+ n) a -> Vec m a
> vtake  Zy    n       xs  = V0
> vtake (Sy m) n (x :> xs) = x :> vtake m n xs

> t4 = U.t "t4"
>    (vtake (Sy (Sy Zy))
>           (Proxy :: Proxy anything)
>           (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0))
>    (6 :> 5 :> V0)

-- TODO

> type family Arity (n :: Nat) (a :: *) :: *
> type instance Arity  Z    x = x
> type instance Arity (S n) x = x -> Arity n x

> varity :: Arity n x -> Vec n x -> x
> varity x       V0  =           x
> varity f (x :> xs) = varity (f x) xs

 > t5 = U.t "t5"
 >    (varity _ (2 :> 1 :> V0))
 >    (3 :> 2 :> V0)

------------------------------------------------------------------------------

> test :: IO Counts
> test =
>   runTestTT $ TestList $ t1 ++ t2 ++ t3 ++ t4
