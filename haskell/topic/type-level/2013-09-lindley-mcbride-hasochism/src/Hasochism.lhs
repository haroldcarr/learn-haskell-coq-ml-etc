> {-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults                  #-}
>
> {-# LANGUAGE DataKinds      #-}
> {-# LANGUAGE GADTs          #-}
> --{-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE PolyKinds      #-}
> {-# LANGUAGE RankNTypes     #-}
> {-# LANGUAGE TypeFamilies   #-}
> {-# LANGUAGE TypeOperators  #-}
>
> module Hasochism where
>
> import           Data.Traversable (foldMapDefault)
> import           GHC.Types        hiding (Nat)
> import           Test.HUnit       (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util  as U (t)
>
> default (Int, Float)

------------------------------------------------------------------------------
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

Choose order of arguments carefully since Haskell supports partial application at type level
but not λ-abstraction.

> instance Eq a => Eq (Vec n a) where
>   (==)        V0       V0  = True
>   (==) (l :> ls) (r :> rs) = l == r && ls == rs

> instance Show a => Show (Vec n a) where
>   show V0        = "V0"
>   show (l :> ls) = show l ++ " :> " ++ show ls

Example departs from dependently typed tradition
- length index to left of payload type parameter
  (to develop the functorial structure of each Vec n below)

> t1 :: [Test]
> t1  = U.t "t1"
>   (3 :> 2 :> 1 :> V0)
>   (3 :> 2 :> 1 :> V0)

TypeFamilies, TypeOperators : compute at type level
- define "families" (meaning "functions") of "types"
  in the sense of "things at the type level",
  not just the sense of "things of kind ⋆".

open type family:

> type family (m :: Nat) :+ (n :: Nat) :: Nat
> type instance Z   :+ n =         n
> type instance S m :+ n = S (m :+ n)

Haskell : type equality is syntactic.

Above type family : axioms for propositional equality.

Programs that use type-level computation must be elaborated in terms of explicit appeal to evidence.

Translation from surface language to kernel
tries to generate evidence by constraint solving heuristic.

> vappend :: Vec m a -> Vec n a -> Vec (m :+ n) a
> vappend       V0  ys =                 ys
> vappend (x :> xs) ys = x :> vappend xs ys

type-level numbers in vappend are compiletime:
- flow of control determined from constructors of first vector

> t2 :: [Test]
> t2  = U.t "t2"
>   (vappend (6 :> 5 :> 4 :> V0) (3 :> 2 :> 1 :> V0))
>   (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0)

Sometimes numbers needed at runtime. E.g., chopping a vector in two:

vchop :: Vec (m :+ n) x -> (Vec m x, Vec n x)

Need m at runtime.

But Haskell’s dependent ∀ quantifier is for implicit and static things.

Standard solution : define runtime replica of static data as a singleton GADT.

> data Natty :: Nat -> * where
>   Zy ::            Natty  Z
>   Sy :: Natty n -> Natty (S n)

Each type level n in Nat kind has corresponding type Natty n.

Π-types
- correspond to universal quantification
  of dependent type theory abstract dependently over explicit dynamic things
- generalization of function types
  A → B
  to dependent function types
  (x : A) → B
  where x might occur in B.
- the type B of the returned value depends on arg x

Simulate : abstract dependently at type level and non-dependently over singleton representative.

> vchop :: Natty m
>       -> Vec (m :+ n) a
>       -> (Vec m a, Vec n a)
> vchop  Zy          xs  = (V0     , xs)
> vchop (Sy k) (x :> xs) = (x :> ys, zs)
>   where (ys, zs) = vchop k xs

> t3 :: [Test]
> t3  = U.t "t3"
>  (vchop (Sy (Sy Zy)) (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0))
>  ( 6 :> 5 :> V0
>  , 4 :> 3 :> 2 :> 1 :> V0
>  )

Can only construct Π-types with domains admitting the singleton construction
- currently (2013) : simple data structures
- cannot do (n : Nat) → (xs : Vec n x ) → T [xs]
- we expect this gap to be plugged in the near future

To write:

vtake :: Nat -> Vec (m :+ n) a -> Vec m a
vtake  Z          xs  = V0
vtake (S k) (x :> xs) = x :> vtake k xs

Need to tell GHC how to instantiate n in the recursive call
(to reason about addition : (m :+ ) is injective.
- To GHC, it is just an unknown axiomatised function.
- Not a problem in vchop, because relaying the suffix, zs,
  from the recursive output to the result makes clear that the same n is needed in both places.
- n not needed at run time, but without it, no way to see the program makes sense.
- summary: there are data that, despite being static, must be made explicit.
One way : ‘proxy types’

> data Proxy :: k -> * where
>   Proxy :: Proxy i

the only dynamic information in Proxy i is defined-ness, which there is never the need to check.
Only point of proxy : to point out it has the same type at binding and usage sites.

PolyKinds

data App f a = MkApp (f a)
                                       f                         a
Haskell 98 : inferred kind for App is (Type -> Type)          -> Type           -> Type
But it could be                      ((Type -> Type) -> Type) -> (Type -> Type) -> Type
With kind polymorphism (PolyKinds), GHC infers
                            forall k. (k    -> Type)          -> k              -> Type

> vtake :: Natty m -> Proxy n -> Vec (m :+ n) a -> Vec m a
> vtake  Zy    _        _  = V0
> vtake (Sy m) n (x :> xs) = x :> vtake m n xs

> t4 :: [Test]
> t4  = U.t "t4"
>   (vtake (Sy (Sy Zy))
>          (Proxy :: Proxy anything)
>          (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0))
>   (6 :> 5 :> V0)

example : implicit quantification over data used statically to compute a type but erased at runtime

apply an n-ary operator to an n-vector of arguments

> type family Arity (n :: Nat) (x :: *) :: *
> type instance Arity  Z    x = x
> type instance Arity (S n) x = x -> Arity n x

> varity :: Arity n x -> Vec n x -> x
> varity x       V0  =           x
> varity f (x :> xs) = varity (f x) xs

>
> t5 :: [Test]
> t5  = U.t "t5"
>   (varity (+) (2 :> 1 :> V0))
>   3

> t6 :: [Test]
> t6  = U.t "t6"
>   (varity (\w x y z -> w + x + y + z) (1 :> 10 :> 100 :> 1000 :> V0))
>   1111

Summary

distinguished Haskell’s dependent static implicit ∀·quantifier
from the dependent dynamic explicit Π-types of dependent type theory.

How to make ∀· quantifier
- static and explicit with a Proxy, and
- how to make it dynamic and explicit whenever the singleton construction is possible

Haskell struggles to simulate Π with ∀·, the reverse is the case in type theory.

Needed: on both sides : a more systematic treatment of the varieties of quantification.

------------------------------------------------------------------------------
3. Explicit and Implicit Π-Types

singletons (e.g., Natty)
- simulate a dependent dynamic EXPLICIT quantifier,
  corresponding to the explicit Π-type of type theory: Agda’s (x : S) → T

Impls of type theory often support a dependent dynamic IMPLICIT quantifier
e.g., AGDA: the {x : S} → T
enabling type constraints to induce the synthesis of useful information.

Haskell analogue of implicit Π is constructed with singleton CLASSES.

-- single returns Natty singleton corresponding to each promoted Nat.
-- A NATTY number is known at run time, despite not being given explicitly.

> class NATTY (n :: Nat) where
>  -- enables extracting an explicit singleton using implicit runtime knowledger of a value
>  natty :: Natty n
>
> instance NATTY Z where
>   natty = Zy
>
> instance NATTY n => NATTY (S n) where
>   natty = Sy natty

Enables writing a more implicit version of vtake:

> -- return type determines the required length
> -- singleton construction happens in instance inference
> vtrunc :: NATTY m => Proxy n -> Vec (m :+ n) x -> Vec m x
> vtrunc = vtake natty

> t7 :: [Test]
> t7  = U.t "t7"
>   (vtrunc Proxy (1 :> 2 :> 3 :> 4 :> V0) :: Vec (S (S Z)) Int)
>   (1 :> 2 :> V0)

3.1 Instances for Indexed Types

convenient to omit singleton args when machine can figure them out
- but is additional cost of defining singleton classes/types worth it?
- yes : when no choice but to work implicitly:
  when not possible to abstract an instance over a singleton type, but it can be constrained
e.g.,:

> instance Functor (Vec n) where
>   fmap _       V0  = V0
>   fmap f (x :> xs) = f x :> fmap f xs
>
>
> -- needs to inspect a run time length to make the right number of copies
> vcopies :: forall n x. Natty n -> x -> Vec n x
> vcopies  Zy    _ = V0
> vcopies (Sy n) x = x :> vcopies n x
>
> -- pointwise application, requiring only static knowledge of the length
> vapp :: forall n s t. Vec n (s -> t) -> Vec n s -> Vec n t
> vapp V0 V0 = V0
> vapp (f :> fs) (s :> ss) = f s :> vapp fs ss
>
> instance NATTY n => Applicative (Vec n) where
>   pure  = vcopies natty
>   (<*>) = vapp

3.2 Matrices and a Monad

Vec n is Applicative and Traversable (and Foldable)

> instance Traversable (Vec n) where
>   traverse _ V0 = pure V0
>   traverse f (x :> xs) = (:>) <$> f x <*> traverse f xs
>
> instance Foldable (Vec n) where
>   foldMap = foldMapDefault

> -- vertical vector of horizontal vectors
> data Matrix :: * -> (Nat, Nat) -> * where
>   Mat :: {unMat :: Vec h (Vec w a)} -> Matrix a '(w, h)
>
> -- 'w' width is used at runtime -- not known in case where height is Z
> transpose :: NATTY w => Matrix a '(w, h) -> Matrix a '(h, w )
> transpose = Mat . sequenceA . unMat
>
> vtail :: Vec (S n) x -> Vec n x
> vtail (_ :> xs) = xs
>
> diag :: Matrix x '(n, n) -> Vec n x
> diag (Mat              V0 )  = V0
> diag (Mat ((x :> _) :> xss)) = x :> diag (Mat (fmap vtail xss))
>
> instance NATTY n => Monad (Vec n) where
>   return   = pure
>   xs >>= f = diag (Mat (fmap f xs))

3.3 Exchanging Explicit and Implicit

above
class NATTY natty
enables extracting an explicit singleton using implicit runtime knowledger of a value

to work the other way around, e.g.,

have an explicit Natty n
need use it in a context with an implicit NATTY n type class constraint.

Use GHC to building a NATTY n dictionary:

> natter :: Natty n -> (NATTY n => t) -> t
> natter  Zy    t = t
> natter (Sy n) t = natter n t

natter is an identity function.
The t being passed recursively is successively but silently precomposed with
the dictionary transformer generated from the instance NATTY n ⇒ NATTY (S n) declaration.

the constructed dictionary contains an exact replica of the Natty n value which natter has traversed

now have a complete matrix of dependent quantifiers
shown here for the example of natural numbers

            implicit                   explicit
static    ∀(n :: Nat).          ∀(n :: Nat). Proxy n ->
dynamic  ∀n. NATTY n =>             ∀n. Natty n ->

involving the kind Nat
and two ways (neither of which is the type Nat) to give its inhabitants run time representation
NATTY and Natty,
which are (clumsily) interchangeable despite the former wrapping the latter.

3.4 The NATTY-in-Natty Question

above did this  singleton representation of natural numbers

data Natty :: Nat → ? where
  Zy ::           Natty  Z
  Sy :: Natty n → Natty (S n)

Eisenberg and Weirich Singletons design choice is to insert a NATTY constraint successor case,
effectively storing two copies of the predecessor

> data NattyEW :: Nat -> * where
>   ZyEW ::                         NattyEW  Z
>   SyEW :: NATTY n => NattyEW n -> NattyEW (S n)

Each choice has advantages and disadvantages.
unconstrained : easier construction of singletons
constrained   : more powerful elimination

> -- without NATTY constraint on Sy
> vlength :: Vec n x -> Natty n
> vlength       V0  = Zy
> vlength (_ :> xs) = Sy (vlength xs)

> -- with NATTY constraint
> -- more complex in order to bring appropriate NATTY constraint into scope for inductive case
> vlengthEW :: Vec n x -> Natty n
> vlengthEW       V0  = Zy
> vlengthEW (_ :> xs) = natter n (Sy n) where n = vlengthEW xs

another example

> -- identity matrix of size n.
> -- Here, a singleton is being eliminated
> -- Without NATTY constraint on Sy
> -- must use natter to enable the use of the relevant Applicative structure
> idMatrix :: Natty n -> Matrix Int '(n, n)
> idMatrix  Zy    = Mat V0
> idMatrix (Sy n) = natter n $ Mat ((1 :> pure 0) :> ((0:>) <$> unMat (idMatrix n)))

> -- with NATTY constraint on Sy
> -- do not need natter, because required constraint is brought into scope by pattern matching.
> idMatrixEW :: NattyEW n -> Matrix Int '(n, n)
> idMatrixEW  ZyEW    = Mat V0
> idMatrixEW (SyEW n) = Mat ((1 :> pure 0) :> ((0:>) <$> unMat (idMatrixEW n)))

for constructions like vlength : convenient to omit NATTY constraint from successor constructor

for eliminations like idMatrix : convenient to attach NATTY constraint to successor constructor

hard to predict which one is "best"
the issue with elimination happens only when we have the explicit witness but need the implicit one

also a time/space trade-off
including the constraint requires storing same information twice at each node
but enables an impl of natter by one step of case analysis, rather than a full recursion.

> natterEW :: NattyEW n -> (NATTY n => t) -> t
> natterEW  ZyEW    t = t
> natterEW (SyEW _) t = t

authors experience suggests omitting the constraint is more convenient more of the time

------------------------------------------------------------------------------

> test :: IO Counts
> test =
>   runTestTT $ TestList $ t1 ++ t2 ++ t3 ++ t4 ++ t5 ++ t6 ++ t7
