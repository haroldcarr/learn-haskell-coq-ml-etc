> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE GADTs               #-}
> {-# LANGUAGE KindSignatures      #-}
> {-# LANGUAGE RankNTypes          #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving  #-}
> {-# LANGUAGE TypeOperators       #-}
>
> module TL where
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

https://github.com/kosmikus/SSGEP/
https://github.com/kosmikus/SSGEP/raw/master/LectureNotes.pdf

since type-level programming is peculiar in Haskell, we’ll move step by step
- normal lists over length-indexed vectors
- heterogeneous lists
- generalized n-ary products

1.2 Kinds and data kinds

layered type system
- types defined using a more limited language than terms
- omission : no type level lambda abstraction
- types
  - are structured
  - can apply types to one another
  - can parameterize named types
- types need their own types : called kinds

1.2.1 Stars and functions

kind *
- for types that classify terms (not every type of kind * need be inhabited)
- a fully applied 'data' type is of kind *
- kind * is open (new members can be added via 'data' at any time)
- if 'k' and 'l' are kinds: 'k -> l'
  indicates types of kind l parameterized over types of kind k.

examples
- *           : Int, Double, Bool, Char, Maybe Int, [Double], IO Bool
- * -> *      : Maybe, [], IO are of kind * -> *
- * -> * -> * : Either, (, )

- may be partially applied : Either Int is of kind * -> *
- kind system says that
  - Either (Maybe [Int]) (Char, Bool) : well-formed
  - (Maybe, IO) : not

1.2.2 Promoted data kinds

can define new kinds ourselves, via {-# LANGUAGE DataKinds #-}

when defining a new datatype, can also use it as a kind.

Example:

> data Bool0 = False0 | True0

defines
- datatype Bool
- data constructors :  False0 ::  Bool0,  True0 ::  Bool0
- kind 'Bool
- type constructors : ’False0 :: 'Bool0, ’True0 :: 'Bool0

quotes emphasize the promotion (sometimes required to resolve ambiguity)

- ’False is a type
- but NO terms/values of type ’False
- kind * classifies values
- new kinds via promotion contain types that are uninhabited
- useful as
  - args/results of type-level functions
  - parameters of datatypes and classes

1.3 Generalized algebraic data types (GADTs)

goal : heterogeneous lists to represent sequences of args to data constructors

do so by generalizing normal Haskell lists in several steps

1.3.1 Vectors

first step : length-indexed lists, aka vectors

> data Nat = Zero | Suc Nat
>
> --    KindSignatures         GADTs
> --        v                  v
> data Vec (a :: *) (n :: Nat) where
>   VNil  ::                 Vec a  'Zero
>   VCons :: a -> Vec a n -> Vec a ('Suc n)
> infixr 5 `VCons`
> -- StandaloneDeriving
> deriving instance Show a => Show (Vec a n)
> deriving instance Eq   a => Eq   (Vec a n)

defines Vec :: * -> Nat -> * with two constructors
- GADTs enable restricting/specifying the result type

Nat is used as an index (note: no inhabitants of types of kind Nat)

> type Three = 'Suc ('Suc ('Suc 'Zero)) -- can be inferred
> vabc :: Vec Char Three
> vabc = 'a' `VCons` 'b' `VCons` 'c' `VCons` VNil

GADTs allow finer-grained pattern matching

> -- | type requires non-empty vector
> -- so do not have to match on Nil (doing so would not be type-correct)
> vtail :: Vec a ('Suc n) -> Vec a n
> vtail (VCons _ xs) = xs
>
> vmap :: (a -> b) -> Vec a n -> Vec b n
> vmap _ VNil = VNil -- for this case n ~ 'Zero (VCons usage here would cause type err)
> vmap f (VCons x xs) = f x `VCons` vmap f xs -- n ~ Suc n’ for some n’

because vmap preserves the length
vmap f xs :: Vec a n’ and VCons-ing one element will yield a vector of length n again.

1.3.2 Functions on vectors

> -- | creates a list with n copies of x
> replicate0 :: Int -> a -> [a]
> replicate0 n x
>  | n <= 0 = []
>  | otherwise = x : replicate0 (n - 1) x

something similar for vectors

size of vector is statically known
- assume number of copies is also known at type level
- how do we pass it
- cannot use Int : lives at term level

Maybe NO arg

    vreplicate :: a -> Vec a n

Can not do

type says function is parametric in n : i.e., that n is not needed
- have to make compiletime distinction based on type n

OPTION: use type class (type classes extend to the presence of data kinds):

> class VReplicateC (n :: Nat) where
>   vreplicateC :: a -> Vec a n
> instance VReplicateC 'Zero where
>   vreplicateC _ = VNil
> instance VReplicateC n => VReplicateC ('Suc n) where
>   vreplicateC x = x `VCons` vreplicateC x

> t1 :: [Test]
> t1 = U.t "t1"
>   (vreplicateC 'x' :: Vec Char Three)
>   (VCons 'x' (VCons 'x' (VCons 'x' VNil)))

disadvantage to doing this via type classes
- requires defining new type classes for each wanted function
- class constraints will appear in types of functions that use them
  - leak impl info
- changes ripple throughout program

1.4 Singleton types

1.4.1 Singleton natural numbers

create a value that does nothing more than help GHC to know value of n

create 'GADT SNat n' such that 'SNat n' has ONE value for each n
then pattern matching on 'SNat n' to learn 'n' at runtime

usual singleton definition

> data SNat0 (n :: Nat) where -- preliminary
>   SZero0 :: SNat0 'Zero
>   SSuc0  :: SNat0 n -> SNat0 ('Suc n)
>
> sThree :: SNat0 Three
> sThree = SSuc0 (SSuc0 (SSuc0 SZero0))

> vReplicate :: SNat0 n -> a -> Vec a n
> vReplicate SZero0      _ = VNil
> vReplicate (SSuc0 n') x = VCons x (vReplicate n' x)
>
> t2 :: [Test]
> t2 = U.t "t2"
>    (vReplicate (SSuc0 (SSuc0 (SSuc0 SZero0))) "hi")
>    ("hi" `VCons` "hi" `VCons` "hi" `VCons` VNil)

make num arg to vReplicate implicit see eisenberg:
https://typesandkinds.wordpress.com/tag/singletons/

using different mutually recursive definition

because (from Andres via twitter)
- matter of taste
- using a "typical singleton"
  - usually need two versions of every function
    - one with explicit singleton
    - one wrapper to make it implicit
- with following definition, only need one

> data SNat (n :: Nat) where
>   SZero :: SNat 'Zero
>   SSuc  :: SNatI n => SNat ('Suc n)
> class SNatI (n :: Nat) where -- 'I' means "Implicit"
>   sNat  :: SNat n
> instance SNatI 'Zero where
>   sNat = SZero
> instance SNatI n => SNatI ('Suc n) where
>   sNat = SSuc

now can define with implicit num arg:

> --          RankNTypes (or ExistentialQuantification)
> --              v
> vreplicate :: forall a n
>             . SNatI n
>            => a
>            -> Vec a n
> --                  ScopedTypeVariables
> --                  v
> vreplicate x = case sNat :: SNat n of
>   SZero -> VNil
>   SSuc  -> x `VCons` vreplicate x

> t3 :: [Test]
> t3 = U.t "t3"
>   (vreplicateC 'x' :: Vec Char Three)
>   (VCons 'x' (VCons 'x' (VCons 'x' VNil)))


1.4.2 Evaluation

singletons approach (compared to  one class per function solution)
- advantages
  - one class per type used in pattern matching
    - do not need the class, if willing to pass singleton explicitly
  - more interface stability
  - exposes less of the impl
- disadvantage
  - type class resolution happens at compile time
    - value of type SNat is a runtime value
    - pattern matching on it happens at run-time
    - singleton vreplicate does unnecessary runtime work
      - case distinctions for which outcome was already known at compile time

choice
- type class
  - compiletime resolution (usually more efficient, but can produce lots of code)
- singleton
  - runtime resolution (usually does unnecessary work, but can be more compact).

1.4.3 Applicative vectors

with vreplicate, vectors fit the interface of an applicative functor:

class Functor f => Applicative f where
  pure ::a -> fa
  (<*>) :: f (a -> b) -> f a -> f b

one possible instance for lists is where
- pure produces an infinite list of copies
- (<*>) zips together the list, applying list of functions pointwise to list of args

same idea with vectors
- vreplicate plays the role of pure
- vapply is like (<*>)

> vapply :: Vec (a -> b) n -> Vec a n -> Vec b n
> vapply VNil VNil = VNil
> vapply (f `VCons` fs) (x `VCons` xs) = f x `VCons` vapply fs xs

cannot make Vec an instance of Applicative class: Vec params are in the wrong order

------------------------------------------------------------------------------

1.5 Heterogeneous lists

like a vector
- list-like structure with a statically known length
- types of elements can all be different
- require all element types to be statically known
  - indexing by Nat not enough
  - will index by a list of types

1.5.1 Promoted lists and kind polymorphism

term-level lists
- type constructor [] ; kind * -> *
- constructors
  - []  :: [a]
  - (:) :: a -> [a] -> [a]

Can promote lists and treat [] as a kind constructor, and [] and (:) as types.

Note : even seen as a type constructor, ’(:) has kind ’(:) :: a -> [a] -> [a]
- means it is kind-polymorphic

type-level list of promoted Booleans

   :k ['True, 'False]
   -- >               :: [Bool]
   :k ['Zero, Three]
   -- >               :: [Nat]

want
   :k [Char, Bool, Int]
   -- >                  [*]

> data HList (xs :: [*]) where
>   HNil :: HList '[]
>   --                            TypeOperators
>   --                                 v
>   HCons :: x -> HList xs -> HList (x ': xs)
> infixr 5 `HCons`

> group :: HList '[Char, Bool, Int]
> group = 'x' `HCons` False `HCons` 3 `HCons` HNil

------------------------------------------------------------------------------

> test :: IO Counts
> test  =
>     runTestTT $ TestList $ t1 ++ t2 ++ t2
