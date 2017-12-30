> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module S2_programming_with_singletons where
>
> import Data.Singletons.TH
> import qualified GHC.Types as T (Type)

https://github.com/goldfirere/singletons
https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf

blue  : code from singletons library; code generated through the use of the
red   : code generated via singletons library
black : other

2  programming with singletons

Definitions of the singletons library:

data family Sing (a :: κ)

class SingI (a :: κ) where
  sing ::Sing a

class SingE (a :: κ) where
  type Demote a :: ∗
  fromSing ::Sing a → Demote (Any :: κ)

class (SingI a,SingE a) ⇒ SingRep a
instance (SingI a,SingE a) ⇒ SingRep a

data SingInstance (a :: κ) where
  SingInstance ::SingRep a ⇒ SingInstance a

class (t ∼ Any) ⇒ SingKind (t :: κ) where
  singInstance :: ∀ (a :: κ). Sing a → SingInstance a

2.1 indexed datatypes

 $(singletons
   [d|
     data Nat = Zero | Succ Nat deriving (Show)
   |])

or

> data Nat = Zero | Succ Nat deriving (Show)
> $(genSingletons [''Nat])

> data Vec :: T.Type -> Nat -> T.Type where
>   VNil  :: Vec a 'Zero
>   VCons :: a -> Vec a n -> Vec a ('Succ n)

'Vec' definition requires GADTs because
- constructors VNil and VCons do not treat second type argument (of kind Nat) uniformly
- VNil  : know empty vectors have zero length : constructor produces Vec a 'Zero
- VCons : increments statically-tracked length

non-uniform treatment means pattern matching increases the knowledge of the type system:

> head :: Vec a ('Succ n) -> a
> head (VCons h _) = h

impossible to be called with VNil

  head VNil = undefined
    * Couldn't match type ‘'Succ n’ with ‘'Zero’

GHC detects that VNil case could never occur.
- when checking GHC would derive : Vec a (’Succ n) ∼ Vec a ’Zero (because type of pattern must match type of argument)
- equality can NOT hold, therefore compile-time error

2.2 type-level computation

:<  is type-level function
used in nth

can represent using a multiparamter type class

situations where type-level computation is essential: append length-indexed vectors

FUNCTION PROMOTION

singletons supports automatic reuse of runtime functions at the type-level, through function promotion.

define plus term function, and type family function

> $(promote [d|
>   plus :: Nat -> Nat -> Nat
>   plus  Zero m = m
>   plus (Succ n) m = Succ (plus n m) |])

generates

type family Plus (n ::Nat) (m ::Nat)::Nat
type instance Plus ’Zero m = m
type instance Plus (’Succ n) m = ’Succ (Plus n m)

> vappend :: Vec a n -> Vec a m -> Vec a (Plus n m)
> vappend  VNil       v = v
> vappend (VCons h t) v = VCons h (vappend t v)

2.3 Singleton datatypes

genSingletons generates singleton definitions from datatypes already defined.

> data MyEnum = X | Y
> $(genSingletons [''MyEnum])

double-quote : tells Template Haskell the name of a type.
single quote : followed by capitalized identifier : indicates a data constructor.

library uses kind-indexed data family, 'Sing', to provide common name for all singleton types.

  data family Sing (a :: κ)

Each instance in family has its own set of data constructors, but family shares ONE type constructor.
- data constructors for a datatype are determined by parameters to the data family.

kind-indexed type family can branch on kind of its argument, not just type, and
the constructors of a kind-indexed data family are determined by kind arguments as well as type arguments to data constructor.

above generates:

 data instance Sing (a :: MyEnum) where
   SX :: Sing 'X
   SY :: Sing 'Y

If Nat was defined normally then given to genSingletons:

 data instance Sing (a :: Nat) where
   SZero :: Sing 'Zero
   SSucc :: SingRep n => Sing n -> Sing ('Succ n)

each constructor in datatype produces constructor in singleton type (prepended with S)

Symbolic names (operators) prepended with :%.

library user’s responsibility to avoid name clashes.

library also produces synonyms to Sing to enforce the kind of a type argument.
Synonyms are the datatype names prepended with an S

 type SNat  (a :: Nat)  = Sing a
 type SBool (a :: Bool) = Sing a

Use synonyms in type signatures when kind of type parameter is known.
- synonym instead of Sing adds documentation and kind-checking.

2.4 singleton functions

> $(singletons [d|
>   isEven :: Nat -> Bool
>   isEven        Zero     = True
>   isEven (Succ  Zero)    = False
>   isEven (Succ (Succ n)) = isEven n
>   nextEven :: Nat -> Nat
>   nextEven n = if isEven n then n else Succ n |])

generates
- promoted version (as a type family)
- runtime version that works with singleton types
- name is original name, prepended with an s and next letter capitalized.

  sIsEven :: Sing n -> Sing (IsEven n)
  sIsEven         SZero     = sTrue
  sIsEven (SSucc  SZero)    = sFalse
  sIsEven (SSucc (SSucc n)) = sIsEven n
  sNextEven :: Sing n -> Sing (NextEven n)
  sNextEven n = sIf (sIsEven n) n (sSucc n)

> makeEven ::SNat n -> Vec a n -> Vec a (NextEven n)
> makeEven n vec = case sIsEven n of
>  STrue -> vec
>  SFalse -> case vec of
>    VCons h t -> VCons h (VCons h t)

Pattern matching on result of sIsEven brings info about n into context so cases in pattern match have expected type.

library provides 'genPromotions' and 'promote'
- convert term level declarations into type-level declarations only.

Generating singletons requires promoting first
- most users will use only the genSingletons and singletons functions.
- See Section 3.3 for more details on singleton conversion for functions.

2.5 forgetting static information

'fromSing' eliminates a singleton term and gives back a term of the unrefined datatype.
- witnesses one direction of isomorphism between members of singleton type family and unrefined version of type.

> vtake :: Nat -> Vec a n -> [a]
> vtake = undefined

To call vtake with SNat n value, need to convert to  Nat. fromSing does that.

> vtake' :: (i :<= n) ~ 'True => SNat i -> Vec a n -> [a]
> vtake' i = vtake (fromSing i)

  class SingE (a :: κ) where
    type Demote a :: ∗
    fromSing ::Sing a -> Demote (Any :: κ)

Demote associated kind-indexed type family : returns type from which a kind was promoted.

  instance SingE (a :: Nat) where
    type Demote a      = Nat
    fromSing  SZero    = Zero
    fromSing (SSucc n) = Succ (fromSing n)
  instance SingE (a :: Maybe κ) where
    type Demote a      = Maybe (Demote (Any :: κ))
    fromSing SNothing  = Nothing
    fromSing (SJust a) = Just (fromSing a)

Ideally, write Demote with only explicit kind parameter.  Not yet supported in GHC.
Instead, Demote takes type parameter a and its kind κ, and it branches only on its kind parameter κ.

To write Maybe instance, need to supply recursive call to Demote with some type of kind κ.
- use Any type, GHC primitive that is an inhabitant of every kind.

Demote, provides an exact solution to our problem: use Any with an explicit kind signature to get the recursive Demote call to work.
Because use Any, also necessary to use Any in type signature for fromSing;
otherwise type checker tries to unify Demote (a::κ) with Demote (Any ::κ).
Using the knowledge that the type parameter is irrelevant, see that these two types clearly unify,
but compiler does not have that specialized knowledge and issues an error.

2.6 implicit arguments

runtime singleton arguments determined by compile-time type inference.

> -- explicit
> replicate1 :: SNat n -> a -> Vec a n
> replicate1  SZero    _ = VNil
> replicate1 (SSucc n) a = VCons a (replicate1 n a)

implicit arguments via

  class SingI (a :: κ) where
    sing :: Sing a

contains singleton value in its dictionary: available at runtime

  TODO : does not type check

 replicate2 :: forall a n. SingI n => a -> Vec a n
 replicate2 a = case (sing :: Sing n) of
   SZero   -> VNil
   SSucc _ -> VCons a (replicate2 a)

SingI instances automatically generated along with the singleton type definitions

  $(genSingletons [’’Nat])

generates

  instance SingI ’Zero where
    sing = SZero
  instance SingRep n ⇒ SingI (’Succ n) where
    sing = SSucc sing

SingRep class

SingI / SingE classes
- kept separate
- because possible to define SingE instances on a datatype-by-datatype basis
- SingI instances must be defined per constructor

often convenient to combine these two classes

SingRep class is a essentially a synonym for combination of SingI and SingE.

So unnecessary for singletons library to generate instances for it.
All parameters to singleton type constructors have a SingRep constraint
enabling programmer to use sing and fromSing after pattern matching with these constructors.

  class    (SingI a, SingE a) ⇒ SingRep a
  instance (SingI a, SingE a) ⇒ SingRep a

2.7 explicitly providing implicit arguments

when in a context where
- have value of type 'SNat n'
- but no dictionary for 'SingI n'
- want to call replicate2

current solution is SingKind class
- defined over kind
- provide necessary instance of SingI
- intuition : SingKind is class of kinds that have singletons associated with them


  class SingKind (κ :: <X>) where ...

where  <X> is the sort of kinds
- informs compiler that κ is a kind variable, not a type variable.

not valid Haskell. workaround:

  class (t ∼ Any) ⇒ SingKind (t :: κ) where
    singInstance :: ∀ (a :: κ). Sing a → SingInstance a

Any (again, see above) used to pin down the value of the t type variable, indicating that only κ matters.

singInstance returns a term of type SingInstance
- that stores dictionaries for SingI and SingE.

  data SingInstance (a :: κ) where
    SingInstance ::SingRep a ⇒ SingInstance a

generated instance of SingKind for Nat:

  instance SingKind (Any ::Nat) where
    singInstance  SZero   = SingInstance
    singInstance (SSucc ) = SingInstance

using SingKind

mkTrueList :: SNat n -> Vec Bool n
mkTrueList n = case singInstance n of
  SingInstance -> replicate2 True
