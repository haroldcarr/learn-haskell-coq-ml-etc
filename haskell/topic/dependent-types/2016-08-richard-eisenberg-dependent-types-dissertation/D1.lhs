> {-# LANGUAGE ConstraintKinds        #-}
> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE PolyKinds              #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeFamilies           #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE TypeSynonymInstances   #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module D1 where
>
> import qualified Data.Kind as K
> import qualified Data.List as L
> import qualified Data.Set  as S

p 21

TYPE CLASSES

> class Show' a where
>   show' :: a -> String
> instance Show' Bool where
>   show' True  = "True"
>   show' False = "False"
> instance Show' String where -- TypeSynonymInstances, FlexibleInstances
>   show' s     = s

CONSTRAINT

> smooshList :: Show' a => [a] -> String
> smooshList = concatMap show'

DICTIONARIES

type classes work by passing type class dictionaries
- dictionary is record containing all of methods defined in the type class. e.g.,

> newtype ShowDict a = MkShowDict {showMethod :: a -> String }
> showBool :: Bool -> String
> showBool True = "True"
> showBool False = "False"
> showDictBool :: ShowDict Bool
> showDictBool = MkShowDict showBool

whenever constraint Show' Bool must be satisfied
- GHC produces the showDictBool dictionary
- implicitly passes it as runtime argument to functions with Show' constraint

smooshList function actually takes two arguments
- the dictionary, and
- [a]

p 22

TYPE FAMILIES

A type family is a function on types.

CLOSED TYPE FAMILY
- all defining equations given in one place

> type family F1 a where -- TypeFamilies
>   F1 Int  = Bool
>   F1 Char = Double
>
> --       Bool      Double
> useF1 :: F1 Int -> F1 Char
> useF1 True  =  1.0
> useF1 False = -1.0

OPEN TYPE FAMILY
- defining equations can be extended
- interact well with type classes
- because open, can be extended whenever new class instance

> type family Element c
> class Collection c where
>   singleton :: Element c -> c
> type instance Element [a] = a
> instance Collection [a] where
>   singleton x = [x ]
> type instance Element (S.Set a) = a
> instance Collection (S.Set a) where
>   singleton = S.singleton

Because type families often extended in correspondence with a type class:

ASSOCIATED OPEN TYPE FAMILIES (syntax)

> class Collection' c where
>   type Element' c
>   singleton' :: Element' c -> c
> instance Collection' [a] where
>   type Element' [a] = a
>   singleton' x = [x ]
> instance Collection' (S.Set a) where
>   type Element' (S.Set a) = a
>   singleton' = S.singleton

p 23

PARTIALITY IN TYPE FAMILIES

type family may be partial (i.e., not defined over all inputs).
- no problem
- If type family used at type for which not defined, the type application is considered to be stuck. e.g.,

> type family F2 a
> type instance F2 Int = Bool

> -- this compiles
> useF2 :: F2 Char -> F2 Char
> useF2 x = x

-- this does not compile
callF2 :: F2 Char
callF2 = useF2 'c'
    * Couldn't match expected type ‘F2 Char’ with actual type ‘Char’

type F2 Char is stuck
- does not evaluate
- equal only to itself.
- impossible to detect if type is stuck
  - requires pattern-matching on type family application (not possible)
- stuck open type might become unstuck with inclusion of more modules
- stuckness is FRAGILE

p 24

DATA FAMILIES

data family defines a family of datatypes

data family Array a -- compact storage of elements of type a
data instance Array Bool = MkArrayBool ByteArray
data instance Array Int  = MkArrayInt (Vector Int)

Now can have different runtime representation for Array Bool than for Array Int
- not possible with traditional parameterized types.

RICH KINDS

KINDS IN HASKELL98

gain confidence in correctness of type-level programs by ensuring they are well-kinded.

e.g.,

x :: Element Maybe -> a
x = undefined
    * Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’

kind system keeps types in line, because type constructors may appear without their arguments. e.g.,


data Maybe a = Nothing | Just a

Maybe
- does not represent a type. It is a type constructor.  Needs to be given a type to become a type.
- Maybe Int, Maybe Bool are types

Kind : ARITY
- * : proper types (e.g., Int, Bool)
- * -> * : type constuctor

given

data Zero
data Succ a

could still write nonsense

Succ Bool
Maybe Zero

because they satisfy ARITY

p 25

PROMOTED DATATYPES

with DataKinds, given

> data Nat = Zero | Succ Nat

declare two entities
- type Nat  inhabited by terms (i.e., values) Zero and Succ Nat, and
- kind Bool inhabited by types ’Zero and ’Succ Nat

now

Succ Bool

not possible

more richly kinded type-level programming

> type family a + b where -- TypeOperators
>   'Zero + b = b         -- DataKinds
>   'Succ a + b = 'Succ (a + b)

’Succ ’Zero + ’Succ (’Succ ’Zero)    simplified to ’Succ (’Succ (’Succ ’Zero))

GHC does KIND INFERENCE on definition of type-level +.

Can also explicitly specify kinds

type family (a :: Nat) + (b :: Nat) :: Nat where ...

NOTE on TICK

ticks disambiguate a promoted data constructor ’X from a declared type X
GHC maintains separate type and term namespaces.
ticks are optional if no ambiguity

p 26

KIND POLYMORPHISM

allows kind variables to be held abstract, like done with type variables. e.g.,

> -- length of a type-level list at any kind
> type family Length (list :: [k]) :: Nat where -- PolyKinds
>   Length      '[]  = 'Zero
>   Length (x ': xs) = 'Succ (Length xs)

kind polymorphism extends to other constructs

> newtype T f a = MkT (f a)

With PolyKinds, GHC infers        :   ∀ k. (k → ?) → k → ? for T

Haskell98 would infer less general:        (? → ?) → ? → ?

kind-polymorphic type has extra, invisible parameters
- correspond to kind arguments
- invisible : arguments do not appear in source code

-fprint-explicit-kinds flag
- print kind parameters when they occur, e.g.,

T Maybe Bool     :    T * Maybe Bool   (making * kind parameter visible)

p 26

CONSTRAINT KINDS

make constraints first-class

can write kind of Show as : * -> Constraint
- e.g., Show Int is of kind Constraint
- Constraint is first-class kind
- can be quantified over.

useful construct over Constraints:

> data Some :: (* -> K.Constraint) -> * where
>   Some :: c a => a -> Some c  -- ConstraintKinds

a value of Some Show
- must store inside it a term of some (existentially quantified) type 'a'
  such that 'Show a'
- pattern-match against 'Some' constructor
  can use 'Show a' constraint   e.g.,

no 'Show a' constraint in signature
get constraint from pattern-matching on Some

> showSomething :: Some Show -> String
> showSomething (Some thing) = show thing
>
> ss = showSomething (Some "foo")
> si = showSomething (Some (1::Int))
>
> numThing :: Some Real -> Rational
> numThing (Some thing) = toRational thing
>
> ni = numThing (Some 1)
> nd = numThing (Some 2.0)

use-case : heterogeneous list where all elements satisfies a constraint

> heteroList :: [Some Show]
> heteroList = [Some True, Some (5 :: Int), Some (Just ())]
> printList :: [Some Show] -> String
> printList things = "[" ++ L.intercalate ", " (map showSomething things) ++ "]"

putStrLn $ printList heteroList
=> [True, 5, Just ()]

p 27

GADTs

enable term-level pattern matches to refine information about types

important GADT: PROPOSITIONAL EQUALITY

Data.Type.Equality:

~ : notation for type equality constraint

> data (a :: k) :~: (b :: k) where
>   Refl :: a :~: a

- value of type τ :~: σ (for some τ and σ)
- evidence that type τ is equal to type σ

use-case

> castWith :: (a :~: b) -- Refl
>          -> a         -- a
>          -> b
> castWith Refl a = a

takes
- term of type a :~: b : runtime evidence that a equals b
— term of type a
- can return given term: GHC knows a and b are same type
- castWith MUST pattern-match against Refl, because

p 28

alterate way to define makes why clearer (defined using Haskell98 datatype syntax):

data (a :: k) :~: (b :: k) where
  Refl :: (a ~ b) => a :~: b

- Refl constructor takes no arguments
- requires constraint a ~ b

to use Refl at type τ :~: σ
- GHC must know : τ ~ σ
- Refl is matched, constraint τ ~ σ available for use in body of pattern match.

castWith
- pattern-matching against Refl brings a ~ b into context
- GHC can apply this equality in the right-hand side of the equation to say that x has type b.
- match forces equality evidence to be reduced to a value (e.g., laziness)
- matching evaluates the argument (otherwise ⊥ could be hiding)

The fact that type equality evidence must exist and be executed at runtime is somewhat unfortunate.

HIGHER-RANK TYPES

Standard ML and Haskell98 use the Hindley-Milner (HM) type system.

HM type system allows only prenex quantification : type can quantify over type variables only at the very top.

system based
- types : no quantification
- type schemes : have quantification

    τ ::= α | H | τ1 τ2 types
    σ ::= ∀α.σ | τ type schemes

α : any of a countably infinite set of type variables
H : any type constant (including (->))

Let-bound definitions in HM are assigned type schemes

lambda-bound definitions are assigned monomorphic types, only

HM : OK: length :: ∀ a. [a] → Int

bad :: (∀ a. a → a → a) → Int
- bad’s type has a ∀ somewhere other than at the top of the type
- second rank
- forbidden in HM

GHC allows types of arbitrary rank, but cannot be inferred, must have type signature

> higherRank :: (forall a. a -> a) -> (Bool, Char) -- RankNTypes
> higherRank f = (f True, f 'x')

(type inference of higher-rank types can be done with bidirectional type-checking)

p 29

SCOPED TYPE VARIABLES

enables referring to a declared type variable from within the body of a function

  foldl :: (b -> a -> b) -> b -> [a] -> b
  foldl f z0 xs0 = lgo z0 xs0
   where
    lgo :: b -> [a] -> b  -- the type vars here are independent of foldl's
    -- as if b0 → [a0 ] → b0
    lgo z [ ] = z
    lgo z (x : xs) = lgo (f z x) xs

To make a and b in foldl’s signature be lexically scoped, explicitly quantify them.

> {-# ANN foldl  ("HLint: ignore Eta reduce" :: String) #-}
> {-# ANN module ("HLint: ignore Use foldl" :: String) #-}
> foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
> foldl f z0 xs0 = lgo z0 xs0
>  where
>   lgo :: b -> [a] -> b
>   lgo z      []  = z
>   lgo z (x : xs) = lgo (f z x) xs -- ScopedTypeVariables

p 30

FUNCTIONAL DEPENDENCIES

GHC’s earliest feature introduced to enable type-level programming.

Functional dependencies encode type-level programming through relations, not functions.

competitor to type families (functions that take arguments and produce result)

declares that choice of one parameter to a type class fixes the choice of another parameter

> class Pred (a :: Nat) (b :: Nat) | a -> b -- FunctionalDependencies (implies MultiParamTypeClasses)
> instance Pred  'Zero    'Zero
> instance Pred ('Succ n) n

parameter a determines b

instance declarations respect the functional dependency
- no two instances have same a but differing b

Functional dependencies are more powerful than type families (work of Stolarek et al. [86], attempts to close gap) e.g.,

> class Plus (a :: Nat) (b :: Nat) (r :: Nat) | a b -> r, r a -> b
> instance               Plus  'Zero    b  b
> instance Plus a b r => Plus ('Succ a) b ('Succ r) -- UndecidableInstances

- a and b determine r (like args to a type family)
- AND r and a determine b

knowing Plus a b r and Plus a b’ r : can conclude b = b’

(functional dependency r b -> a also holds, GHC unable to prove so cannot declare it)

