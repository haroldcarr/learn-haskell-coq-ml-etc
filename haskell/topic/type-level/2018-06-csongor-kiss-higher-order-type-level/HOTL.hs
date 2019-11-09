{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE TypeOperators     #-}

-- https://www.imperial.ac.uk/media/imperial-college/faculty-of-engineering/computing/public/ug-prizes-201718/Csongor-Kiss-Higher-order-type-level-programming-in-Haskell.pdf

module HOTL where

-- import           Data.Kind
import           Data.Text as T


-- p 7 : 1.2 Type-level properties

data Nat
  = Zero
  | Succ Nat

data Vector :: Nat -> * -> * where                  -- GADTs [Cheney and Hinze, 2003], KindSignatures
  VNil  ::                    Vector  'Zero    a    -- DataKinds  [Yorgey et al., 2012]
  VCons :: a -> Vector n a -> Vector ('Succ n) a

safeHead :: Vector ('Succ n) a -> a
safeHead (VCons x _xs) = x

sh :: Integer
sh = safeHead (VCons 1 (VCons 2 VNil))
-- xx = safeHead VNil

-- p 8 : 1.3 TypeFamilies [Chakravarty et al., 2005] [Eisen- berg and Stolarek, 2014]

type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add  'Zero    b =              b
  Add ('Succ a) b = 'Succ (Add a b)

vappend :: Vector n a -> Vector m a -> Vector (Add n m) a
vappend  VNil        v =                     v
vappend (VCons a as) v = VCons a (vappend as v)

-- value-level functions can be curried/partially applied, type families can NOT

-- p 9 :  1.4 Limitations

-- this def type checks, but cannot pass in an 'f' (because it would be partially applied)
type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map _      '[]  = '[]               -- TypeOperators
  Map f (x ': xs) = f x ': Map f xs

-- type families are required to be saturated : meaning must provide every argument at once
-- means type language is first-order

-- to map a fun over a list, must specialise
type family MapAddOne (xs :: [Nat ]) :: [Nat ] where
  MapAddOne      '[]  = '[]
  MapAddOne (x ': xs) = 'Succ x ': MapAddOne xs


-- p 12 : 2.1 Types and kind

data HCMaybe a = HCNothing | HCJust a

-- HCMaybe (* -> *) is a type constructor : it takes a type argument and returns a new type.
-- HCMaybe type constructor abstracts over types

-- abstracting over type constructors, e.g.,
-- Functor is higher-kinded: it abstracts over type constructors.

class HCFunctor (f :: * -> *) where
  hcfmap :: (a -> b) -> f a -> f b

-- Maybe is parameterised by 'a', which can be mapped over.

instance HCFunctor HCMaybe where
  hcfmap :: (a -> b) -> HCMaybe a -> HCMaybe b
  hcfmap _ HCNothing  = HCNothing
  hcfmap f (HCJust a) = HCJust (f a)

-- kind system rules:    κ ::= * | κ -> κ

-- p 14 : 2.2 Rich kinds

-- Kiselyov et al. [2004] : store info about collections in types (e.g., length of list)

-- no constructors : so not possible to construct a value
-- purpose is for type to exist at type level
data HZero
data HSucc n

-- but weak
-- :k HSucc
-- HSucc :: k -> *
-- so allows:

xx :: HSucc Bool
xx = undefined

-- but following ruled out by DataKinds
-- xxx :: 'Succ Bool
-- xxx = undefined

-- types classify values
-- kinds classify types
-- how are kinds classified? What is the "type" of Nat, or the "type" of *
-- Martin-Lof [1984] : infinite universe hierarchy : U0 ∈ U1 ∈ U3 ∈ ...
-- simplification : Weirich et al. [2013] : introduced * :: * (Type in Type”) axiom into type system
-- - collapses infinite universes into one,
-- - introduces inconsistency into type system (e.g., Russell’s paradox)

-- p 15 : 2.3 Equality Chakravarty et al. [2005] : TYPE EQUALITIES

-- introduced into the equational theory as axioms by type family declarations, e.g.,

-- | type family that relates containers with the type of elements they contain
type family Elem c

-- | generic functions that operate over collections and their elements:
class Collection c where
  empty  :: c
  insert :: Elem c -> c -> c

type instance Elem [a] = a
instance Collection [a] where
  empty  = []
  insert = (:)

type instance Elem Text = Char
instance Collection Text where
  empty  = ""
  insert = T.cons

-- equations 'type instance Elem [a] = a', etc.,
-- introduce axioms into the equational theory

-- equality of types in Haskell is written with the '∼' symbol  : Elem [a] ∼ a
-- :t insert 'c'
-- >             :: (Collection c, Elem c ~ Char) => c -> c

-- to specify specific type (via TypeApplications) -- Eisenberg et al. [2016]
-- to provide types as inputs to functions
-- :t insert @String
-- >                 :: Char -> String -> String

-- a type family is indexed by type arguments
-- it encodes a family of rewrite rules that depend on args
-- Eisenberg et al. [2014b] extended type families : ability to define a closed set of overlapping equations

type family Equals (a :: k) (b :: k) :: Bool where
  Equals a a = 'True
  Equals a b = 'False

-- can take polymorphically kinded arguments (here, parameterised by kind variable k)
-- non-linear patterns: 1st equation uses same var twice; matches only when the two args are equal
-- equations can overlap: last equation matches any combination of types
-- - iff they are of the same kind
-- - since closed (all equations are provided at the same time, indicated by the 'where')
--   equations can overlap; tried in top-to-bottom order

-- can be recursive (see 'Add' above)
-- Add introduces two axioms:
--    Add  'Zero    b ∼              b
--    Add ('Succ a) b ∼ 'Succ (Add a b)
-- :kind! Add ('Succ 'Zero) ('Succ 'Zero)
-- > = 'Succ ('Succ 'Zero)

-- p 17 : 2.4 Type functions are first-order : type families cannot take other type families as arguments

{-
therefore the type language a first-order system

reason for this restriction: type inference engine

when dealing with an equality between type applications
checker decomposes
-}
replaceWith10 :: Functor f => f a -> f Int
replaceWith10 fa = fmap (\_ -> 10) fa
{-
say called : replaceWith10 (Just False)

solve equality constraint f a ∼ Maybe Bool

decomposes to : f ∼ Maybe
                a ∼ Bool
via substitution map [f -> Maybe, a -> Bool]
then type inference is done   replaceWith10 :: Maybe Bool -> Maybe Int
-}

{-
consider
-}
decompose :: (f a ~ g b) => a -> b
decompose = id -- in/out the same
{-
f a ∼ g b decomposes to f ∼ g; a ∼ b
f ~ g discarded since not used
a ∼ b used to refine result : a -> a
-}

{-
now show that allowing type families to be unsaturated violates type safety
-}

-- | discards its arg; always returns Int
type family AlwaysInt a where AlwaysInt _ = Int

{-
bad :: Integer -> Char
bad = decompose @AlwaysInt @Integer @AlwaysInt @Char

type of bad is (AlwaysInt Integer ∼ AlwaysInt Char) => Integer -> Char

equality constraint normalises to Int ∼ Int

if allowed, would mean id would need to coerce any Integer into any Char

problem :
- trying to substitute a non-injective function for a var that GHC assumes to be injective (and generative)

INJECTIVITY  : f is injective  <==> f a ∼ f b ==> a ∼ b
GENERATIVITY : f is generative <==> f a ∼ g b ==> f ∼ g
MATCHABILITY : f is matchable  <==> f is injective and generative

type families generally not injective nor generative

for type system to decompose f a ∼ g b to f ∼ g; a ∼ b
- ensure neither f nor g can be instantiated with type families
- achieved by enforcing type families to be fully saturated, so they can reduce
-}

-- p 19 : 2.5 Singletons Eisenberg and Weirich [2012]

{-
differentiates type functions and type constructors at kind level
via defunctionalisation using code generation generation [Sheard and Peyton Jones, 2002]

defunctionalisation
- eliminates higher-order functions
- replaces them with a first-order apply function
- higher-order function calls replaced by unique identifier that represents that function call
- then Apply function dispatches on the unique identifier

e.g.,
-}

type TyFun a b = a -> b -> *
infixr 0 `TyFun`
data SymConst2         :: * `TyFun` * `TyFun` *
data SymConst (n :: *) :: * `TyFun` *
data SymId             :: * `TyFun` *

