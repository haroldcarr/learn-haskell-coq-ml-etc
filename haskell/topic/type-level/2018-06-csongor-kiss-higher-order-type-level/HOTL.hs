{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

-- https://www.imperial.ac.uk/media/imperial-college/faculty-of-engineering/computing/public/ug-prizes-201718/Csongor-Kiss-Higher-order-type-level-programming-in-Haskell.pdf

module HOTL where

import           Data.Text as T

data Nat
  = Zero
  | Succ Nat

data Vector :: Nat -> * -> * where                  -- GADTs, KindSignatures
  VNil  ::                    Vector  'Zero    a    -- DataKinds
  VCons :: a -> Vector n a -> Vector ('Succ n) a

safeHead :: Vector ('Succ n) a -> a
safeHead (VCons x _xs) = x

sh :: Integer
sh = safeHead (VCons 1 (VCons 2 VNil))
-- xx = safeHead VNil

-- TypeFamilies

type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add  'Zero    b =              b
  Add ('Succ a) b = 'Succ (Add a b)

vappend :: Vector n a -> Vector m a -> Vector (Add n m) a
vappend  VNil        v =                     v
vappend (VCons a as) v = VCons a (vappend as v)

-- value-level functions can be curried/partially applied, type families can NOT

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

-- Chakravarty et al. [2005] : TYPE EQUALITIES

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

-- 2.4 Type functions are first-order : type families cannot take other type families as arguments
