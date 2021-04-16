{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}

module Part2_Agda_in_a_Nutshell where

import           Data.Kind (Type)

data Writers    = Wilde | Shelley | Byron | Sartre | Camus
data Literature = DorianGrey | Alastor | ChildeHarold | LaNausée | LEtranger

data ℕ = Zero | Succ ℕ

data Vec :: Type -> ℕ -> Type where
  Nil  ::                 Vec a  'Zero
  Cons :: a -> Vec a n -> Vec a ('Succ n)

data Fin :: ℕ -> Type where
  Fzero :: forall (n :: ℕ).          Fin ('Succ n)
  Fsucc :: forall  n      . Fin n -> Fin ('Succ n)

-- Vec and Fin in Haskell are not dependent types in a strict sense,
-- because forall (n :: ℕ) in Haskell does not introduce a value,
-- therefore Fin does not depend on any values.

-- For an introduction to type level programming in Haskell, see
-- https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html

const₁ :: forall a b. a -> b -> a
const₁ x _y = x

-- Haskell doesn't have dependent quantification at term level, but does have it at type level.
-- So implement comparable functions as closed type families.
-- Similarly, promote values of type Writers into the type level via DataKinds.

data Ireland = Dublin
data France  = Paris
data England = London

type family WriterToCountry (a :: Writers) :: Type where
  WriterToCountry 'Wilde   = Ireland
  WriterToCountry 'Shelley = England
  WriterToCountry 'Byron   = England
  WriterToCountry 'Sartre  = France
  WriterToCountry 'Camus   = France

-- This function can be implemented via PolyKinds since GHC 8.6.
type family WriterToCity (a :: Writers) :: WriterToCountry a where
  WriterToCity    'Wilde   = 'Dublin
  WriterToCity    'Shelley = 'London
  WriterToCity    'Byron   = 'London
  WriterToCity    'Camus   = 'Paris
  WriterToCity    'Sartre  = 'Paris

s :: forall a b c. (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

type family S₂ (f :: a -> b -> c) (g :: a -> b) (x :: a) :: c where
  S₂ f g x = f x (g x)

data Person = Person
  { name    :: String
  , country :: String
  , age     :: Int
  }

-- Propositional equality in Haskell

-- Unlike Agda, this datatype is defined on types of arbitrary kinds, not on terms.
-- In other words, (:~:) :: k -> k -> *, where k is an arbitrary kind.
data a :~: b where
  Refl :: a :~: a






