{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module X03 where

import           GHC.TypeLits
import           Protolude
import           Test.Hspec

-- http://dev.stephendiehl.com/hask/#typelevel-strings

{-
Typelevel Strings

Since GHC 8.0.

Typelevel strings : Symbol with kind Symbol.

GHC.TypeLits : typeclases for
- lifting to/from value level
- comparing/computing at typelevel

symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String

type family AppendSymbol (m :: Symbol) (n :: Symbol) :: Symbol

type family CmpSymbol (m :: Symbol) (n :: Symbol) :: Ordering

sameSymbol :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Maybe (a :~: b)

Used to add compile-time info encoded in strings.

Example : attach units to numerical quantities and perform dimensional analysis.
-}

newtype Tagged (l :: Symbol) a = Tagged a deriving (Eq, Show)

m :: Tagged "m" Double
m  = Tagged 10.0

s :: Tagged "s" Double
s  = Tagged 20.0

addUnits
  :: Num a
  => Tagged u1 a
  -> Tagged u2 a
  -> Tagged u1 a
addUnits (Tagged x) (Tagged y) = Tagged (x + y)

x03 :: Spec
x03  = describe "X03" $ do
  it "   au m s" $
    addUnits m s `shouldBe`
    Tagged 30.0
  it "   au s m" $
    addUnits s m `shouldBe`
    Tagged 30.0

addMS :: Tagged "m" Double -> Tagged "s" Double -> Tagged "m" Double
addSM :: Tagged "s" Double -> Tagged "m" Double -> Tagged "s" Double
addMS  = addUnits
addSM  = addUnits

divUnits
  :: Fractional a
  => Tagged u1 a
  -> Tagged u2 a
  -> Tagged (u1 `AppendSymbol` u2) a
divUnits (Tagged x) (Tagged y) = Tagged (x / y)

divMS :: Tagged "m" Double -> Tagged "s" Double -> Tagged "ms" Double
divSM :: Tagged "s" Double -> Tagged "m" Double -> Tagged "sm" Double
divMS  = divUnits
divSM  = divUnits

{-
-- sum :: (Foldable t, Num a) => t a -> a
-- No instance for (Num (Tagged "m" Double))
xx :: [Tagged "m" Double] -> Tagged "m" Double
xx  = sum
-}
