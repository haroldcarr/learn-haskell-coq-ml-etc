{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

module X05 where

import           Data.Proxy
import           Data.Tagged
import           Data.Typeable
import           Protolude
import           Test.Hspec

type Length u = Tagged u Double

oneHalfInch    :: Length "inches"
oneHalfInch     = Tagged 0.5

twoCentimeters :: Length "centimeters"
twoCentimeters  = Tagged 2.0

add0 :: Length "inches"
     -> Length "centimeters"
     -> Length a
add0 (Tagged x) (Tagged y) = Tagged (x + y)

x1 :: Length a
x1  = add0 oneHalfInch twoCentimeters

x2 :: Length "ounces"
x2  = add0 oneHalfInch twoCentimeters

typeOfLengthCentimeters      :: TypeRep
typeOfLengthCentimeters       = typeOf twoCentimeters
typeOfProxyLengthCentimeters :: TypeRep
typeOfProxyLengthCentimeters  = typeOf (Proxy::Proxy (Length "centimeters"))

typeOfLengthInches           :: TypeRep
typeOfLengthInches            = typeOf oneHalfInch
typeOfProxyLengthInches      :: TypeRep
typeOfProxyLengthInches       = typeOf (Proxy::Proxy (Length "inches"))

add :: forall k (a :: k) (b :: k) (c :: k)
     . (Typeable a, Typeable b, Typeable c, Typeable k)
    => Length a
    -> Length b
    -> Proxy (Length c)
    -> Maybe (Length c)
add x y p = do
  Tagged x' <- canonical x
  Tagged y' <- canonical y
  let xy = x' + y'
  if | typeOf p == typeOfProxyLengthCentimeters -> Just (Tagged xy)
     | typeOf p == typeOfProxyLengthInches      -> Just (Tagged (xy/2.54))
     | otherwise -> Nothing

canonical
  :: forall k (a :: k)
   . (Typeable a, Typeable k)
  => Length a
  -> Maybe (Length "centimeters")
canonical la@(Tagged a) =
  if | typeOf la == typeOfLengthCentimeters ->
       cast la
     | typeOf la == typeOfLengthInches ->
       Just (Tagged (a/2.54))
     | otherwise -> Nothing

x3  = add oneHalfInch    twoCentimeters (Proxy::Proxy (Length "inches"))
x4  = add twoCentimeters twoCentimeters (Proxy::Proxy (Length "centimeters"))

x05 :: Spec
x05  = describe "X05" $ do
  it "x3" $ x3 `shouldBe` Just (Tagged 0.8649017298034596)
  it "x4" $ x4 `shouldBe` Just (Tagged 4.0)

