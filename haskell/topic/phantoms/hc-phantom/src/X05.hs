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

fourCentimeters :: Length "centimeters"
fourCentimeters  = Tagged 4.0

add0 :: Length "inches"
     -> Length "centimeters"
     -> Length a
add0 (Tagged x) (Tagged y) = Tagged (x + y)

x1 :: Length a
x1  = add0 oneHalfInch fourCentimeters

x2 :: Length "ounces"
x2  = add0 oneHalfInch fourCentimeters

typeOfLengthCentimeters      :: TypeRep
typeOfLengthCentimeters       = typeOf fourCentimeters
typeOfProxyLengthCentimeters :: TypeRep
typeOfProxyLengthCentimeters  = typeRep (Proxy :: Proxy (Proxy (Length "centimeters")))

typeOfLengthInches           :: TypeRep
typeOfLengthInches            = typeOf oneHalfInch
typeOfProxyLengthInches      :: TypeRep
typeOfProxyLengthInches       = typeRep (Proxy :: Proxy (Proxy (Length "inches")))

add :: forall k (a :: k) (b :: k) (c :: k)
     . (Typeable a, Typeable b, Typeable c, Typeable k)
    => Length a
    -> Length b
    -> Proxy (Length c)
    -> Maybe (Length c)
add x y p = do
  Tagged x' <- toCentimeters x
  Tagged y' <- toCentimeters y
  let xy = x' + y'
  if | typeOf p == typeOfProxyLengthCentimeters -> Just (Tagged xy)
     | typeOf p == typeOfProxyLengthInches      -> Just (Tagged (xy/2.54))
     | otherwise -> Nothing

toCentimeters
  :: forall k (a :: k)
   . (Typeable a, Typeable k)
  => Length a
  -> Maybe (Length "centimeters")
toCentimeters la@(Tagged a) =
  if | typeOf la == typeOfLengthCentimeters ->
       cast la
     | typeOf la == typeOfLengthInches ->
       Just (Tagged (a * 2.54))
     | otherwise -> Nothing

ic = add oneHalfInch     fourCentimeters (Proxy::Proxy (Length "inches"))
cc = add fourCentimeters fourCentimeters (Proxy::Proxy (Length "centimeters"))

x05 :: Spec
x05  = describe "X05" $ do
  it "ic" $ ic `shouldBe` Just (Tagged 2.074803149606299)
  it "cc" $ cc `shouldBe` Just (Tagged 8.0)

