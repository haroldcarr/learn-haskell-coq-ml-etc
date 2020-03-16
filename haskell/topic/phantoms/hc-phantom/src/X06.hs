{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module X06 where

import           Data.Proxy
import           Data.Typeable
import           Protolude
import           Test.Hspec

data Length u where
  Centimeters :: Double -> Length "centimeters"
  Inches      :: Double -> Length "inches"
deriving instance Eq   (Length u)
deriving instance Show (Length u)

oneHalfInch    :: Length "inches"
oneHalfInch     = Inches 0.5

twoCentimeters :: Length "centimeters"
twoCentimeters  = Centimeters 2.0
{-
add0 :: Length "inches"
     -> Length "centimeters"
     -> Length a
add0 (Inches x) (Centimeters y) = Centimeters (x + y) -- Tagged (x + y)

x1 :: Length a
x1  = add0 oneHalfInch twoCentimeters

x2 :: Length "ounces"
x2  = add0 oneHalfInch twoCentimeters
-}
typeOfLengthCentimeters      :: TypeRep
typeOfLengthCentimeters       = typeOf twoCentimeters
typeOfProxyLengthCentimeters :: TypeRep
typeOfProxyLengthCentimeters  = typeRep (Proxy :: Proxy (Proxy (Length "centimeters")))

typeOfLengthInches           :: TypeRep
typeOfLengthInches            = typeOf oneHalfInch
typeOfProxyLengthInches      :: TypeRep
typeOfProxyLengthInches       = typeRep (Proxy :: Proxy (Proxy (Length "inches")))

add :: forall (a :: Symbol) (b :: Symbol) (c :: Symbol)
     . Typeable c
    => Length a
    -> Length b
    -> Proxy (Length c)
    -> Maybe (Length c)
add x y p = do
  let Centimeters x' = canonical x
  let Centimeters y' = canonical y
  let xy = x' + y'
  if | typeOf p == typeOfProxyLengthCentimeters -> cast (Centimeters xy)
     | typeOf p == typeOfProxyLengthInches      -> cast (Inches     (xy/2.54))
     | otherwise -> Nothing

canonical
  :: forall (a :: Symbol)
   . Length a
  -> Length "centimeters"
canonical = \case
  la@Centimeters {} -> la
  Inches         a  -> Centimeters (a/2.54)

x3  = add oneHalfInch    twoCentimeters (Proxy::Proxy (Length "inches"))
x4  = add twoCentimeters twoCentimeters (Proxy::Proxy (Length "centimeters"))

x06 :: Spec
x06  = describe "X06" $ do
  it "x3" $ x3 `shouldBe` Just (Inches      0.8649017298034596)
  it "x4" $ x4 `shouldBe` Just (Centimeters 4.0)
