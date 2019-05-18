{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HC2 where

import qualified Prelude
import           Protolude
import           Test.Hspec

data N (n :: Nat) = N deriving (Eq, Show)

-- existential
data SomeN = forall n . KnownNat n => SomeN (N n)
deriving instance Show SomeN

-- create type from runtime value
someNVal :: forall a . Int -> SomeN
someNVal i =
 case someNatVal (fromIntegral i) of
   Nothing                       -> panic "negative length"
   Just (SomeNat (_ :: Proxy n)) -> SomeN (N :: N n)

-- extract existential and call continuation with its value
withN :: forall a r . SomeN -> (forall n. KnownNat n => N n -> r) -> r
withN s f = case s of SomeN n -> f n

h24 :: Spec
h24  = it "h24" $ withN (someNVal 3) f `shouldBe` 3
 where
  f :: forall n . KnownNat n => N n -> Int
  f _ = fromIntegral (natVal (Proxy :: Proxy n)) -- create term from type

-- hack: create type at runtime and call compile-time indexed type
h25 :: Spec
h25  = it "h25" $ withN (someNVal 3) f `shouldBe` Just "3"
 where
  f  :: forall n . KnownNat n => N n -> Maybe Text
  f  n = if natVal (Proxy :: Proxy n) == 3 then Just (f' (N :: N 3)) else Nothing
  f' :: N 3 -> Text
  f' _ = "3"
