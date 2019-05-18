{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HC1 where

import qualified Prelude
import           Protolude
import           Test.Hspec

------------------------------------------------------------------------------
-- TYPE-TO-TERM : natVal

newtype N (n :: Nat) = N Int deriving (Eq, Generic, Show) -- Generic only for NFData
instance NFData (N n)                                     -- NFData only for bottom example

mkN :: forall n . KnownNat n => Int -> Either Text (N n)
mkN i =
  if | i  < 0    -> Left (show i <> " < 0")
     | i /= n'   -> Left (show i <> " /= " <> show n')
     | otherwise -> Right (N i)
 where
  n' = fromIntegral (natVal (Proxy :: Proxy n))

h1,h1',h1'':: Spec
h1   = it "h1"   $ (mkN (-1) :: Either Text (N 1)) `shouldBe` Left "-1 < 0"
h1'  = it "h1'"  $ (mkN 1    :: Either Text (N 2)) `shouldBe` Left "1 /= 2"
h1'' = it "h1''" $ (mkN 1    :: Either Text (N 1)) `shouldBe` Right (N 1)

{-
-- note: not possible to build a structure where term and type do not line up:
newtype N' (n :: Nat) = N' Int deriving (Eq, Show)

mkN' :: forall n . KnownNat n => Int -> Either Text (N' n)
mkN' i =
  if i  < 0
  then Left (show i <> " < 0")
  else Right (N i)

    • Couldn't match type ‘N n0’ with ‘N' n’
      Expected type: Either Text (N' n)
        Actual type: Either Text (N n0)
    • In the expression: Right (N i)

-- TODO: how does 'mkN' know the terms/types line up?
-}

------------------------------------------------------------------------------
-- TERM-TO-TYPE : someNatVal

data SomeN = forall n . KnownNat n => SomeN (N n)
instance Eq SomeN where SomeN (N nl) == SomeN (N nr) = nl == nr
deriving instance Show SomeN

--                    term    type hidden inside
--                     |        |
--                     v        v
someNVal :: forall a . Int -> SomeN
someNVal i =
 case someNatVal (fromIntegral i) of
   Nothing                       -> panic "negative length"
   Just (SomeNat (_ :: Proxy n)) -> SomeN (N i :: N n)

h2  :: Spec
h2   = it "h2"  $                   someNVal 1      `shouldBe` someNVal 1

h2' :: Spec
h2'  = it "h2'" $ let SomeN (N n) = someNVal 1 in n `shouldBe`          1

------------------------------------------------------------------------------
-- type-safe operations on (positive) Int

withN :: forall a r . Int -> (forall n. KnownNat n => N n -> r) -> r
withN i f =
  case someNatVal (fromIntegral i) of  -- TERM-TO-TYPE
    Nothing                       -> panic "negative length"
    Just (SomeNat (_ :: Proxy n)) -> f (N i :: N n) -- f operates of typed N

h4 :: Spec
h4  = it "h4" $ withN 3 f4 `shouldBe` 3

f4 :: forall n . KnownNat n => N n -> Int
f4 _ = fromIntegral (natVal (Proxy :: Proxy n)) -- TYPE-TO-TERM example

------------------------------------------------------------------------------

h5,h5',h5'' :: Spec
h5   = it "h5"   $ f5      (mkN 0 :: Either Text (N 5))  `shouldBe` 5
h5'  = it "h5'"  $ f5'     (mkN 0 :: Either Text (N 5))  `shouldBe` "Left \"0 /= 5\""
h5'' = it "h5''" $ (f5 $!! (mkN 0 :: Either Text (N 5))) `shouldBe` 5 -- TODO ??

f5  :: forall n . (KnownNat n, n ~ 5) => Either Text (N n) -> Int
f5' :: forall n . (KnownNat n, n ~ 5) => Either Text (N n) -> Text

-- 'i' ignored so not evaluated?
f5  _ = fromIntegral (natVal (Proxy :: Proxy n))

f5' (Right (N i)) = show i
f5' l             = show l


--                    ignored            must match f6
--                      v                     v
h6 :: Spec
h6 = it "h6" $ f6 (mkN (-1) :: Either Text (N 0)) `shouldBe` "OK"

f6 :: Either Text (N 0) -> Text
f6 _ = "OK"
