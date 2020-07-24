{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module FuelLevel where

import           Protolude
import           Refined
import           Test.Hspec

------------------------------------------------------------------------------

newtype FuelLevel0 = FuelLevel0
  { getFuelLevel0 :: Int
  } deriving (Eq, Ord, Num, Show)

validateFuelLevel0 :: Int -> Maybe FuelLevel0
validateFuelLevel0 i =
  if 0 <= i && i <= 10
  then Just (FuelLevel0 i)
  else Nothing

fl0 :: Maybe FuelLevel0
fl0 = do
  x <- validateFuelLevel0 3
  y <- validateFuelLevel0 9
  pure (x + y)

-- if full == 10, then the result of addition should be checked to producing Nothing.
fl0t :: Spec
fl0t  = it "FuelLevel0" $ fl0 `shouldBe` Just (FuelLevel0 12)

------------------------------------------------------------------------------

newtype FuelLevel1 p = FuelLevel1
  { getFuelLevel1 :: Int
  } deriving (Eq, Ord, Num, Show)

data InRange1

validateFuelLevel1 :: Int -> Maybe (FuelLevel1 InRange1)
validateFuelLevel1 i =
  if 0 <= i && i <= 10
  then Just (FuelLevel1 i)
  else Nothing

fl1 :: Maybe (FuelLevel1 InRange1)
fl1 = do
  x <- validateFuelLevel1 3
  y <- validateFuelLevel1 9
  pure (x + y)

-- if full == 10, then the result of addition should be checked to producing Nothing.
fl1t :: Spec
fl1t  = it "FuelLevel1" $ fl1 `shouldBe` Just (FuelLevel1 12)

------------------------------------------------------------------------------

type FuelLevel2 = Refined (FromTo 0 10) Int

type FuelLevelException2 = RefineException

validateFuelLevel2 :: Int -> Either FuelLevelException2 FuelLevel2
validateFuelLevel2  = refine

fl2 :: Either FuelLevelException2 FuelLevel2
fl2  = do
  x <- validateFuelLevel2 3
  y <- validateFuelLevel2 9
  -- • No instance for (Num FuelLevel2) arising from a use of ‘+’
  -- pure (x + y)
  -- • Couldn't match type ‘Int’ with ‘Refined (FromTo 0 10) Int’
  -- pure (unrefine x + unrefine y)
  refine (unrefine x + unrefine y)

-- this time the result of addition is check, thus raising an exception
fl2t :: Spec
fl2t  =
  let r = fl2 in
  it "FuelLevel2" $ (show r :: Text) `shouldBe`
  "Left The predicate (FromTo 0 10) does not hold: \n  Value is out of range (minimum: 0, maximum: 10)"



