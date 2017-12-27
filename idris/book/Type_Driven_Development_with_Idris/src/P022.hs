{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module P022 where

import Data.Reflection

type family StringOrInt b where
  StringOrInt 'True  = Int
  StringOrInt 'False = String

intType :: StringOrInt 'True
intType = 3

stringType :: StringOrInt 'False
stringType = "three"

data SBool :: Bool -> * where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

getStringOrInt :: SBool x -> StringOrInt x
getStringOrInt x = reify x $ \p -> case reflect p of
  STrue  -> 94
  SFalse -> "Ninety four"

getStringOrInt' :: SBool x -> StringOrInt x
getStringOrInt' = \case
  STrue  -> 94
  SFalse -> "Ninety four"

valToString :: SBool x -> StringOrInt x -> String
valToString x val = reify x $ \p -> case reflect p of
  STrue  -> show val
  SFalse -> val

valToString' :: SBool x -> StringOrInt x -> String
valToString' = \case
  STrue  -> show
  SFalse -> id

