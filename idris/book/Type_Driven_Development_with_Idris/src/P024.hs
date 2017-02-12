{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module P024 where

type family StringOrInt b where
  StringOrInt 'True  = Int
  StringOrInt 'False = String

intType :: StringOrInt 'True
intType = 3

stringType :: StringOrInt 'False
stringType = "three"

{-
getStringOrInt :: (x :: Bool) -> StringOrInt x
getStringOrInt x = case x of
    True  -> 94
    False -> "Ninety four"

valToString :: (x :: Bool) -> StringOrInt x -> String
valToString x val = case x of
    True  -> show val
    False -> val
-}
