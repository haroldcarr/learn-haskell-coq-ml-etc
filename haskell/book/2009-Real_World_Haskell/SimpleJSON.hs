module SimpleJSON
    (
      JValue(..) -- .. means export that type and all its constructors
    , getString
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
    ) where

-- from Haskell type to JSON type
data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray  [JValue]
              deriving (Eq, Ord, Show)

-- from JSON type to Haskell type
getString (JString s) = Just s
getString _           = Nothing
getInt    (JNumber n) = Just (truncate n)
getInt    _           = Nothing
getDouble (JNumber n) = Just n
getDouble _           = Nothing
getBool   (JBool   b) = Just b
getBool   _           = Nothing
getObject (JObject o) = Just o
getObject _           = Nothing
getArray  (JArray a)  = Just a
getArray  _           = Nothing
isNull    v           = v == JNull


