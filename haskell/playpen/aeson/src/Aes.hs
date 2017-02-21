{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Mar 17 (Tue) 18:23:16 by Harold Carr.
Last Modified : 2016 Feb 09 (Tue) 14:35:29 by Harold Carr.
-}

module Aes where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import           Data.Text

data Client i = GovOrg     i String
              | Company    i String Person String
              | Individual i Person
              deriving (Show)

clientToJSON :: Client Integer -> Value
clientToJSON (GovOrg i n) =
    object [ "type" .= String "govorg"
           , "id"   .= Number (fromInteger i)
           , "name" .= String (pack n)
           ]

clientToJSON (Company i n p d) =
    object [ "type"   .= String "company"
           , "id"     .= Number (fromInteger i)
           , "name"   .= String (pack n)
           , "person" .= personToJSON p
           , "duty"   .= String (pack d)
           ]

clientToJSON (Individual i p) =
    object [ "type"   .= String "individual"
           , "id"     .= Number (fromInteger i)
           , "person" .= personToJSON p
           ]

jsonToClient :: FromJSON i => Value -> Parser (Client i)
jsonToClient (Object o) =
    case M.lookup "type" o of
        Just (String "govorg")     -> GovOrg     <$> o .: "id" <*> o .: "name"
        Just (String "company")    -> Company    <$> o .: "id" <*> o .: "name" <*> o .: "person" <*> o .: "duty"
        Just (String "individual") -> Individual <$> o .: "id" <*> o .: "person"
        _                          -> Control.Applicative.empty
jsonToClient _ = Control.Applicative.empty

instance ToJSON (Client Integer) where
    toJSON    = clientToJSON
instance FromJSON i => FromJSON (Client i) where
    parseJSON = jsonToClient

data Person = Person String String
            deriving (Show)

personToJSON :: Person -> Value
personToJSON (Person f l) =
  object [ "first" .= String (pack f)
         , "last"  .= String (pack l)
         ]

jsonToPerson :: Value -> Parser Person
jsonToPerson (Object o) = Person <$> o .: "first" <*> o .: "last"
jsonToPerson _          = Control.Applicative.empty

instance ToJSON   Person where
    toJSON = personToJSON
instance FromJSON Person where
    parseJSON = jsonToPerson

{-
let tjg = toJSON (GovOrg     (1 :: Integer) "Govey")
fromJSON tjg :: Result (Client Integer)
let eng = encode tjg
decode eng :: Maybe (Client Integer)

let tjc = toJSON (Company    (2 :: Integer) "CompanyFoo" (Person "harold" "carr") "role")
fromJSON tjc :: Result (Client Integer)
let enc = encode tjc
decode enc :: Maybe (Client Integer)
-}

-- End of file.
