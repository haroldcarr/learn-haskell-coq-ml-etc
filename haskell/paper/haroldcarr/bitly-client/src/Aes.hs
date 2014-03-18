{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           GHC.Generics

data Client i = GovOrg     i String
              | Company    i String Person String
              | Individual i Person
              deriving (Show, Generic)

data Person = Person String String
            deriving (Show, Read, Generic)

instance ToJSON   i => ToJSON   (Client i)
instance FromJSON i => FromJSON (Client i)
instance ToJSON   Person
instance FromJSON Person

{-
let tj = toJSON (GovOrg  (1 :: Integer) "Govey")
let tj = toJSON (Company (2 :: Integer) "CompanyFoo" (Person "harold" "carr") "role")
let tj = toJSON (Individual (3 :: Integer) (Person "flavia" "cervino-wood"))
let en = encode tj
let fj = fromJSON tj
{ "type": "company", "id": 1, "name": "Black Hole Inc."
, "person": { "first": "John", "last": "Smith" }, "duty": "Traveller" }
-}

-- End of file.
