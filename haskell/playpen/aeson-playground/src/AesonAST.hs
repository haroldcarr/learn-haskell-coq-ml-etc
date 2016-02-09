{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Created       : 2016 Feb 09 (Tue) 14:37:22 by Harold Carr.
Last Modified : 2016 Feb 09 (Tue) 15:47:55 by Harold Carr.
-}

module AesonAST where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import           Data.Map
import           Data.Maybe          (fromJust)
import           Data.Text
import           GHC.Generics

------------------------------------------------------------------------------

data SparqlResults = SparqlResults {
      head    :: VarsObject
    , results :: BindingsVector
    } deriving (Generic, Show)

data VarsObject = VarsObject {
      vars :: [String]
    } deriving (Generic, Show)

data BindingsVector = BindingsVector {
      bindings :: [Binding]
    } deriving (Generic, Show)

data Binding = Binding {
      subject   :: Maybe BindingValue
    , predicate :: Maybe BindingValue
    , object    :: Maybe BindingValue
    } deriving (Generic, Show)

data BindingValue = BindingValue {
      -- type :: String
      value :: String
    } deriving (Generic, Show)

instance FromJSON SparqlResults
instance FromJSON VarsObject
instance FromJSON BindingsVector
instance FromJSON Binding
instance FromJSON BindingValue

------------------------------------------------------------------------------
-- Working with the AST

foo123 = fromJust $ decode "{\"foo\": 123}" :: Value
-- Object (fromList [("foo",Number 123)])
fooabc = fromJust $ decode "{\"foo\": [\"abc\",\"def\"]}" :: Value
-- Object (fromList [("foo",Array (fromList [String "abc",String "def"]))])

-- write functions to traverse above and make arbitrary transformations

sp = "{ \"head\": { \"vars\": [ \"subject\" , \"predicate\" , \"object\" ] } , \"results\": { \"bindings\": [ { \"subject\": { \"type\": \"uri\" , \"value\": \"http://openhc.org/data/event/University_of_Utah_Humanities_Happy_Hour\" } , \"predicate\": { \"type\": \"uri\" , \"value\": \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" } , \"object\": { \"type\": \"uri\" , \"value\": \"http://xmlns.com/foaf/0.1/Organization\" } } , { \"subject\": { \"type\": \"uri\" , \"value\": \"http://openhc.org/data/event/University_of_Utah_Humanities_Happy_Hour\" } , \"predicate\": { \"type\": \"uri\" , \"value\": \"http://xmlns.com/foaf/0.1/name\" } , \"object\": { \"type\": \"literal\" , \"xml:lang\": \"en\" , \"value\": \"University of Utah Humanities Happy Hour\" } } ] } }"

spast = fromJust $ decode sp :: Object

splist = M.toList spast

sphash = M.fromList splist

resultsObject = fromJust $ M.lookup (pack "results") sphash
resultsObjectBare = ov resultsObject

-- resultsHash = M.fromList $ M.toList resultsObject

------------------------------------------------------------------------------
-- Decoding to a Haskell value

-- can decode to any instance of FromJSON:

lint = fromJust $ decode "[1,2,3]" :: [Int]
-- [1,2,3]

-- instances for standard data types

mobj = fromJust $ decode "{\"foo\":1,\"bar\":2}" :: Map String Int
-- fromList [("bar",2),("foo",1)]

------------------------------------------------------------------------------
-- Decoding a mixed-type object

-- above not work for mixed-type objects
-- instead
-- when object contains JSON objects:

mixo = fromJust $ decode "{\"name\":\"Dave\",\"age\":2}" :: Object
-- fromList [("age",Number 2.0),("name",String "Dave")]

-- extract values using parse, parseEither, parseMaybe:

pobj = fromJust $ do result <- decode "{\"name\":\"Dave\",\"age\":2}"
                     flip parseMaybe result $ \obj -> do
                         age :: Int <- obj .: "age"
                         name       <- obj .: "name"
                         return (name ++ ": " ++ show (age*2))
-- "Dave: 4"

-- Any type that implements FromJSON can be used here.

-- End of file.
