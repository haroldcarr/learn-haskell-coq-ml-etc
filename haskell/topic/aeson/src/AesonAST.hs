{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Created       : 2016 Feb 09 (Tue) 14:37:22 by Harold Carr.
Last Modified : 2016 Feb 09 (Tue) 19:12:46 by Harold Carr.
-}

module AesonAST where

import qualified Data.Aeson          as A (Object, Value, decode, (.:))
import qualified Data.Aeson.Types    as AT (FromJSON, parseMaybe)
import qualified Data.HashMap.Strict as HM (fromList, lookup, toList)
import qualified Data.Map            as M (Map)
import qualified Data.Maybe          as MB (fromJust)
import qualified GHC.Generics        as G (Generic)
import           Prelude             as P

------------------------------------------------------------------------------

data SparqlResults = SparqlResults {
      head    :: VarsObject
    , results :: BindingsVector
    } deriving (G.Generic, Show)

data VarsObject = VarsObject {
      vars :: [String]
    } deriving (G.Generic, Show)

data BindingsVector = BindingsVector {
      bindings :: [Binding]
    } deriving (G.Generic, Show)

data Binding = Binding {
      subject   :: Maybe BindingValue
    , predicate :: Maybe BindingValue
    , object    :: Maybe BindingValue
    } deriving (G.Generic, Show)

data BindingValue = BindingValue {
      -- type :: String
      value :: String
    } deriving (G.Generic, Show)

instance AT.FromJSON SparqlResults
instance AT.FromJSON VarsObject
instance AT.FromJSON BindingsVector
instance AT.FromJSON Binding
instance AT.FromJSON BindingValue

------------------------------------------------------------------------------
-- Working with the AST

foo123 = MB.fromJust $ A.decode "{\"foo\": 123}" :: A.Value
-- Object (fromList [("foo",Number 123)])
fooabc = MB.fromJust $ A.decode "{\"foo\": [\"abc\",\"def\"]}" :: A.Value
-- Object (fromList [("foo",Array (fromList [String "abc",String "def"]))])

-- write functions to traverse above and make arbitrary transformations

{-
spast = MB.fromJust $ A.decode foo123 :: A.Value

splist = HM.toList spast

sphash = HM.fromList splist

resultsObject = MB.fromJust $ HM.lookup (T.pack "results") sphash

-- resultsHash = M.fromList $ M.toList resultsObject
-}

------------------------------------------------------------------------------
-- Decoding to a Haskell value

-- can decode to any instance of FromJSON:

lint = MB.fromJust $ A.decode "[1,2,3]" :: [Int]
-- [1,2,3]

-- instances for standard data types

mobj = MB.fromJust $ A.decode "{\"foo\":1,\"bar\":2}" :: M.Map String Int
-- fromList [("bar",2),("foo",1)]

------------------------------------------------------------------------------
-- Decoding a mixed-type object

-- above not work for mixed-type objects
-- instead
-- when object contains JSON objects:

mixo = MB.fromJust $ A.decode "{\"name\":\"Dave\",\"age\":2}" :: A.Object
-- fromList [("age",Number 2.0),("name",String "Dave")]

-- extract values using parse, parseEither, parseMaybe:

pobj = MB.fromJust $ do result <- A.decode "{\"name\":\"Dave\",\"age\":2}"
                        flip AT.parseMaybe result $ \obj -> do
                            age :: Int <- obj A..: "age"
                            name       <- obj A..: "name"
                            return (name ++ ": " ++ show (age*2))
-- "Dave: 4"

-- Any type that implements FromJSON can be used here.

-- End of file.
