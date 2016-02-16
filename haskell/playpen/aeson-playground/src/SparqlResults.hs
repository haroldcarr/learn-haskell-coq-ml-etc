{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2016 Feb 09 (Tue) 14:37:22 by Harold Carr.
Last Modified : 2016 Feb 13 (Sat) 15:00:31 by Harold Carr.
-}

module SparqlResults where

import qualified Data.Aeson       as A (decode)
import qualified Data.Aeson.Types as AT (FromJSON)
import qualified Data.List        as L (nub, transpose)
import qualified Data.Maybe       as MB (fromJust)
import qualified GHC.Generics     as G (Generic)
import           Prelude          as P

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

selectFun ("subject")   = subject
selectFun ("predicate") = predicate
selectFun ("object")    = SparqlResults.object
selectFun x             = error ("not supported: " ++ x)

traverseResults(SparqlResults (VarsObject vs) (BindingsVector bs)) =
    P.zip vs $ P.map L.nub $ L.transpose $ traverseBindings (P.map selectFun vs) bs

traverseBindings vs bs = case bs of
    []      -> []
    (b:bs') -> P.map (\f -> value $ MB.fromJust $ f b) vs : traverseBindings vs bs'

sp = "{ \"head\": { \"vars\": [ \"subject\" , \"predicate\" , \"object\" ] } , \"results\": { \"bindings\": [ { \"subject\": { \"type\": \"uri\" , \"value\": \"http://openhc.org/data/event/University_of_Utah_Humanities_Happy_Hour\" } , \"predicate\": { \"type\": \"uri\" , \"value\": \"http://www.w3.org/1999/02/22-rdf-syntax-ns#type\" } , \"object\": { \"type\": \"uri\" , \"value\": \"http://xmlns.com/foaf/0.1/Organization\" } } , { \"subject\": { \"type\": \"uri\" , \"value\": \"http://openhc.org/data/event/University_of_Utah_Humanities_Happy_Hour\" } , \"predicate\": { \"type\": \"uri\" , \"value\": \"http://xmlns.com/foaf/0.1/name\" } , \"object\": { \"type\": \"literal\" , \"xml:lang\": \"en\" , \"value\": \"University of Utah Humanities Happy Hour\" } } ] } }"

spSpq = MB.fromJust $ A.decode sp :: SparqlResults
spR   = traverseResults spSpq
