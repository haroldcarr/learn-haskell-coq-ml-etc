{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Search where

import           Control.Monad       ((>=>))
import           Data.Aeson          (Value (Array, Object, String),
                                      decodeStrict)
import           Data.ByteString     as BS (hGetContents)
import qualified Data.HashMap.Strict as HM (toList)
import           Data.Text           as T (Text)
import           Data.Text.Encoding  as T (encodeUtf8)
import           Prelude             as P
import           System.IO           (IOMode (ReadMode), withFile)
import           Test.HUnit          (Counts, Test (TestList), runTestTT)
import           Test.HUnit.Util     as U (t)

readSwagger :: FilePath -> IO (Maybe Value)
readSwagger filename =
    withFile filename ReadMode (BS.hGetContents >=> return . decodeStrict)

--                      path   ref
findRefs :: Value -> [([Text], (Text, Value))]
findRefs v = findRefs' v [] []

--                    path      result-so-far                result
findRefs' :: Value -> [Text] -> [([Text], (Text, Value))] -> [([Text], (Text, Value))]
findRefs' (Object o) path result =
    let kvs = HM.toList o
    in if | P.null kvs                 -> result
          | "$ref" == fst (P.head kvs) -> (P.reverse path, P.head kvs) : result
          | otherwise                  -> P.concatMap (\(k,v) -> findRefs' v (k:path) result) kvs

findRefs' (Array a) path result         = P.concatMap (   \v  -> findRefs' v    path  result) a

findRefs' _ _ result = result

s1 :: Value
(Just s1) = decodeStrict (T.encodeUtf8 swagger1)

s2 :: Value
(Just s2) = decodeStrict (T.encodeUtf8 swagger2)

ts1 :: [Test]
ts1 = U.t "ts1"
    (findRefs s1)
    [(["definitions","Parent"]                                  ,("$ref",String "#/definitions/DoesNotExist"))
    ,(["definitions","Pet","properties","tags","items"]         ,("$ref",String "#/definitions/Tag"))
    ,(["paths","/pets","get","responses","200","schema","items"],("$ref",String "#/definitions/Pet"))]

ts2 :: [Test]
ts2 = U.t "ts2"
    (findRefs s2)
    [(["definitions","Parent","allOf"]                          ,("$ref",String "#/definitions/Circular2"))
    ,(["definitions","Circular1","allOf"]                       ,("$ref",String "#/definitions/Parent"))
    ,(["definitions","Circular2","allOf"]                       ,("$ref",String "#/definitions/Circular1"))
    ,(["definitions","Pet","properties","category"]             ,("$ref",String "#/definitions/Category"))
    ,(["definitions","Pet","properties","tags","items"]         ,("$ref",String "#/definitions/Tag"))
    ,(["paths","/pets","get","responses","200","schema","items"],("$ref",String "#/definitions/Pet"))]

------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList $ ts1 ++ ts2

swagger1 :: T.Text
swagger1 =
    "{\"swagger\": \"2.0\",\
      \\"paths\": {\
         \\"/pets\": {\
             \\"get\": {\
                 \\"tags\"       : [\"pet\"],\
                 \\"summary\"    : \"list the pets\",\
                 \\"operationId\": \"getPets\",\
                 \\"responses\"  : { \"default\": { \"description\": \"Generic Error\" },\
                                     \\"200\"    : { \"description\": \"Pets list\",\
                                                    \\"schema\": { \"type\": \"array\",\
                                                                  \\"items\": {\"$ref\":\"#/definitions/Pet\"}}}}}}},\
      \\"definitions\": {\
         \\"Parent\": { \"$ref\": \"#/definitions/DoesNotExist\" },\
         \\"Pet\"   : { \"properties\": { \"tags\": {  \"type\" : \"array\",\
                                                      \\"items\": {\"$ref\": \"#/definitions/Tag\"}}}},\
         \\"Tag\"   : { \"properties\": { \"n ame\": { \"type\": \"string\" }}}}}"

swagger2 :: T.Text
swagger2 =
    "{ \"swagger\": \"2.0\",\
    \  \"info\": { \"description\": \"indirect-circular-ancestor\" },\
    \  \"host\": \"petstore.swagger.wordnik.com\",\
    \  \"basePath\": \"/v2\",\
    \  \"schemes\": [ \"http\" ],\
    \  \"paths\": {\
    \      \"/pets\": { \"get\": { \"tags\": [\"pet\"],\
    \                          \"summary\": \"list the pets\",\
    \                          \"operationId\": \"getPets\",\
    \                          \"responses\": { \"default\": { \"description\": \"Generic Error\" },\
    \                                         \"200\": { \"description\": \"Pets list\",\
    \                                                  \"schema\": { \"type\": \"array\",\
    \                                                              \"items\": { \"$ref\": \"#/definitions/Pet\"}}}}}}},\
    \  \"definitions\": {\
    \      \"Circular2\": { \"allOf\": [{ \"$ref\": \"#/definitions/Circular1\" },\
    \                               { \"properties\": { \"createdAt\": { \"type\": \"string\", \"format\": \"date-time\"  } } }] },\
    \      \"Parent\"   : { \"allOf\": [{ \"$ref\": \"#/definitions/Circular2\"  }] },\
    \      \"Circular1\": { \"allOf\": [{ \"$ref\": \"#/definitions/Parent\" },\
    \                               { \"properties\": { \"age\": { \"type\": \"integer\", \"format\": \"int32\" }}}]},\
    \      \"Category\" : { \"properties\": { \"id\"       : { \"type\": \"integer\", \"format\": \"int64\"  },\
    \                                     \"name\"     : { \"type\": \"string\" } } },\
    \      \"Pet\"      : { \"required\"  : [ \"name\", \"photoUrls\"  ],\
    \                     \"properties\": { \"id\"       : { \"type\": \"integer\", \"format\": \"int64\" },\
    \                                     \"category\" : { \"$ref\": \"#/definitions/Category\" },\
    \                                     \"name\"     : { \"type\": \"string\", \"example\": \"doggie\" },\
    \                                     \"photoUrls\": { \"type\": \"array\", \"items\": { \"type\": \"string\" } },\
    \                                     \"tags\"     : { \"type\": \"array\", \"items\": { \"$ref\": \"#/definitions/Tag\" } },\
    \                                     \"status\"   : { \"type\": \"string\", \"description\": \"pet status in the store\" } } },\
    \      \"Tag\"      : { \"properties\": { \"id\"       : { \"type\": \"integer\", \"format\": \"int64\" },\
    \                                     \"name\"     : { \"type\": \"string\" } } } } }"
