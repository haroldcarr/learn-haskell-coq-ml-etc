{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Search where

import           Control.Monad       ((>=>))
import           Data.Aeson
import           Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import           Data.Text           as T
import           Data.Text.Encoding  as T
import           Prelude             as P
import           System.IO           (IOMode (ReadMode), withFile)
import           Test.HUnit          (Counts, Test (TestList), runTestTT)
import           Test.HUnit.Util     as U

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
          | otherwise                  -> P.concatMap (\(k,v) -> (findRefs' v (k:path) result)) kvs
findRefs' _ _ result = result

swagger :: T.Text
swagger =
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

ex :: Value
(Just ex) = decodeStrict (T.encodeUtf8 swagger)

tex :: [Test]
tex = U.t "tex"
    (findRefs ex)
    [(["definitions","Parent"]                                  ,("$ref",String "#/definitions/DoesNotExist"))
    ,(["definitions","Pet","properties","tags","items"]         ,("$ref",String "#/definitions/Tag"))
    ,(["paths","/pets","get","responses","200","schema","items"],("$ref",String "#/definitions/Pet"))]

------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList {- $ -} tex

{-

(Just s) <- readSwagger "/Users/carr/.sync/.ksync/rpt/0-catalog/validation-playground/swagger-validation/resource    s/examples/swagger20/refs-simple-invalid.json"
-}
