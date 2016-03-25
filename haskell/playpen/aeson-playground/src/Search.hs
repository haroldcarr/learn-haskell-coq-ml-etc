{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Search where

import           Control.Monad       ((>=>))
import           Data.Aeson
import           Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import           Data.Text           as T
import           Prelude             as P
import           System.IO           (IOMode (ReadMode), withFile)

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
    in if P.null kvs
           then result
           else let (hk,_) = P.head kvs
                in if hk == "$ref"
                       then (P.reverse path, P.head kvs) : result
                       else P.concatMap (\(k,v) -> (findRefs' v (k:path) result)) kvs
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

