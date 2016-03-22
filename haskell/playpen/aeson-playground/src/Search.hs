{-# LANGUAGE OverloadedStrings #-}

module Search where

import           Control.Applicative
import           Control.Monad       (liftM, (>=>))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString     as BS
import qualified Data.HashMap.Strict as HM
import           Data.Text           as T
import           System.IO           (IOMode (ReadMode), withFile)

readSwagger :: FilePath -> IO (Maybe Value)
readSwagger filename =
    withFile filename ReadMode (BS.hGetContents >=> return . decodeStrict)

{-
--                      path   ref
findRefs :: Value -> [([Text],[Text])]
findRefs v = findRefs' v [] []

--                    path      result
findRefs' :: Value -> [Text] -> [([Text],[Text])] -> [([Text],[Text])]
findRefs' (Object o) p r = let kvs = HM.toList o
                           in if (fst (head kvs)) == "$ref"
                                 then [(p, (head kvs))]
findRefs'         _  _ r = r
-}

{-
s <- readSwagger "/Users/carr/.sync/.ksync/rpt/0-catalog/validation-playground/swagger-validation/resources/examples/swagger20/simple-ref.json"
decode s :: Maybe Value

-}
