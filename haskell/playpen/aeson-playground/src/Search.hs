{-# LANGUAGE OverloadedStrings #-}

module Search where

import           Control.Monad       ((>=>))
import           Data.Aeson          (Value (Array, Object, String),
                                      decodeStrict)
import           Data.ByteString     as BS (hGetContents)
import           Data.HashMap.Strict as HM (fromList, toList)
import           Data.Text           as T (Text)
import           Prelude             as P
import           System.IO           (IOMode (ReadMode), withFile)
import           System.IO.Unsafe    (unsafePerformIO)
import           Test.HUnit          (Counts, Test (TestList), runTestTT)
import           Test.HUnit.Util     as U (t)

--               path    field field-value
type Result = [([Text], (Text, Value))]

readJson :: FilePath -> IO (Maybe Value)
readJson filename =
    withFile filename ReadMode (BS.hGetContents >=> return . decodeStrict)

findInJson :: Text -> Value -> Result
findInJson goal top = fJson top []
  where
    --                 path
    fJson :: Value -> [Text] -> Result
    fJson (Object o) path  = fObj (HM.toList o)
      where
        fObj []            = []
        fObj (hd@(k,v):tl)
            | goal == k    = (P.reverse path, hd) :  fObj tl
            | otherwise    = fJson v (k:path)     ++ fObj tl
    fJson (Array  a) path  = P.concatMap (`fJson` path) a
    fJson         _     _  = []

readFind :: Text -> FilePath -> IO Result
readFind goal filename = readJson filename >>= (\(Just s) -> return $ findInJson goal s)

ts1 :: [Test]
ts1 = U.t "ts1"
    (unsafePerformIO (readFind "$ref" "test/refs-simple-invalid.json"))
    [(["definitions","Parent"]                                  ,("$ref",String "#/definitions/DoesNotExist"))
    ,(["definitions","Pet","properties","tags","items"]         ,("$ref",String "#/definitions/Tag"))
    ,(["paths","/pets","get","responses","200","schema","items"],("$ref",String "#/definitions/Pet"))]

ts2 :: [Test]
ts2 = U.t "ts2"
    (unsafePerformIO (readFind "$ref" "test/refs-indirect-circular-ancestor-invalid.json"))
    [(["definitions","Parent","allOf"]                          ,("$ref",String "#/definitions/Circular2"))
    ,(["definitions","Circular1","allOf"]                       ,("$ref",String "#/definitions/Parent"))
    ,(["definitions","Circular2","allOf"]                       ,("$ref",String "#/definitions/Circular1"))
    ,(["definitions","Pet","properties","category"]             ,("$ref",String "#/definitions/Category"))
    ,(["definitions","Pet","properties","tags","items"]         ,("$ref",String "#/definitions/Tag"))
    ,(["paths","/pets","get","responses","200","schema","items"],("$ref",String "#/definitions/Pet"))]

ts3 :: [Test]
ts3 = U.t "ts3"
    (unsafePerformIO (readFind "X" "test/x.json"))
    [(["Tag"]                              ,("X",Object (fromList [("Tag-X-value",Object (fromList [("X",String "Tag-X-value-X-value")]))])))
    ,(["responses","default"]              ,("X",String "responses-default-X-value"))
    ,(["responses","200","schema","items"] ,("X",String "responses-200-schema-items-X-value"))
    ,(["responses","200"]                  ,("X",String "responses-200-X-value"))
    ,(["tags","items"]                     ,("X",String "tags-items-X-value-2"))]

ts4 :: [Test]
ts4 = U.t "ts4"
    (findInJson
     "X"
     (Object
      (fromList
       [("default" ,Object (fromList [("X",String "responses-default-X-value")]))
       ,("200"     ,Object (fromList [("schema",Object (fromList [("items",Object (fromList [("X",String "responses-200-schema-items-X-value")]))
                                                                 ,("type",String "array")]))
                                     ,("X",String "responses-200-X-value")]))])))
    [(["default"]               ,("X",String "responses-default-X-value"))
    ,(["200","schema","items"]  ,("X",String "responses-200-schema-items-X-value"))
    ,(["200"]                   ,("X",String "responses-200-X-value"))]
------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList $ ts1 ++ ts2 ++ ts3 ++ ts4
