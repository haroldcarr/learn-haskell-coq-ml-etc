{-# LANGUAGE MultiWayIf        #-}
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
findInJson goal top = f goal top [] []
  where
    --                     path     result-in    result-out
    f :: Text -> Value -> [Text] -> Result    -> Result
    f g (Object o) path result =
        let kvs = HM.toList o
        in if | P.null kvs            -> result
              | g == fst (P.head kvs) -> (P.reverse path, P.head kvs) : result
              | otherwise             -> P.concatMap (\(k,v) -> f g v (k:path) result) kvs
    f g (Array  a) path result         = P.concatMap (   \v  -> f g v    path  result) a
    f _         _     _ result         = result

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
    -- INCORRECT RESULTS - MISSING STUFF
    [(["definitions","Tag"]                                     , ("X",Object (fromList [("Tag-X-value",Object (fromList [("X",String "Tag-X-value-X-value")]))])))
    ,(["definitions","Pet","properties","tags","items"]         , ("X",String "items-X-2-value"))
    ,(["paths","/pets","get","responses","default"]             , ("X",String "default-X-value"))
    ,(["paths","/pets","get","responses","200","schema","items"], ("X",String "200-schema-items-X-value"))]

------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList $ ts1 ++ ts2 ++ ts3
