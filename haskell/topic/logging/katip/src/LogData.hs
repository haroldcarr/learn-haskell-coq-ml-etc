{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module LogData where

import qualified Data.Aeson                 as JS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.HashMap.Strict        as HMap
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics
------------------------------------------------------------------------------
import qualified Raft                       as R

------------------------------------------------------------------------------

data LogItem
  = NID  R.NodeID
  | RID  R.RequestId
  | LI   R.LogIndex
  | TERM R.Term
  | TXT  T.Text
  deriving (Eq, Generic)
instance Show LogItem where
  show (LI   x) = show x
  show (NID  x) = show x
  show (RID  x) = show x
  show (TERM x) = show x
  show (TXT  x) = "TXT " ++ T.unpack x
instance JS.ToJSON   LogItem
instance JS.FromJSON LogItem

newtype LogList = LogList [LogItem] deriving (Eq, Generic)
instance Show LogList where
  show (LogList x) = show x
instance JS.ToJSON   LogList
instance JS.FromJSON LogList

class JS.ToJSON a => ToLabeledJsonObject a where
  toLabeledJsonObject   :: a -> JS.Object

instance ToLabeledJsonObject LogItem where
  toLabeledJsonObject (NID  x) = mkSingleton "NID"  x
  toLabeledJsonObject (RID  x) = mkSingleton "RID"  x
  toLabeledJsonObject (TERM x) = mkSingleton "TERM" x
  toLabeledJsonObject (LI   x) = mkSingleton "LI"   x
  toLabeledJsonObject (TXT  x) = mkSingleton "TXT"  x

mkSingleton :: JS.ToJSON a => T.Text -> a -> JS.Object
mkSingleton l x = HMap.singleton l (JS.toJSON x)

instance ToLabeledJsonObject LogList where
  toLabeledJsonObject (LogList xs) =
    HMap.singleton "LL" (JS.Array (V.fromList (map (JS.Object . toLabeledJsonObject) xs)))

data Format = JSON | CompactJSON | Textual

encode :: Format -> LogList -> BSL.ByteString
encode JSON a =
  JS.encode $ HMap.fromList [ ("encoding"::T.Text, JS.String "JSON")
                            , ("data", JS.toJSON a)
                            ]
encode CompactJSON (LogList xs) =
  JS.encode $ HMap.fromList [ ("encoding"::T.Text, JS.String "CompactJSON")
                            , ("data"    ::T.Text, toLabeledJsonArray xs)
                            ]
encode Textual a =
  (BSLC8.pack . show) a

toLabeledJsonArray :: [LogItem] -> JS.Value
toLabeledJsonArray xs = JS.Array (V.fromList (map (JS.Object . toLabeledJsonObject) xs))

------------------------------------------------------------------------------

logListObjectToTaggedJsonValues :: JS.Value -> Either T.Text [(T.Text, JS.Value)]
logListObjectToTaggedJsonValues v = case v of
  JS.Object o ->
    case HMap.lookup "LL" o of
      Nothing           -> Left $ "xxx : missing LL: " <> tshow v
      Just (JS.Array a) -> mapM f (V.toList a)
      Just x            -> Left $ "xxx : LL value not a JSON Array: " <> tshow x
  _ -> Left $ "xxx : not an JSON Object: " <> tshow v
 where
  f (JS.Object o') = case HMap.toList o' of
    [x] -> Right x
    e   -> Left $ "xxx : object in array has more than one field: " <> tshow e
  f e = Left $ "xxx : array contains items that are not JSON objects: " <> tshow e
  tshow :: Show a => a -> T.Text
  tshow = T.pack . show

taggedJsonValuesToLogItems :: [(T.Text, JS.Value)] -> Either T.Text [LogItem]
taggedJsonValuesToLogItems = mapM f
 where
  f (l, v) = case l of
    "LI"   -> LI   <$> fj @R.LogIndex  v
    "NID"  -> NID  <$> fj @R.NodeID    v
    "RID"  -> RID  <$> fj @R.RequestId v
    "TERM" -> TERM <$> fj @R.Term      v
    "TXT"  -> TXT  <$> fj @T.Text      v
    e      -> Left $ "taggedJsonValuesToLogItems : unexpected: " <> e
  fj :: forall a . JS.FromJSON a => JS.Value -> Either T.Text a
  fj v' = case JS.fromJSON v' of
    JS.Success a -> Right a
    JS.Error   s -> Left (T.pack s)

decodeLogList :: BSL.ByteString -> Either T.Text LogList
decodeLogList a = case JS.eitherDecode a of
  Left  e -> Left $ "decodeLogList: " <> T.pack e
  Right v -> logListObjectToTaggedJsonValues v
         >>= taggedJsonValuesToLogItems
         >>= Right . LogList

------------------------------------------------------------------------------

exampleLogList :: LogList
exampleLogList =
  LogList [ LI   (R.LogIndex 7777)
          , RID  (R.RequestId (-2) (-3))
          , NID  (R.NodeID "host" 8080 "host:8080")
          , TERM (R.Term 1)
          , TXT  "catchup"
          ]
