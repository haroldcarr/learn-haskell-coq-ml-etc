{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module LogData where

------------------------------------------------------------------------------
import qualified Data.Aeson                 as JS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.HashMap.Strict        as HMap
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics
------------------------------------------------------------------------------
import qualified Types                      as R
------------------------------------------------------------------------------
-- API

-- | Things that can be logged.
data LogItem
  = CONSENSUS T.Text
  | EorE      T.Text
  | INFO      T.Text
  | LI        R.LogIndex
  | NETWORK   T.Text
  | MSG_ID    T.Text
  | MSG_TYPE  T.Text
  | NID       R.NodeID
  | RECOVERY  T.Text
  | RID       R.RequestId
  | ROLE      R.Role
  | RPC_CATEGORY T.Text
  | RPC_TXT   T.Text
  | TERM      R.Term
  | TO        T.Text
  | TXT       T.Text
  deriving (Eq, Generic)

-- | Ordered list of things to be logged.
newtype LogList = LogList [LogItem] deriving (Eq, Generic)

-- | Encoding format.
data Format = JSON | CompactJSON | Textual

-- | Encode according to format.
encode :: Format -> LogList -> BSL.ByteString
encode JSON        = JS.encode
encode CompactJSON = JS.encode . toLabeledJsonArray
 where toLabeledJsonArray (LogList xs) =
         JS.Array (V.fromList (map (JS.Object . toLabeledJsonObject) xs))
encode Textual     = BSLC8.pack . show

-- | Decode according to format.
decode :: Format -> BSL.ByteString -> Either T.Text LogList
decode JSON        = eitherStringToEitherText . JS.eitherDecode
decode CompactJSON = decodeLogList
decode Textual     = Left . ("cannot decode Textual: " <>) . T.pack . BSLC8.unpack

------------------------------------------------------------------------------
-- IMPL

-- encoding

instance Show LogItem where
  show x@(CONSENSUS _) = show x
  show x@(EorE _) = show x
  show x@(INFO _) = show x
  show (LI   x) = show x
  show x@(MSG_ID _) = show x
  show x@(MSG_TYPE _) = show x
  show x@(NETWORK _) = show x
  show (NID  x) = show x
  show x@(RECOVERY _) = show x
  show x@(RPC_CATEGORY _) = show x
  show x@(RPC_TXT _) = show x
  show (RID  x) = show x
  show x@(ROLE _) = show x
  show (TERM x) = show x
  show x@(TO _) = show x
  show (TXT  x) = "TXT " ++ T.unpack x
instance JS.ToJSON   LogItem
instance JS.FromJSON LogItem

instance Show LogList where
  show (LogList x) = show x
instance JS.ToJSON   LogList
instance JS.FromJSON LogList

class JS.ToJSON a => ToLabeledJsonObject a where
  toLabeledJsonObject   :: a -> JS.Object

instance ToLabeledJsonObject LogItem where
  toLabeledJsonObject (EorE x) = mkSingleton "EorE"   x
  toLabeledJsonObject (CONSENSUS x) = mkSingleton "CONSENSUS"   x
  toLabeledJsonObject (INFO x) = mkSingleton "INFO" x
  toLabeledJsonObject (LI   x) = mkSingleton "LI"   x
  toLabeledJsonObject (MSG_ID   x) = mkSingleton "MSG_ID"   x
  toLabeledJsonObject (MSG_TYPE   x) = mkSingleton "MSG_TYPE"   x
  toLabeledJsonObject (NETWORK  x) = mkSingleton "NETWORK"  x
  toLabeledJsonObject (NID  x) = mkSingleton "NID"  x
  toLabeledJsonObject (RECOVERY x) = mkSingleton "RECOVERY"  x
  toLabeledJsonObject (RID  x) = mkSingleton "RID"  x
  toLabeledJsonObject (ROLE x) = mkSingleton "ROLE" x
  toLabeledJsonObject (RPC_CATEGORY x) = mkSingleton "RPC_CATEGORY" x
  toLabeledJsonObject (RPC_TXT x) = mkSingleton "RPC_TEXT" x
  toLabeledJsonObject (TERM x) = mkSingleton "TERM" x
  toLabeledJsonObject (TO  x) = mkSingleton "TO"  x
  toLabeledJsonObject (TXT  x) = mkSingleton "TXT"  x

-- decoding

decodeLogList :: BSL.ByteString -> Either T.Text LogList
decodeLogList a = case JS.eitherDecode a of
  Left  e -> Left $ "decodeLogList: " <> T.pack e
  Right v -> logListArrayToTaggedJsonValues v
         >>= taggedJsonValuesToLogItems
         >>= Right . LogList

logListArrayToTaggedJsonValues :: JS.Value -> Either T.Text [(T.Text, JS.Value)]
logListArrayToTaggedJsonValues v = case v of
  JS.Array a -> mapM f (V.toList a)
  _          -> Left $ "llattjv : not an JSON Array: " <> tshow v
 where
  f (JS.Object o') = case HMap.toList o' of
    [x] -> Right x
    e   -> Left $ "llattjv : object in array has more than one field: " <> tshow e
  f e = Left $ "llattjv : array contains items that are not JSON objects: " <> tshow e
  tshow :: Show a => a -> T.Text
  tshow = T.pack . show

taggedJsonValuesToLogItems :: [(T.Text, JS.Value)] -> Either T.Text [LogItem]
taggedJsonValuesToLogItems = mapM f
 where
  f (l, v) = case l of
    "LI"   -> LI   <$> fj @R.LogIndex  v
    "NID"  -> NID  <$> fj @R.NodeID    v
    "RID"  -> RID  <$> fj @R.RequestId v
    "ROLE" -> ROLE <$> fj @R.Role      v
    "TERM" -> TERM <$> fj @R.Term      v
    "TXT"  -> TXT  <$> fj @T.Text      v
    e      -> Left $ "taggedJsonValuesToLogItems : unexpected: " <> e
  fj :: forall a . JS.FromJSON a => JS.Value -> Either T.Text a
  fj v' = case JS.fromJSON v' of
    JS.Success a -> Right a
    JS.Error   s -> Left (T.pack s)

------------------------------------------------------------------------------
-- utilities

mkSingleton :: JS.ToJSON a => T.Text -> a -> JS.Object
mkSingleton l x = HMap.singleton l (JS.toJSON x)

eitherStringToEitherText :: Either String a -> Either T.Text a
eitherStringToEitherText ea = case ea of
  Left str -> Left (T.pack str)
  Right  a -> Right a

------------------------------------------------------------------------------
-- test data

exampleLogList :: LogList
exampleLogList =
  LogList [ LI   (R.LogIndex 7777)
          , RID  (R.RequestId (-2) (-3))
          , NID  (R.NodeID "host" 8080 "host:8080")
          , TERM (R.Term 1)
          , TXT  "catchup"
          ]
