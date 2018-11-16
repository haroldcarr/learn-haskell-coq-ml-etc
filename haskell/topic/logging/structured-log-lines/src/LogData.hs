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
import qualified Data.List                  as DL
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics
------------------------------------------------------------------------------
import qualified Types                      as R
------------------------------------------------------------------------------
-- API

-- | Encoding format.
-- Note: 'Show' and 'ShowCompact' are one-way: they can only be encoded, not decoded.
data Format = JSON | JSONCompact | Show | ShowCompact

-- | Ordered list of things to be logged.
newtype LogList = LogList [LogItem] deriving (Eq, Generic)

-- | Things that can be logged.
data LogItem
  = AT        T.Text
  | CONSENSUS T.Text
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
  | RPC_TEXT   T.Text
  | TERM      R.Term
  | TO        T.Text
  | TXT       T.Text
  deriving (Eq, Generic)

-- | Encode according to format.
encode :: Format -> LogList -> BSL.ByteString
encode JSON        = JS.encode
encode JSONCompact = JS.encode . toLabeledJsonArray
 where toLabeledJsonArray (LogList xs) =
         JS.Array (V.fromList (map (JS.Object . toLabeledJsonObject) xs))
encode Show        = BSLC8.pack . show
encode ShowCompact = BSLC8.pack . showCompact

-- | Decode according to format.
decode :: Format -> BSL.ByteString -> Either T.Text LogList
decode JSON        = eitherStringToEitherText . JS.eitherDecode
decode JSONCompact = decodeLogList
decode Show        = Left . ("cannot decode Show: " <>)        . T.pack . BSLC8.unpack
decode ShowCompact = Left . ("cannot decode ShowCompact: " <>) . T.pack . BSLC8.unpack

------------------------------------------------------------------------------
-- IMPL

instance JS.ToJSON   LogList
instance JS.FromJSON LogList

instance JS.ToJSON   LogItem
instance JS.FromJSON LogItem

-- encoding

instance Show LogList where
  show (LogList x) = show x

instance Show LogItem where
  show (AT           x) = showLogItemText "AT" x
  show (CONSENSUS    x) = showLogItemText "CONSENSUS" x
  show (EorE         x) = showLogItemText "EorE" x
  show (INFO         x) = showLogItemText "INFO" x
  show (LI           x) = show x
  show (MSG_ID       x) = showLogItemText "MSG_ID" x
  show (MSG_TYPE     x) = showLogItemText "MSG_TYPE" x
  show (NETWORK      x) = showLogItemText "NETWORK" x
  show (NID          x) = show x
  show (RECOVERY     x) = showLogItemText "RECOVERY" x
  show (RPC_CATEGORY x) = showLogItemText "RPC_CATEGORY" x
  show (RPC_TEXT     x) = showLogItemText "RPC_TEXT" x
  show (RID          x) = show x
  show (ROLE         x) = showLogItemText "ROLE" (T.pack (show x)) -- TODO
  show (TERM         x) = show x
  show (TO           x) = showLogItemText "TO" x
  show (TXT          x) = showLogItemText "TXT" x

showLogItemText :: String -> T.Text -> String
showLogItemText s x = s ++ " " ++ T.unpack x

class Show a => ShowCompact a where
  showCompact :: a -> String

instance ShowCompact LogList where
  showCompact (LogList x) = showCompact x

instance ShowCompact a => ShowCompact [a] where
  showCompact as = DL.intercalate "; " (map showCompact as)

instance ShowCompact LogItem where
  showCompact (AT x)           = showC' x
  showCompact (CONSENSUS x)    = showC' x
  showCompact (EorE x)         = showC' x
  showCompact (INFO x)         = showC' x
  showCompact (LI   x)         = show x
  showCompact (MSG_ID x)       = showC' x
  showCompact (MSG_TYPE x)     = showC' x
  showCompact (NETWORK x)      = showC' x
  showCompact (NID x)          = show x
  showCompact (RECOVERY x)     = showC' x
  showCompact (RPC_CATEGORY x) = showC' x
  showCompact (RPC_TEXT x)      = showC' x
  showCompact (RID  x)         = show x
  showCompact (ROLE x)         = show x
  showCompact (TERM x)         = show x
  showCompact (TO x)           = showC' x
  showCompact (TXT x)          = showC' x

showC' :: T.Text -> String
showC' = T.unpack

class JS.ToJSON a => ToLabeledJsonObject a where
  toLabeledJsonObject   :: a -> JS.Object

instance ToLabeledJsonObject LogItem where
  toLabeledJsonObject (AT x)           = mkJsonObj "AT"   x
  toLabeledJsonObject (CONSENSUS x)    = mkJsonObj "CONSENSUS"   x
  toLabeledJsonObject (EorE x)         = mkJsonObj "EorE"   x
  toLabeledJsonObject (INFO x)         = mkJsonObj "INFO" x
  toLabeledJsonObject (LI   x)         = mkJsonObj "LI"   x
  toLabeledJsonObject (MSG_ID   x)     = mkJsonObj "MSG_ID"   x
  toLabeledJsonObject (MSG_TYPE   x)   = mkJsonObj "MSG_TYPE"   x
  toLabeledJsonObject (NETWORK  x)     = mkJsonObj "NETWORK"  x
  toLabeledJsonObject (NID  x)         = mkJsonObj "NID"  x
  toLabeledJsonObject (RECOVERY x)     = mkJsonObj "RECOVERY"  x
  toLabeledJsonObject (RID  x)         = mkJsonObj "RID"  x
  toLabeledJsonObject (ROLE x)         = mkJsonObj "ROLE" x
  toLabeledJsonObject (RPC_CATEGORY x) = mkJsonObj "RPC_CATEGORY" x
  toLabeledJsonObject (RPC_TEXT x)      = mkJsonObj "RPC_TEXT" x
  toLabeledJsonObject (TERM x)         = mkJsonObj "TERM" x
  toLabeledJsonObject (TO  x)          = mkJsonObj "TO"  x
  toLabeledJsonObject (TXT  x)         = mkJsonObj "TXT"  x

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
    "AT"           -> AT           <$> fj @T.Text      v -- TODO UTCTime
    "CONSENSUS"    -> CONSENSUS    <$> fj @T.Text      v
    "EorE"         -> EorE         <$> fj @T.Text      v
    "INFO"         -> INFO         <$> fj @T.Text      v
    "LI"           -> LI           <$> fj @R.LogIndex  v
    "MSG_ID"       -> MSG_ID       <$> fj @T.Text      v
    "MSG_TYPE"     -> MSG_TYPE     <$> fj @T.Text      v
    "NETWORK"      -> NETWORK      <$> fj @T.Text      v
    "NID"          -> NID          <$> fj @R.NodeID    v
    "RECOVERY"     -> RECOVERY     <$> fj @T.Text      v
    "RID"          -> RID          <$> fj @R.RequestId v
    "ROLE"         -> ROLE         <$> fj @R.Role      v
    "RPC_CATEGORY" -> RPC_CATEGORY <$> fj @T.Text      v
    "RPC_TEXT"      -> RPC_TEXT      <$> fj @T.Text      v
    "TERM"         -> TERM         <$> fj @R.Term      v
    "TO"           -> TO           <$> fj @T.Text      v
    "TXT"          -> TXT          <$> fj @T.Text      v
    e              -> Left $ "taggedJsonValuesToLogItems : unexpected: " <> e
  fj :: forall a . JS.FromJSON a => JS.Value -> Either T.Text a
  fj v' = case JS.fromJSON v' of
    JS.Success a -> Right a
    JS.Error   s -> Left (T.pack s)

------------------------------------------------------------------------------
-- utilities

mkJsonObj :: JS.ToJSON a => T.Text -> a -> JS.Object
mkJsonObj l x = HMap.singleton l (JS.toJSON x)

eitherStringToEitherText :: Either String a -> Either T.Text a
eitherStringToEitherText ea = case ea of
  Left str -> Left (T.pack str)
  Right  a -> Right a

------------------------------------------------------------------------------
-- test data

exampleLogList :: LogList
exampleLogList  = LogList exampleLogList'

exampleLogList' :: [LogItem]
exampleLogList' =
  [ AT "at"
  , CONSENSUS "consensus"
  , EorE "eore"
  , INFO "info"
  , LI (R.LogIndex 7777)
  , NETWORK "network"
  , MSG_ID "msg_id"
  , MSG_TYPE "msg_type"
  , NID  (R.NodeID "host" 8080 "host:8080")
  , RECOVERY "recovery"
  , RID  (R.RequestId (-2) (-3))
  , ROLE R.Follower
  , RPC_CATEGORY "rpc_category"
  , RPC_TEXT   "rpc_text"
  , TERM (R.Term 1)
  , TO "to"
  , TXT "txt"
  ]
