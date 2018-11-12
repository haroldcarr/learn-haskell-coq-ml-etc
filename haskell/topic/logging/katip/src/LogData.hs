{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LogData where

import qualified Data.Aeson           as JS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HMap
import qualified Data.List            as DL
import qualified Data.Maybe           as DM
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           GHC.Generics
------------------------------------------------------------------------------
import qualified Raft                 as R

------------------------------------------------------------------------------
type LL = LogList
exll :: LogList
exll = LogList [ LI   (R.LogIndex 7777)
               , RID  (R.RequestId (-2) (-3))
               , NID  (R.NodeID "host" 8080 "host:8080")
               , TERM (R.Term 1)
               , TXT  "catchup"
               ]
jsen :: BSL.ByteString
jsen  = JS.encode exll

loen :: BSL.ByteString
loen  = (JS.encode . toLabeledJsonObject) exll

jsdv :: JS.Value
Just jsdv  = JS.decode jsen
jsdd :: LogList
Just jsdd  = JS.decode jsen

lodv :: JS.Value
Just lodv  = JS.decode loen
lodd :: LogList
lodd  = dzzz loen -- does not work

xxx :: JS.Value -> [(T.Text, JS.Value)]
xxx v = case v of
  JS.Object o ->
    case HMap.lookup "LL" o of
      Nothing -> error "xxx : missing LL"
      Just (JS.Array a) -> map f (V.toList a)
      _                 -> error "xxx : LL value not a JSON Array"
  _           -> error "xxx : not an JSON Object"
 where
  f (JS.Object o') = case HMap.toList o' of
    [x] -> x
    _   -> error "xxx : object in array has more than one field"
  f _ = error "xxx : array contains items that are not JSON objects"

yyy :: [(T.Text, JS.Value)] -> [LogItem]
yyy [] = []
yyy ((l,v):xs) = case l of
  "LI" -> LI (fromJsonE v :: R.LogIndex) : yyy xs
  "NID" -> NID (fromJsonE v :: R.NodeID) : yyy xs
  "RID" -> RID (fromJsonE v :: R.RequestId) : yyy xs
  "TERM" -> TERM (fromJsonE v :: R.Term) : yyy xs
  "TXT" -> TXT (fromJsonE v :: T.Text) : yyy xs
  _ -> error "yyy : unexpected"
 where
  fromJsonE :: JS.FromJSON a => JS.Value -> a
  fromJsonE v' = case JS.fromJSON v' of
    JS.Success a -> a
    JS.Error   s -> error s

zzz :: JS.Value -> LogList
zzz = LogList . yyy . xxx

dzzz :: BSL.ByteString -> LogList
dzzz a = case JS.decode a of
  Nothing -> error "dzzz"
  Just a' -> zzz a'

------------------------------------------------------------------------------

data LogItem
  = NID  R.NodeID
  | RID  R.RequestId
  | LI   R.LogIndex
  | TERM R.Term
  | TXT  T.Text
  deriving Generic
instance Show LogItem where
  show (LI   x) = show x
  show (NID  x) = show x
  show (RID  x) = show x
  show (TERM x) = show x
  show (TXT  x) = "TXT " ++ T.unpack x
instance JS.ToJSON   LogItem
instance JS.FromJSON LogItem

newtype LogList = LogList [LogItem] deriving Generic
instance Show LogList where
  show (LogList x) = show x
instance JS.ToJSON   LogList
instance JS.FromJSON LogList

class (JS.ToJSON a, JS.FromJSON a) => LabeledJsonObject a where
  toLabeledJsonObject   :: a -> JS.Object
  -- fromLabeledJsonObject :: JS.Object -> a

instance LabeledJsonObject LogItem where
  toLabeledJsonObject (NID  x) = mkSingleton "NID"  x
  -- fromLabeledJsonObject (JS.Object [("LI", JS.Number x)]) = LI $ JS.fromJSON x :: R.LogIndex
  toLabeledJsonObject (RID  x) = mkSingleton "RID"  x
  toLabeledJsonObject (TERM x) = mkSingleton "TERM" x
  toLabeledJsonObject (LI   x) = mkSingleton "LI"   x
  toLabeledJsonObject (TXT  x) = mkSingleton "TXT"  x

mkSingleton :: JS.ToJSON a => T.Text -> a -> JS.Object
mkSingleton l x = HMap.singleton l (JS.toJSON x)

instance LabeledJsonObject LogList where
  toLabeledJsonObject (LogList xs) =
    HMap.singleton "LL" (JS.Array (V.fromList (map (JS.Object . toLabeledJsonObject) xs)))
