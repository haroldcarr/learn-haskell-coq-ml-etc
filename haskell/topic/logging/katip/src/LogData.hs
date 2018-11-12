{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LogData where

import qualified Data.Aeson           as JS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as HMap
import qualified Data.List            as DL
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
lodd :: Maybe LogList
lodd  = JS.decode loen -- does not work

{-
turn
(Object (fromList [("LL",Array [Object (fromList [             ("LI",             Number 7777.0)]),Object (fromList [("RID",Object (fromList [("_ridShard",Number -3.0),("_ridNonce",Number -2.0)]))]),Object (fromList [("NID",Object (fromList [("fullAddr",String "host:8080"),("host",String "host"),("port",Number 8080.0)]))]),Object (fromList [("TERM",Number 1.0)]),Object (fromList [("TXT",String "catchup")])])]))
into
                        (Array [Object (fromList [("tag",String "LI"),("contents",Number 7777.0)]),Object (fromList [("tag",String "RID"),("contents",Object (fromList [("_ridShard",Number -3.0),("_ridNonce",Number -2.0)]))]),Object (fromList [("tag",String "NID"),("contents",Object (fromList [("fullAddr",String "host:8080"),("host",String "host"),("port",Number 8080.0)]))]),Object (fromList [("tag",String "TERM"),("contents",Number 1.0)]),Object (fromList [("tag",String "TXT"),("contents",String "catchup")])])
-}

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

yyy :: [(T.Text, JS.Value)] -> [[(T.Text, JS.Value)]]
yyy [] = []
yyy ((l,v):xs) = [("tag"::T.Text, JS.String l), ("contents"::T.Text, v)] : yyy xs

zzz :: [[(T.Text, JS.Value)]] -> JS.Value
zzz = JS.Array .V.fromList . map (JS.Object . HMap.fromList)

zzz' :: JS.Value -> Maybe LogList
zzz' = JS.decode . JS.encode

-- zzz' (zzz $ yyy $ xxx lodv)

tj  :: JS.Value
tj   = JS.toJSON exll
tje :: BSL.ByteString
tje  = JS.encode tj -- same as enc

xx :: JS.Object
xx =  case exll of
  LogList xs -> case JS.toJSON xs of
    a@(JS.Array _) -> HMap.singleton "appdata" a
    x              -> error ("xx LogList JSON" <> show x)
xxe :: BSL.ByteString
xxe  = JS.encode xx -- same as enc but enclosed in object


{-
:set -XOverloadedStrings
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Aeson as JS
BSLC8.putStrLn ooe -- BEST OUTPUT
BSLC8.putStrLn tje
BSLC8.putStrLn xxe
BSLC8.putStrLn en

JS.decode ooe :: Maybe JS.Value
JS.decode ooe :: Maybe LogList -- Nothing -- BUT DOESN'T REVERSE

JS.decode tje :: Maybe JS.Value
JS.decode tje :: Maybe LogList

JS.decode xxe :: Maybe JS.Value
JS.decode xxe :: Maybe LogList -- Nothing

JS.decode en  :: Maybe JS.Value
JS.decode en  :: Maybe LogList
-}

------------------------------------------------------------------------------

data LogItem
  = NID  R.NodeID
  | RID  R.RequestId
  | LI   R.LogIndex
  | TERM R.Term
  | TXT  T.Text
  deriving Generic
instance Show LogItem where
  show (NID  x) = show x
  show (RID  x) = show x
  show (LI   x) = show x
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
