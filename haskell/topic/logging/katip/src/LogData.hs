{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LogData where

import qualified Data.Aeson               as JS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.HashMap.Strict      as HMap
import qualified Data.Scientific          as Sci
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Katip                    as K
import           GHC.Generics
------------------------------------------------------------------------------
import qualified Raft as R

------------------------------------------------------------------------------
type LL = LogList
exll :: LogList
exll = LogList [ LI (R.LogIndex 7777)
               , RID (R.RequestId (-2) (-3))
               , NID (R.NodeID "host" 8080 "host:8080")
               , TERM (R.Term 1)
               , TXT "catchup"
               ]
oo  :: JS.Object
oo   = K.toObject exll
ooe :: BSL.ByteString
ooe  = JS.encode oo
tj  :: JS.Value
tj   = JS.toJSON exll
tje :: BSL.ByteString
tje  = JS.encode tj

xx :: JS.Object
xx =  case exll of
  LogList xs -> case JS.toJSON xs of
    a@(JS.Array _) -> HMap.singleton "appdata" a
    x              -> error ("xx LogList JSON" <> show x)
xxe :: BSL.ByteString
xxe  = JS.encode xx

en  :: BSL.ByteString
en   = JS.encode exll

{-
:set -XOverloadedStrings
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Aeson as JS
BSLC8.putStrLn ooe
BSLC8.putStrLn tje
BSLC8.putStrLn xxe
BSLC8.putStrLn en

JS.decode ooe :: Maybe JS.Value
JS.decode ooe :: Maybe LogList -- Nothing

JS.decode tje :: Maybe JS.Value
JS.decode tje :: Maybe LogList

JS.decode xxe :: Maybe JS.Value
JS.decode xxe :: Maybe LogList -- Nothing

JS.decode en  :: Maybe JS.Value
JS.decode en  :: Maybe LogList
-}

------------------------------------------------------------------------------
instance K.ToObject R.NodeID where
  toObject (R.NodeID _ _ fs) = HMap.singleton "NodeID" (JS.String (T.pack fs))
instance K.LogItem R.NodeID where
  payloadKeys _ _ = K.AllKeys

instance K.ToObject R.Term where
  toObject (R.Term t) = HMap.singleton "Term" (JS.Number (Sci.scientific (toInteger t) 0))
instance K.LogItem R.Term where
  payloadKeys _ _ = K.AllKeys

instance K.ToObject R.LogIndex where
  toObject (R.LogIndex i) = HMap.singleton "LogIndex" (JS.Number (Sci.scientific (toInteger i) 0))
instance K.LogItem R.LogIndex where
  payloadKeys _ _ = K.AllKeys

instance K.ToObject R.RequestId where
  toObject x = case JS.toJSON x of
    JS.Object o -> o
    _           -> error "toObject R.RequestId"
instance K.LogItem R.RequestId where
  payloadKeys _ _ = K.AllKeys

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
  show (TXT  x) = T.unpack x
instance JS.ToJSON   LogItem
instance JS.FromJSON LogItem
instance K.ToObject LogItem where
  toObject (NID nid) =
    HMap.singleton "NodeID" (JS.toJSON nid)
  toObject (RID rid) =
    HMap.singleton "RequestId" (JS.toJSON rid)
  toObject (TERM (R.Term term)) =
    HMap.singleton "Term" (JS.toJSON term)
  toObject (LI li) =
    HMap.singleton "LogIndex" (JS.toJSON li)
  toObject (TXT x) =
    HMap.singleton "TXT" (JS.toJSON x)
instance K.LogItem LogItem where
  payloadKeys _ _ = K.AllKeys

newtype LogList = LogList [LogItem] deriving Generic
instance Show LogList where
  show (LogList x) = show x
instance JS.ToJSON   LogList
instance JS.FromJSON LogList
{-
instance K.ToObject LogList where
  toObject (LogList xs) = case JS.toJSON xs of
    a@(JS.Array _) -> HMap.singleton "appdata" a
    x              -> error ("toObject LogList" <> show x)
-}
instance K.ToObject LogList where
  toObject (LogList xs) =
    HMap.singleton "LL" (JS.Array (V.fromList (map (JS.Object . K.toObject) xs)))
instance K.LogItem LogList where
  payloadKeys _ _ = K.AllKeys

