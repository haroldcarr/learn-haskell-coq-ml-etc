{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import qualified Data.Aeson               as JS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.HashMap.Strict      as HMap
import qualified Data.Scientific          as Sci
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified Control.Exception        as CE
import qualified Katip                    as K
import qualified Katip.Core               as K
import qualified System.IO                as SIO
import qualified Control.Monad.RWS.Strict as RWS
import           GHC.Generics
------------------------------------------------------------------------------
import qualified Raft as R

type Stack a = K.KatipContextT IO a

someFunc :: IO ()
someFunc = do
  let (s, e) = R.mkSpecStateEnv
   in RWS.void $ RWS.evalRWST R.server e s
  main2

main2 :: IO ()
main2 = do
  -- handleScribe <- K.mkHandleScribe K.ColorIfTerminal SIO.stdout K.InfoS K.V2
  handleScribe <- K.mkHandleScribeWithFormatter
                   K.jsonFormat K.ColorIfTerminal SIO.stdout K.InfoS K.V2
  i            <- K.initLogEnv "MyApp" "production"
  let mkLogEnv  = K.registerScribe "stdout" handleScribe K.defaultScribeSettings i
  CE.bracket mkLogEnv K.closeScribes $ \le ->
    K.runKatipContextT le (mempty :: K.LogContexts) mempty test

-- `Stack ()` is the same thing as `KatipContextT IO ()`
-- test :: Stack ()
test :: K.KatipContextT IO ()
test = do
  $(K.logTM) K.InfoS "Hello from Katip!"
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  $(K.logTM) K.InfoS $ K.ls (JS.encode (R.RequestId 30 40))
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  $(K.logTM) K.InfoS $ K.showLS        (R.RequestId 30 40)
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  $(K.logT) (R.LogIndex  89) mempty K.InfoS "WWW"
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  K.logLoc  (R.LogIndex 100) mempty K.InfoS "XXX"
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  K.logF    (R.LogIndex 333) mempty K.InfoS "YYY"
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  K.katipAddContext (R.LogIndex 666) $ do
    $(K.logTM) K.InfoS "ZZZ"
    K.katipAddContext (R.RequestId 3 4) $
      $(K.logTM) K.InfoS "AAA"
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  K.katipAddContext (R.LogIndex 7777) $
      K.katipAddContext (R.RequestId (-2) (-3)) $
        K.katipAddContext (R.NodeID "host" 8080 "host:8080") $
          K.katipAddContext (R.Term 1) $
            $(K.logTM) K.InfoS "KKK"
  K.logMsg mempty K.InfoS "----------------------------------------------------------------------"
  $(K.logT) alil mempty K.InfoS "BBB"

alil :: ALIL
alil = ALIL [ LI (R.LogIndex 7777)
            , RID (R.RequestId (-2) (-3))
            , NID (R.NodeID "host" 8080 "host:8080")
            , TERM (R.Term 1)
            , TXT "catchup"
            ]
oo :: JS.Object
oo = K.toObject alil
tj :: JS.Value
tj = JS.toJSON alil
en :: BSL.ByteString
en = JS.encode alil

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

data ALI
  = NID  R.NodeID
  | RID  R.RequestId
  | LI   R.LogIndex
  | TERM R.Term
  | TXT  T.Text
  deriving Generic
instance Show ALI where
  show (NID  x) = show x
  show (RID  x) = show x
  show (LI   x) = show x
  show (TERM x) = show x
  show (TXT  x) = T.unpack x
instance JS.ToJSON   ALI
instance JS.FromJSON ALI
instance K.ToObject ALI where
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
instance K.LogItem ALI where
  payloadKeys _ _ = K.AllKeys

newtype ALIL = ALIL [ALI] deriving Generic
instance Show ALIL where
  show (ALIL x) = show x
instance JS.ToJSON   ALIL
instance JS.FromJSON ALIL
{-
instance K.ToObject ALIL where
  toObject (ALIL xs) = case JS.toJSON xs of
    a@(JS.Array _) -> HMap.singleton "appdata" a
    x              -> error ("toObject ALIL" <> show x)
-}
instance K.ToObject ALIL where
  toObject (ALIL xs) =
    HMap.singleton "alil" (JS.Array (V.fromList (map (JS.Object . K.toObject) xs)))
instance K.LogItem ALIL where
  payloadKeys _ _ = K.AllKeys

