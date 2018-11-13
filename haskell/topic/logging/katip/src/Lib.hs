{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import qualified Control.Exception    as CE
import qualified Data.Aeson           as JS
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Scientific      as Sci
import qualified Data.Text            as T
import qualified Katip                as K
import qualified Katip.Core           as K
import qualified System.IO            as SIO
import qualified Data.Vector          as V
------------------------------------------------------------------------------
import qualified LogData              as LD
import qualified Raft                 as R

type Stack a = K.KatipContextT IO a

run :: IO ()
run = do
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
  $(K.logT) LD.exampleLogList mempty K.InfoS "BBB"

------------------------------------------------------------------------------

-- BEGIN : only necessary when giving directly as context
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
-- END : only necessary when giving directly as context

-- If context is always enclosed in LogList, then above ToObject are NOT necessary.
instance K.ToObject LD.LogItem where
  toObject = LD.toLabeledJsonObject
instance K.LogItem LD.LogItem where
  payloadKeys _ _ = K.AllKeys

instance K.ToObject LD.LogList where
  toObject = LD.toLabeledJsonObject
instance K.LogItem LD.LogList where
  payloadKeys _ _ = K.AllKeys
instance LD.ToLabeledJsonObject LD.LogList where
  toLabeledJsonObject (LD.LogList xs) =
    HMap.singleton "LL" (JS.Array (V.fromList (map (JS.Object . LD.toLabeledJsonObject) xs)))
