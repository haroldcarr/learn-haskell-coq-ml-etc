{-# LANGUAGE OverloadedStrings #-}

module LogDataSpec where

------------------------------------------------------------------------------
import qualified LogData              as LD
------------------------------------------------------------------------------
import qualified Data.Aeson           as JS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

jsen :: BSL.ByteString
jsen  = JS.encode LD.exampleLogList

loen :: BSL.ByteString
loen  = (JS.encode . LD.toLabeledJsonObject) LD.exampleLogList

jsdv :: Maybe JS.Value
jsdv  = JS.decode jsen
jsdd :: Maybe LD.LogList
jsdd  = JS.decode jsen

lodv :: Maybe JS.Value
lodv  = JS.decode loen
lodd :: Either T.Text LD.LogList
lodd  = LD.decodeLogList loen -- does not work

spec :: Spec
spec = do
  describe "encode" $ do
    it "json" $
      jsen `shouldBe`
      "[{\"tag\":\"LI\",\"contents\":7777},{\"tag\":\"RID\",\"contents\":{\"_ridShard\":-3,\"_ridNonce\":-2}},{\"tag\":\"NID\",\"contents\":{\"fullAddr\":\"host:8080\",\"host\":\"host\",\"port\":8080}},{\"tag\":\"TERM\",\"contents\":1},{\"tag\":\"TXT\",\"contents\":\"catchup\"}]"
    it "compact" $
      loen `shouldBe`
      "{\"LL\":[{\"LI\":7777},{\"RID\":{\"_ridShard\":-3,\"_ridNonce\":-2}},{\"NID\":{\"fullAddr\":\"host:8080\",\"host\":\"host\",\"port\":8080}},{\"TERM\":1},{\"TXT\":\"catchup\"}]}"
  describe "decode ByteString" $ do
    it "json" $
      jsdd `shouldBe` Just LD.exampleLogList
    it "compact" $
      lodd `shouldBe` Right LD.exampleLogList
