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

jen,cen,ten :: BSL.ByteString
jen  = LD.encode LD.JSON        LD.exampleLogList
cen  = LD.encode LD.CompactJSON LD.exampleLogList
ten  = LD.encode LD.Textual     LD.exampleLogList

jdc,cdc,tdc :: Either T.Text LD.LogList
jdc  = LD.decode LD.JSON        jen
cdc  = LD.decode LD.CompactJSON cen
tdc  = LD.decode LD.Textual     ten

jsdv :: Maybe JS.Value
jsdv  = JS.decode jen
lodv :: Maybe JS.Value
lodv  = JS.decode cen

spec :: Spec
spec = do
  describe "encode" $ do
    it "json" $
      jen `shouldBe`
      "[{\"tag\":\"LI\",\"contents\":7777}\
      \,{\"tag\":\"RID\",\"contents\":{\"_ridShard\":-3,\"_ridNonce\":-2}}\
      \,{\"tag\":\"NID\",\"contents\":{\"fullAddr\":\"host:8080\",\"host\":\"host\",\"port\":8080}}\
      \,{\"tag\":\"TERM\",\"contents\":1}\
      \,{\"tag\":\"TXT\",\"contents\":\"catchup\"}]"
    it "compact" $
      cen `shouldBe`
      "[{\"LI\":7777},{\"RID\":{\"_ridShard\":-3,\"_ridNonce\":-2}},{\"NID\":{\"fullAddr\":\"host:8080\",\"host\":\"host\",\"port\":8080}},{\"TERM\":1},{\"TXT\":\"catchup\"}]"
    it "textual" $
      ten `shouldBe`
      "[LogIndex 7777,RqId=s:-3,n:-2,NodeID host:8080,Term 1,TXT catchup]"
  describe "decode ByteString" $ do
    it "json"    $ jdc `shouldBe` Right LD.exampleLogList
    it "compact" $ cdc `shouldBe` Right LD.exampleLogList
    it "textual" $
      tdc `shouldBe`
      Left "cannot decode Textual: [LogIndex 7777,RqId=s:-3,n:-2,NodeID host:8080,Term 1,TXT catchup]"
