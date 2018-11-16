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

jsonEncoding,jsonCompactEncoding,showEncoding,showCompactEncoding :: BSL.ByteString
jsonEncoding        = LD.encode LD.JSON        LD.exampleLogList
jsonCompactEncoding = LD.encode LD.JSONCompact LD.exampleLogList
showEncoding        = LD.encode LD.Show        LD.exampleLogList
showCompactEncoding = LD.encode LD.ShowCompact LD.exampleLogList

jsonDecoding,jsonCompactDecoding,showDecoding,showCompactDecoding :: Either T.Text LD.LogList
jsonDecoding        = LD.decode LD.JSON        jsonEncoding
jsonCompactDecoding = LD.decode LD.JSONCompact jsonCompactEncoding
showDecoding        = LD.decode LD.Show        showEncoding
showCompactDecoding = LD.decode LD.ShowCompact        showEncoding

jsdv :: Maybe JS.Value
jsdv  = JS.decode jsonEncoding
lodv :: Maybe JS.Value
lodv  = JS.decode jsonCompactEncoding

spec :: Spec
spec = do
  describe "encode" $ do

    it "JSON" $
      jsonEncoding `shouldBe`
      "[{\"tag\":\"AT\",\"contents\":\"at\"},{\"tag\":\"CONSENSUS\",\"contents\":\"consensus\"},{\"tag\":\"EorE\",\"contents\":\"eore\"},{\"tag\":\"INFO\",\"contents\":\"info\"},{\"tag\":\"LI\",\"contents\":7777},{\"tag\":\"NETWORK\",\"contents\":\"network\"},{\"tag\":\"MSG_ID\",\"contents\":\"msg_id\"},{\"tag\":\"MSG_TYPE\",\"contents\":\"msg_type\"},{\"tag\":\"NID\",\"contents\":{\"fullAddr\":\"host:8080\",\"host\":\"host\",\"port\":8080}},{\"tag\":\"RECOVERY\",\"contents\":\"recovery\"},{\"tag\":\"RID\",\"contents\":{\"_ridShard\":-3,\"_ridNonce\":-2}},{\"tag\":\"ROLE\",\"contents\":\"Follower\"},{\"tag\":\"RPC_CATEGORY\",\"contents\":\"rpc_category\"},{\"tag\":\"RPC_TEXT\",\"contents\":\"rpc_text\"},{\"tag\":\"TERM\",\"contents\":1},{\"tag\":\"TO\",\"contents\":\"to\"},{\"tag\":\"TXT\",\"contents\":\"txt\"}]"

    it "JSONCompact" $
      jsonCompactEncoding `shouldBe`
      "[{\"AT\":\"at\"},{\"CONSENSUS\":\"consensus\"},{\"EorE\":\"eore\"},{\"INFO\":\"info\"},{\"LI\":7777},{\"NETWORK\":\"network\"},{\"MSG_ID\":\"msg_id\"},{\"MSG_TYPE\":\"msg_type\"},{\"NID\":{\"fullAddr\":\"host:8080\",\"host\":\"host\",\"port\":8080}},{\"RECOVERY\":\"recovery\"},{\"RID\":{\"_ridShard\":-3,\"_ridNonce\":-2}},{\"ROLE\":\"Follower\"},{\"RPC_CATEGORY\":\"rpc_category\"},{\"RPC_TEXT\":\"rpc_text\"},{\"TERM\":1},{\"TO\":\"to\"},{\"TXT\":\"txt\"}]"

    it "Show" $
      showEncoding `shouldBe`
      "[AT at,CONSENSUS consensus,EorE eore,INFO info,LogIndex 7777,NETWORK network,MSG_ID msg_id,MSG_TYPE msg_type,NodeID host:8080,RECOVERY recovery,RqId=s:-3,n:-2,ROLE Follower,RPC_CATEGORY rpc_category,RPC_TEXT rpc_text,Term 1,TO to,TXT txt]"

    it "ShowCompact" $
      showCompactEncoding `shouldBe`
      "at; consensus; eore; info; LogIndex 7777; network; msg_id; msg_type; NodeID host:8080; recovery; RqId=s:-3,n:-2; Follower; rpc_category; rpc_text; Term 1; to; txt"

  -------------------------

  describe "decode" $ do

    it "JSON"    $ jsonDecoding `shouldBe` Right LD.exampleLogList

    it "JSONCompact" $ jsonCompactDecoding `shouldBe` Right LD.exampleLogList

    it "Show" $
      showDecoding `shouldBe`
      Left "cannot decode Show: [AT at,CONSENSUS consensus,EorE eore,INFO info,LogIndex 7777,NETWORK network,MSG_ID msg_id,MSG_TYPE msg_type,NodeID host:8080,RECOVERY recovery,RqId=s:-3,n:-2,ROLE Follower,RPC_CATEGORY rpc_category,RPC_TEXT rpc_text,Term 1,TO to,TXT txt]"

    it "ShowCompact" $
      showCompactDecoding `shouldBe`
      Left "cannot decode ShowCompact: [AT at,CONSENSUS consensus,EorE eore,INFO info,LogIndex 7777,NETWORK network,MSG_ID msg_id,MSG_TYPE msg_type,NodeID host:8080,RECOVERY recovery,RqId=s:-3,n:-2,ROLE Follower,RPC_CATEGORY rpc_category,RPC_TEXT rpc_text,Term 1,TO to,TXT txt]"
