{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Trie.PathSpec
  (spec)
where

import           PPrelude
import           Trie.Path

import           Testing

import           Data.Aeson
import qualified Data.ByteString as BS
import           GHC.Generics
import           Prelude         hiding (seq)

import           Test.Hspec
import           Test.QuickCheck

data HexCase = HexCase
  { seq  :: Path
  , term :: Bool
  , out  :: ByteString
  } deriving (Show, Generic)

instance FromJSON Word4 where
  parseJSON x = sndWord4 <$> parseJSON x

instance FromJSON HexCase

spec :: Spec
spec = do
  it "should roundtrip paths" $ property $ \x y ->
    decodePath (encodePath x y) `shouldBe` Just (x, y)

  it "should recover the first nibble" $ property $ \x y ->
    fstWord4 (x `packWord8` y) `shouldBe` x

  it "should recover the second nibble" $ property $ \x y ->
    sndWord4 (x `packWord8` y) `shouldBe` y

  describe "performs example conversions" $ testCommon "hexencodetest" $ \test -> do
    it ("should encode to " ++ show (out test)) $
      let encoded = encodePath (term test) (seq test)
      in encoded `shouldBe` out test
    it ("should decode " ++ show (out test)) $
      let decoded = decodePath (out test)
      in decoded `shouldBe` Just (term test, seq test)
