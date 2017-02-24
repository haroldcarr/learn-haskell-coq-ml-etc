{-# LANGUAGE OverloadedStrings #-}

module SlidesSpec
  (spec)
where

import           Slides

import qualified Data.ByteString as BS
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  alist
  trie
  ptrie

alist =
  describe "AssocList" $ do
    let one   = aupdate []  (mkKey "C")     "one"
    let two   = aupdate one (mkKey "CA")    "two"
    let three = aupdate two (mkKey "CAF") "three"
    it "aupdate" $
      three `shouldBe` [([12,10,15],"three")
                       ,([12,10]   ,"two")
                       ,([12]      ,"one")]
    it "alookup" $
      alookup three (mkKey "CAF") `shouldBe` (Just "three")

trie =
  describe "Trie" $ do
    let cafe = tupdate TE   (mkKey "CAFE") "124AFE"
    let code = tupdate cafe (mkKey "C0DE") "04D21E"
    let cod  = tupdate code (mkKey "C0D")  "A374C8"
    let food = tupdate cod  (mkKey "F00D") "F4D71E"
    it "tupdate cafe" $
      cafe `shouldBe`
        TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
           ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
               ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                   ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "124AFE")
                       ,TE]
                     Nothing]
                   Nothing
               ,TE,TE,TE,TE,TE]
               Nothing
           ,TE,TE,TE]
           Nothing
    it "tupdate code" $
      code `shouldBe`
        TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
           ,TB [TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                   ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "04D21E")
                       ,TE]
                       Nothing
                   ,TE,TE]
                   Nothing
               ,TE,TE,TE,TE,TE,TE,TE,TE,TE
               ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                   ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "124AFE")
                       ,TE]
                       Nothing]
                   Nothing
               ,TE,TE,TE,TE,TE]
               Nothing
           ,TE,TE,TE]
           Nothing
    it "tupdate cod" $
      cod  `shouldBe`
        TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
           ,TB [TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                   ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "04D21E")
                       ,TE]
                       (Just "A374C8")
                   ,TE,TE]
                   Nothing
               ,TE,TE,TE,TE,TE,TE,TE,TE,TE
               ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                   ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "124AFE")
                       ,TE]
                       Nothing]
                   Nothing
               ,TE,TE,TE,TE,TE]
               Nothing
           ,TE,TE,TE]
           Nothing
    it "tupdate food" $
      food `shouldBe`
        TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
           -- C 0
           ,TB [TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                   -- D
                   ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       -- E
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "04D21E")
                       ,TE]
                       (Just "A374C8")
                   ,TE,TE]
                   Nothing
               ,TE,TE,TE,TE,TE,TE,TE,TE,TE
               -- A
               ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                   -- F
                   ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       -- E
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "124AFE")
                       ,TE]
                       Nothing]
                   Nothing
               ,TE,TE,TE,TE,TE]
               Nothing
           ,TE,TE
           -- F 0 0
           ,TB [TB [TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE
                       -- D
                       ,TB [TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                           (Just "F4D71E")
                       ,TE,TE]
                       Nothing
                   ,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
                   Nothing
               ,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE,TE]
               Nothing]
           Nothing
    it "tlookkup code food" $
      tlookup code (mkKey "F00D") `shouldBe` Nothing
    it "tlookkup food food" $
      tlookup food (mkKey "F00D") `shouldBe` (Just "F4D71E")

ptrie =
  describe "PTrie" $ do
    it "plookup 1" $
      plookup (PS (mkKey "F00D") (Right "F4D71E"))
              (mkKey "F00D")
        `shouldBe` Just "F4D71E"
    it "plookup 2" $
      plookup (PS (mkKey "F0") (Left (PS (mkKey "0D") (Right "F4D71E"))))
              (mkKey "F00D")
        `shouldBe` Just "F4D71E"

