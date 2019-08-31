{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Tortellini
import           Parser
------------------------------------------------------------------------------
import           Data.Either
import           Data.Text
import           GHC.Generics
import           Test.Hspec

-- to use 'parseIni', define concrete record types for each section and the top level document

data Config = Config
  { section1 :: Section1
  , section2 :: Section2
  , section3 :: Section3
  , section4 :: Section4
  } deriving (Show, Eq, Generic)

newtype Section1 = Section1 { apple :: Text } deriving (Show, Eq, Generic)

data Section2 = Section2
  { watermelon :: Bool
  , kiwi       :: Int
  } deriving (Show, Eq, Generic)

data Section3 = Section3 {} deriving (Show, Eq, Generic)
data Section4 = Section4 {} deriving (Show, Eq, Generic)

testDoc :: Text
testDoc = intercalate "\n"
  [ "[section1]"
  , "apple=banana"
  , "[section2]"
  , "watermelon=true"
  , "kiwi=1"
  , "[section3]"
  , "[section4]"
  ]

main :: IO ()
main = hspec $ do
  describe "Tortellini.Parser" $
    it "can parse a test document" $ isRight $ parseIniDocument testDoc
  describe "Tortellini" $
    it "can parse a test document and has the right values" $
      case parseIni testDoc of
        Left e -> fail (show e)
        Right Config {section1 = Section1 {apple}, section2 = Section2 {watermelon, kiwi}} -> do
          apple      `shouldBe` "banana"
          watermelon `shouldBe` True
          kiwi       `shouldBe` 1
