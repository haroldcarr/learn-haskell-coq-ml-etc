# Tortellini

A Haskell version of [PureScript-Tortellini](https://github.com/justinwoo/purescript-tortellini) using GHC8 Generics with similar characteristics.

## Why

I found that existing solutions for ini/other config formats give you a nested HashMap to represent data, whereas what you normally want is a record of records of fields. Now instead of dealing with all of the possible missing/existing values and the branching factor of maps, you can parse ini files into records.

## Tl;dr

```hs

data Config = Config
  { section1 :: Section1
  , section2 :: Section2
  , section3 :: Section3
  , section4 :: Section4
  } deriving (Show, Eq, Generic)

data Section1 = Section1
  { apple :: Text
  } deriving (Show, Eq, Generic)
data Section2 = Section2
  { watermelon :: Bool
  , kiwi :: Int
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
  
-- ...
  describe "Tortellini" $
    it "can parse a test document and has the right values" $
      case parseIni testDoc of
        Left e -> fail (show e)
        Right Config {section1 = Section1 {apple}, section2 = Section2 {watermelon, kiwi}} -> do
          apple `shouldBe` "banana"
          watermelon `shouldBe` True
          kiwi `shouldBe` 1
```
