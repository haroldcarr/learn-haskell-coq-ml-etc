{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Parser
import           Tortellini
------------------------------------------------------------------------------
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

testDocAsMap :: IniAsNestedMap
testDocAsMap =
  [("section1", [("apple","banana")])
  ,("section2", [("kiwi","1"),("watermelon","true")])
  ,("section3", [])
  ,("section4", [])
  ]

main :: IO ()
main = hspec $ describe "Tortellini" $ do
  it "parseIniToNestedMap" $
    case parseIniToNestedMap testDoc of
      Left  e -> fail (show e)
      Right m -> m `shouldBe` testDocAsMap
  it "can parse a test document and has the right values" $
    case parseIni testDoc of
      Left  e -> fail (show e)
      Right Config {section1 = Section1 {apple}, section2 = Section2 {watermelon, kiwi}} -> do
        apple      `shouldBe` "banana"
        watermelon `shouldBe` True
        kiwi       `shouldBe` 1

{-
stack repl --test
:set -XAllowAmbiguousTypes
:set -XFlexibleContexts
x = parseIni' testDoc
x :: Rep Config x

<interactive>:28:1: error:
    • Couldn't match type ‘Rep record0’
                     with ‘M1
                             D
                             ('MetaData "Config" "Main" "main" 'False)
                             (C1
                                ('MetaCons "Config" 'PrefixI 'True)
                                ((S1
                                    ('MetaSel
                                       ('Just "section1")
                                       'NoSourceUnpackedness
                                       'NoSourceStrictness
                                       'DecidedLazy)
                                    (Rec0 Section1)
                                  :*: S1
                                        ('MetaSel
                                           ('Just "section2")
                                           'NoSourceUnpackedness
                                           'NoSourceStrictness
                                           'DecidedLazy)
                                        (Rec0 Section2))
                                 :*: (S1
                                        ('MetaSel
                                           ('Just "section3")
                                           'NoSourceUnpackedness
                                           'NoSourceStrictness
                                           'DecidedLazy)
                                        (Rec0 Section3)
                                      :*: S1
                                            ('MetaSel
                                               ('Just "section4")
                                               'NoSourceUnpackedness
                                               'NoSourceStrictness
                                               'DecidedLazy)
                                            (Rec0 Section4))))’
      Expected type: Rep Config x1
        Actual type: Rep record0 x1
      The type variable ‘record0’ is ambiguous
    • In the expression: x :: Rep Config x
      In an equation for ‘it’: it = x :: Rep Config x

------------------------------------------------------------------------------
:set -XOverloadedStrings
from (Config (Section1 "banana") (Section2 True 3) Section3 Section4)

M1 {unM1 = M1 {unM1 = (M1 {unM1 = K1 {unK1 = Section1 {apple = "banana"}}}
                          :*: M1 {unM1 = K1 {unK1 = Section2 {watermelon = True, kiwi = 3}}})
                      :*: (M1 {unM1 = K1 {unK1 = Section3}}
                              :*: M1 {unM1 = K1 {unK1 = Section4}})}}
------------------------------------------------------------------------------
:t (undefined :: Rep Config p)
(undefined :: Rep Config p)
  :: D1
       ('MetaData "Config" "Main" "main" 'False)
       (C1
          ('MetaCons "Config" 'PrefixI 'True)
          ((S1
              ('MetaSel
                 ('Just "section1")
                 'NoSourceUnpackedness
                 'NoSourceStrictness
                 'DecidedLazy)
              (Rec0 Section1)
            :*: S1
                  ('MetaSel
                     ('Just "section2")
                     'NoSourceUnpackedness
                     'NoSourceStrictness
                     'DecidedLazy)
                  (Rec0 Section2))
           :*: (S1
                  ('MetaSel
                     ('Just "section3")
                     'NoSourceUnpackedness
                     'NoSourceStrictness
                     'DecidedLazy)
                  (Rec0 Section3)
                :*: S1
                      ('MetaSel
                         ('Just "section4")
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
                      (Rec0 Section4))))
       p
-}
