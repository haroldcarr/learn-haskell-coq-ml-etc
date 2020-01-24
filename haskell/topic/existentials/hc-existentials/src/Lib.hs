{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

------------------------------------------------------------------------------
import qualified Data.Text  as T
import           Protolude
import           Test.Hspec
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- NO information preserved

data List1 where
  Nil1  ::               List1
  Cons1 :: a -> List1 -> List1

-- do not need to know 'a' to compute length, tail, ...

length1 :: List1 -> Int
length1  = \case
  Nil1       -> 0
  Cons1 _ xs -> 1 + length1 xs

tail1 :: List1 -> Maybe List1
tail1  = \case
  Nil1       -> Nothing
  Cons1 _ xs -> Just xs

-- workaround since == cannot be defined since 'a' is unknown
tailLength1 :: List1 -> Int
tailLength1 xs = case tail1 xs of
  Nothing  -> -1
  Just xs' -> length1 xs'

t01 :: Spec
t01  = do
  it "t01a" $ length1     Nil1                       `shouldBe` 0
  it "t01b" $ length1     (Cons1 1 (Cons1 "2" Nil1)) `shouldBe` 2
  it "t01c" $ tailLength1 Nil1                       `shouldBe` -1
  it "t01b" $ tailLength1 (Cons1 1 (Cons1 2.0 Nil1)) `shouldBe` 1

-- possible types for 'a' is open : can accept any 'a'

------------------------------------------------------------------------------
-- constraints preserved

data List2 where
  Nil2  ::               List2
  Cons2 :: Show a
        => a -> List2 -> List2

-- when pattern matching on 'Cons2' can use any functions from constraint typeclass

showHead2 :: List2 -> Maybe Text
showHead2  = \case
  Nil2      -> Nothing
  Cons2 h _ -> Just (show h)

t02 :: Spec
t02  = do
  it "t02a" $ showHead2 Nil2                       `shouldBe` Nothing
  it "t02b" $ showHead2 (Cons2 1 (Cons2 "2" Nil2)) `shouldBe` Just "1"

-- possible types for 'a' is open : can defined new instances of 'Show'

------------------------------------------------------------------------------
-- all info preserved

data EType a where
  ETypeInt   :: EType Int
  ETypeText  :: EType Text

data List3 where
  Nil3  ::                          List3
  Cons3 :: EType a -> a -> List3 -> List3

-- pattern matching on 'EType' "reveals" 'a'

fold3 :: List3 -> Text
fold3  = \case
  Nil3                 -> "Nil3"
  Cons3 ETypeInt  x xs -> show (x * 10) <> " " <> fold3 xs
  Cons3 ETypeText x xs -> T.toUpper x   <> " " <> fold3 xs

t03 :: Spec
t03 =
  it "t03" $ fold3 (Cons3 ETypeInt 1 (Cons3 ETypeText "two" Nil3))
    `shouldBe` "10 TWO Nil3"

-- but 'a' still cannot escape scope
{-
head3 :: List3 -> Maybe a
head3  = \case
  Nil3 -> Nothing
  Cons3 ETypeInt  x _ -> Just x
  Cons3 ETypeText x _ -> Just x

    • Could not deduce: a ~ Int
      from the context: a1 ~ Int
-}

-- one way is to return a concrete type (not 'a')

headIfInt3 :: List3 -> Maybe Int
headIfInt3  = \case
  Cons3 ETypeInt x _ -> Just x
  _                  -> Nothing

t04 :: Spec
t04  = do
  it "t04a" $ headIfInt3 (Cons3 ETypeInt   1  (Cons3 ETypeText "two" Nil3))
    `shouldBe` Just 1
  it "t04b" $ headIfInt3 (Cons3 ETypeText "1" (Cons3 ETypeText "two" Nil3))
    `shouldBe` Nothing

------------------------------------------------------------------------------
-- OOP

data Object where
  Object :: Num a => -- Num needed to make 'update' work
    { oA  :: a
    , oF1 :: a -> Bool
    , oF2 :: a -> Text } -> Object

evalF1 :: Object -> Bool
evalF1 (Object a f1  _) = f1 a

evalF2 :: Object -> Text
evalF2 (Object a  _ f2) = f2 a

update :: Object -> Object
update (Object a f1 f2) = Object (a * 10) f1 f2
-- update o@(Object a f1 f2) = o { oA = a * 10 }
-- cannot use record update syntax due to
-- GHC error : "Record update for insufficiently polymorphic field"

oop :: Object
oop  = Object 1 even show

t05 :: Spec
t05  = do
  it "t05a" $ evalF1         oop  `shouldBe` False
  it "t05b" $ evalF2         oop  `shouldBe` "1"
  it "t05c" $ evalF2 (update oop) `shouldBe` "10"
