#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module S8AesonBytestring where

import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as BSL (ByteString)
import           Data.Text.Encoding

newtype B64 = B64 ByteString deriving Show

instance ToJSON B64 where
  toJSON (B64 bs) = toJSON $ decodeUtf8 $ B64.encode bs
instance FromJSON B64 where
  parseJSON = withText "B64" $ \t ->
    case B64.decode $ encodeUtf8 t of
      Left _  -> fail "Invalid base 64 encoding"
      Right x -> return $ B64 x

s8 :: Either String B64
s8 = eitherDecode "\"fff\""

mybs :: B64
mybs = B64 "hello"

s8' :: BSL.ByteString
s8' = encode mybs

s8'' :: Maybe B64
s8'' = decode s8'

main :: IO ()
main = print (s8, s8', s8'')
