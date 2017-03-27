{-# LANGUAGE OverloadedStrings #-}

module Json
  ()
where

import           Blockchain             (Block (..))

import           Data.Aeson
import           Data.ByteString        (ByteString)
import           Data.ByteString.Base64 as BS64
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

-- https://github.com/bos/aeson/issues/187

instance ToJSON Block where
  toJSON (Block i ph t d h) =
    object [ "bindex"       .=                i
           , "previousHash" .= encodeToText64 ph
           , "timestamp"    .= encodeToText64 t
           , "bdata"        .= encodeToText64 d
           , "bhash"        .= encodeToText64 h
           ]

instance FromJSON Block where
  parseJSON (Object o) =
    Block <$>  o .: "bindex"
          <*> (o .: "previousHash" >>= decodeFromText64)
          <*> (o .: "timestamp"    >>= decodeFromText64)
          <*> (o .: "bdata"        >>= decodeFromText64)
          <*> (o .: "bhash"        >>= decodeFromText64)

encodeToText64   :: ByteString -> Text
encodeToText64    = decodeUtf8 . BS64.encode

decodeFromText64 :: (Monad m) => Text -> m ByteString
decodeFromText64  = either fail return . BS64.decode . encodeUtf8
