{-# LANGUAGE OverloadedStrings #-}

module Blockchain
  ( Block (Block)
  , generateNextBlock
  , genesisBlock
    -- for testing
  , addBlock
  , bhash
  , calculateHash
  , isValidBlock
  , isValidChain
  , Timestamp
  , BlockData
  , Blockchain
  )
where

import           Control.Applicative    ((<|>))
import           Crypto.Hash.SHA256     (hash)
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.ByteString        as BS (ByteString, concat)
import           Data.ByteString.Base64 as BS64
import           Data.ByteString.Char8  as BSC8 (pack)
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

type Index      = Integer
type Hash       = ByteString
type Timestamp  = ByteString
type BlockData  = ByteString
type Blockchain = [Block]

data Block =
  Block { bindex       :: ! Index
        , previousHash :: ! Hash
        , timestamp    :: ! Timestamp
        , bdata        :: ! BlockData
        , bhash        :: ! Hash
        } deriving (Eq, Show)

calculateHash :: Index -> Hash -> Timestamp -> BlockData -> Hash
calculateHash i p t d = hash (BS.concat [BSC8.pack (show i), p, BSC8.pack (show t), d])

calculateHashForBlock :: Block -> Hash
calculateHashForBlock b = calculateHash (bindex b) (previousHash b) (timestamp b) (bdata b)

genesisBlock :: Block
genesisBlock =
  let i  = 0
      ph = "0"
      t  = "2017-03-05 10:49:02.084473 PST"
      d  = "GENESIS BLOCK"
      h  = calculateHash i ph t d
  in Block i ph t d h

generateNextBlock :: Block -> Timestamp -> BlockData -> Block
generateNextBlock previousBlock tstamp blockData =
  let i  = bindex previousBlock + 1
      ph = bhash previousBlock
  in Block i ph tstamp blockData (calculateHash i ph tstamp blockData)

-- | Returns Nothing if valid.
isValidBlock :: Block -> Block -> Maybe String
isValidBlock previousBlock newBlock
  | bindex previousBlock + 1       /= bindex newBlock       = Just "invalid index"
  | bhash previousBlock            /= previousHash newBlock = Just "invalid previousHash"
  | calculateHashForBlock newBlock /= bhash newBlock        = Just "invalid hash"
  | otherwise                                               = Nothing

-- | Returns Nothing if valid.
isValidChain :: Blockchain -> Maybe String
isValidChain (b:pb:bs) = isValidBlock pb b <|> isValidChain (pb:bs)
isValidChain      [_]  = Nothing
isValidChain       []  = Just "empty chain"

addBlock :: Block -> Blockchain -> Blockchain
addBlock b bs = b : bs

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
  parseJSON invalid    = typeMismatch "Block" invalid

encodeToText64   :: ByteString -> Text
encodeToText64    = decodeUtf8 . BS64.encode

decodeFromText64 :: (Monad m) => Text -> m ByteString
decodeFromText64  = either fail return . BS64.decode . encodeUtf8

