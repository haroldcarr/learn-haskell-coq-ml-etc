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

import           Control.Applicative   ((<|>))
import           Crypto.Hash.SHA256    (hash)
import           Data.ByteString       (ByteString)
import           Data.ByteString       as BS (concat)
import           Data.ByteString.Char8 as BSC8 (pack)

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
  let index = bindex previousBlock + 1
      pHash = bhash previousBlock
  in Block index pHash tstamp blockData (calculateHash index pHash tstamp blockData)

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
