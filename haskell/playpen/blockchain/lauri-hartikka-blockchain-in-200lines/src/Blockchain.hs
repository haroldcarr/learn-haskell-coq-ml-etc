{-# LANGUAGE OverloadedStrings #-}

-- https://medium.com/@lhartikk/a-blockchain-in-200-lines-of-code-963cc1cc0e54#.ufqde5iq6

module Blockchain where

import           Control.Applicative   ((<|>))
import           Crypto.Hash.SHA256    (hash)
import           Data.ByteString       (ByteString)
import           Data.ByteString       as BS
import           Data.ByteString.Char8 as BSC8

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
  let index        = 0
      previousHash = "0"
      timestamp    = "2017-03-05 10:49:02.084473 PST"
      bdata        = "GENESIS BLOCK"
      hash         = calculateHash index previousHash timestamp bdata
  in Block index previousHash timestamp bdata hash

generateNextBlock :: Block -> Timestamp -> BlockData -> Block
generateNextBlock previousBlock timestamp blockData =
  let index        = bindex previousBlock + 1
      previousHash = bhash previousBlock
  in Block index previousHash timestamp blockData (calculateHash index previousHash timestamp blockData)

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

emptyBlockchain :: Blockchain
emptyBlockchain = []

addBlock :: Block -> Blockchain -> Blockchain
addBlock b bs = b : bs
