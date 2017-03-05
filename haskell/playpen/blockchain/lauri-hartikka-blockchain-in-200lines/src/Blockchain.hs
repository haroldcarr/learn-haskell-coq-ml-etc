{-# LANGUAGE OverloadedStrings #-}

-- https://medium.com/@lhartikk/a-blockchain-in-200-lines-of-code-963cc1cc0e54#.ufqde5iq6

module Blockchain where

import           Control.Applicative
import           Crypto.Hash.SHA256
import           Data.ByteString       as BS
import           Data.ByteString.Char8 as BSC8
import           GHC.Generics

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
      hash         = (calculateHash index previousHash timestamp bdata)
  in Block index previousHash timestamp bdata hash

generateNextBlock :: Block -> Timestamp -> BlockData -> Block
generateNextBlock previousBlock timestamp blockData =
  let index        = (bindex previousBlock) + 1
      previousHash = bhash previousBlock
  in Block index previousHash timestamp blockData (calculateHash index previousHash timestamp blockData)

-- | Returns Nothing if valid.
isValidBlock :: Block -> Block -> Maybe String
isValidBlock previousBlock newBlock =
  if      (bindex previousBlock) + 1       /= bindex newBlock       then Just "invalid index"
  else if bhash previousBlock              /= previousHash newBlock then Just "invalid previousHash"
  else if (calculateHashForBlock newBlock) /= bhash newBlock        then Just "invalid hash"
  else                                                                   Nothing

emptyBlockchain :: Blockchain
emptyBlockchain = []

addBlock :: Block -> Blockchain -> Blockchain
addBlock b bs = b : bs

-- | Returns Nothing if valid.
isValidChain :: Blockchain -> Maybe String
isValidChain (b:pb:bs) = isValidBlock pb b <|> isValidChain (pb:bs)
isValidChain   (gb:[]) = Nothing
isValidChain       []  = Just "empty chain"
