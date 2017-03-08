{-# LANGUAGE OverloadedStrings #-}

module Http where

import           Blockchain
import           Json

import           Control.Applicative  ((<|>))
import           Control.Concurrent   (MVar, putMVar)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Snap.Core
import           Snap.Http.Server

site :: MVar ByteString -> IO ()
site httpToConsensus = quickHttpServe $
  ifTop (writeBS "hello world") <|>
  route [ ("blocks",       showBlocks)
        , ("addBlock/:bd", addBlockReq httpToConsensus)
        ]

showBlocks :: Snap ()
showBlocks = writeBS (toStrict (encode [genesisBlock]))

addBlockReq :: MVar ByteString -> Snap ()
addBlockReq httpToConsensus = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> do let newBlock = generateNextBlock genesisBlock "timestamp" bd'
                    liftIO (sendAppendEntries httpToConsensus newBlock)
                    writeBS (toStrict (encode newBlock)))
        bd

sendAppendEntries httpToConsensus block = do
  putMVar httpToConsensus (toStrict (encode (AppendEntry block)))
