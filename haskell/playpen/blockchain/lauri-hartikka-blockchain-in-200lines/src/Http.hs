{-# LANGUAGE OverloadedStrings #-}

module Http where

import           Blockchain
import           Json

import           Control.Applicative  ((<|>))
import           Data.Aeson           (encode)
import           Data.ByteString.Lazy (toStrict)
import           Snap.Core
import           Snap.Http.Server

site :: IO ()
site = quickHttpServe $
  ifTop (writeBS "hello world") <|>
  route [ ("blocks",       showBlocks)
        , ("addBlock/:bd", addBlockReq)
        ]

showBlocks :: Snap ()
showBlocks = writeBS (toStrict (encode [genesisBlock]))

addBlockReq :: Snap ()
addBlockReq = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> writeBS (toStrict (encode (generateNextBlock genesisBlock "timestamp" bd'))))
        bd
