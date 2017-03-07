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
site mvar = quickHttpServe $
  ifTop (writeBS "hello world") <|>
  route [ ("blocks",       showBlocks)
        , ("addBlock/:bd", addBlockReq mvar)
        ]

showBlocks :: Snap ()
showBlocks = writeBS (toStrict (encode [genesisBlock]))

addBlockReq :: MVar ByteString -> Snap ()
addBlockReq mvar = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> do liftIO $ putMVar mvar bd'
                    writeBS (toStrict (encode (generateNextBlock genesisBlock "timestamp" bd'))))
        bd
