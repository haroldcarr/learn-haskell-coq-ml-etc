{-# LANGUAGE OverloadedStrings #-}

module Http
  ( showBlocks
  , site
  )
where

import           Blockchain            (generateNextBlock, genesisBlock)
import           CommandDispatcher
import           Consensus             (AppendEntry (..))
import           Json                  ()
import           Logging               (http)

import           Control.Applicative   ((<|>))
import           Control.Concurrent    (MVar, putMVar)
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (encode)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (unpack)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Snap.Core             (Snap, getParam, ifTop, route, writeBS)
import           Snap.Http.Server      (Config, ConfigLog (ConfigNoLog),
                                        setAccessLog, setErrorLog, setPort,
                                        simpleHttpServe)
import           System.Log.Logger     (infoM)
import           Text.Read             (readMaybe)

site (CommandDispatcher listBlocks addBlock) host port = do
  let config = setErrorLog ConfigNoLog . setAccessLog ConfigNoLog $ setPort port mempty :: Config Snap ()
  simpleHttpServe config $
    ifTop (writeBS "hello world") <|>
    route [ ("blocks/:i",    showBlocks listBlocks)
          , ("addBlock/:bd", addBlockReq host port addBlock)
          ]

showBlocks listBlocks = do
  i <- getParam "i"
  maybe (writeBS "must specify index")
        (\i' -> case readMaybe (BSC8.unpack i') of
                  Nothing -> writeBS "index must be an int"
                  justI   -> do blocks <- liftIO (listBlocks justI)
                                writeBS (toStrict (encode blocks)))
        i

addBlockReq host port addBlock = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> do newBlock <- liftIO (addBlock bd')
                    liftIO (infoM http ("http: addBlockReq: " <> host <> " " <> show port <> " " <> show newBlock))
                    writeBS (toStrict (encode newBlock)))
        bd

