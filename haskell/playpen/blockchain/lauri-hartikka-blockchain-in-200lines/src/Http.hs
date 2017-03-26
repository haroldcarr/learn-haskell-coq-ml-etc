{-# LANGUAGE OverloadedStrings #-}

module Http
  ( showBlocks
  , site
  )
where

import           Blockchain           (generateNextBlock, genesisBlock)
import           CommandDispatcher
import           Consensus            (AppendEntry (..))
import           Json                 ()
import           Logging              (http)

import           Control.Applicative  ((<|>))
import           Control.Concurrent   (MVar, putMVar)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid          ((<>))
import           Snap.Core            (Snap, getParam, ifTop, route, writeBS)
import           Snap.Http.Server     (Config, ConfigLog (ConfigNoLog),
                                       setAccessLog, setErrorLog, setPort,
                                       simpleHttpServe)
import           System.Log.Logger    (infoM)

site (CommandDispatcher listBlocks addBlock) host port = do
  let config = setErrorLog ConfigNoLog . setAccessLog ConfigNoLog $ setPort port mempty :: Config Snap ()
  simpleHttpServe config $
    ifTop (writeBS "hello world") <|>
    route [ ("blocks",       showBlocks listBlocks)
          , ("addBlock/:bd", addBlockReq host port addBlock)
          ]

showBlocks listBlocks = do
  blocks <- liftIO (listBlocks Nothing)
  writeBS (toStrict (encode blocks))

addBlockReq host port addBlock = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> do newBlock <- liftIO (addBlock bd')
                    liftIO (infoM http ("http: addBlockReq: " <> host <> " " <> show port <> " " <> show newBlock))
                    writeBS (toStrict (encode newBlock)))
        bd

