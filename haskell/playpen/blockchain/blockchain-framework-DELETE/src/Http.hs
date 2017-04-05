{-# LANGUAGE OverloadedStrings #-}

module Http
  ( commandReceiver
  , showBlocks
  )
where

import           CommandDispatcher     (CommandDispatcher (CommandDispatcher))
import           Logging               (http)

import           Control.Applicative   ((<|>))
import           Control.Monad.Trans   (liftIO)
import           Data.Aeson            (ToJSON, encode)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (unpack)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Network.Socket        (HostName, PortNumber)
import           Snap.Core             (Snap, getParam, ifTop, route, writeBS)
import           Snap.Http.Server      (Config, ConfigLog (ConfigNoLog),
                                        setAccessLog, setErrorLog, setPort,
                                        simpleHttpServe)
import           Snap.Internal.Core    (MonadSnap)
import           System.Log.Logger     (infoM)
import           Text.Read             (readMaybe)

commandReceiver :: CommandDispatcher -> HostName -> PortNumber -> IO ()
commandReceiver (CommandDispatcher _ _ _ listBlocks addBlock _) host port = do
  let config = setErrorLog ConfigNoLog . setAccessLog ConfigNoLog $ setPort (fromEnum port) mempty :: Config Snap ()
  simpleHttpServe config $
    ifTop (writeBS "hello world") <|>
    route [ ("blocks/:i",    showBlocks listBlocks)
          , ("addBlock/:bd", addBlockReq host port addBlock)
          ]

showBlocks :: (ToJSON a, Read a1, MonadSnap m)
           => (Maybe a1 -> IO a)
           -> m ()
showBlocks listBlocks = do
  i <- getParam "i"
  maybe (writeBS "must specify index")
        (\i' -> case readMaybe (BSC8.unpack i') of
                  Nothing -> writeBS "index must be an int"
                  justI   -> do blocks <- liftIO (listBlocks justI)
                                writeBS (toStrict (encode blocks)))
        i

addBlockReq :: (ToJSON a, Show a, MonadSnap m)
            => HostName
            -> PortNumber
            -> (ByteString -> IO a)
            -> m ()
addBlockReq host port addBlock = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> do newBlock <- liftIO (addBlock bd')
                    liftIO (infoM http ("http: addBlockReq: " <> host <> " " <> show port <> " " <> show newBlock))
                    writeBS (toStrict (encode newBlock)))
        bd

