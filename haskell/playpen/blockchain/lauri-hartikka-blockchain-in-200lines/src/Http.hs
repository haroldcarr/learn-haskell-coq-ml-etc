{-# LANGUAGE OverloadedStrings #-}

module Http
  (site)
where

import           Blockchain           (generateNextBlock, genesisBlock)
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

site httpToConsensus host port = do
  let config = setErrorLog ConfigNoLog . setAccessLog ConfigNoLog $ setPort port mempty :: Config Snap ()
  simpleHttpServe config $
    ifTop (writeBS "hello world") <|>
    route [ ("blocks",       showBlocks)
          , ("addBlock/:bd", addBlockReq host port httpToConsensus)
          ]

showBlocks = writeBS (toStrict (encode [genesisBlock]))

addBlockReq host port httpToConsensus = do
  bd <- getParam "bd"
  maybe (writeBS "must specify data")
        (\bd' -> do let newBlock = generateNextBlock genesisBlock "timestamp" bd'
                    liftIO (infoM http ("http: addBlockReq: " <> host <> " " <> show port <> " " <> show newBlock))
                    -- send block to verifiers
                    liftIO (sendAppendEntries httpToConsensus newBlock)
                    -- return block to caller
                    writeBS (toStrict (encode newBlock)))
        bd

sendAppendEntries httpToConsensus block =
  putMVar httpToConsensus (toStrict (encode (AppendEntry block)))
