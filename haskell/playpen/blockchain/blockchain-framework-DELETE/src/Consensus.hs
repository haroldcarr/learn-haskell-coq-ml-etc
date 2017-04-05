{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Consensus
  ( AppendEntry (..)
  , AppendEntryResponse (..)
  , ConsensusMessage
  , handleConsensusMessage
  , HandleConsensusMessage
  )
where

import           Blockchain           (Block, BlockData)
import           Logging

import           Data.Aeson           (FromJSON, ToJSON, decodeStrict, encode)
import           Data.ByteString      as BS
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid          ((<>))
import           GHC.Generics
import           Network.Socket       as N (HostName, PortNumber)
import           System.Log.Logger    (infoM)

data AppendEntry =
  AppendEntry { appendEntry :: Block
              }
  deriving (Eq, Generic, Show)

data AppendEntryResponse =
  AppendEntryResponse { appendEntryResponse :: Bool
                      , block               :: Maybe Block
                      }
  deriving (Eq, Generic, Show)

------------------------------------------------------------------------------

type ConsensusMessage = ByteString

type HandleConsensusMessage = HostName
                           -> PortNumber
                           -> (BlockData -> IO ())
                           -> (Block -> IO (Maybe String))
                           -> ConsensusMessage
                           -> IO ()

handleConsensusMessage :: HandleConsensusMessage
handleConsensusMessage host port sendToConsensusNodes isValid msg =
  if | BS.isPrefixOf "{\"appendEntry\":" msg -> do
         infoC host port "APPENDENTRY"
         case decodeStrict msg of
           Nothing ->     sendToConsensusNodes (toStrict (encode (AppendEntryResponse False Nothing)))
           Just (AppendEntry blk) -> do
             v <- isValid blk
             case v of
               Nothing -> sendToConsensusNodes (toStrict (encode (AppendEntryResponse True  (Just blk))))
               _       -> sendToConsensusNodes (toStrict (encode (AppendEntryResponse False (Just blk))))
     | BS.isInfixOf "\"appendEntryResponse\":" msg -> do
         infoC host port "APPENDENTRYRESPONSE"
         case decodeStrict msg of
           Just aer@(AppendEntryResponse _ _) -> infoC host port (show aer)
           Nothing                            -> infoC host port "AER NOT OK"
     | otherwise -> infoC host port ("handleMessage: unknown message: " ++ show msg)

------------------------------------------------------------------------------

instance ToJSON   AppendEntry
instance FromJSON AppendEntry
instance ToJSON   AppendEntryResponse
instance FromJSON AppendEntryResponse

------------------------------------------------------------------------------

infoC :: HostName -> PortNumber -> String -> IO ()
infoC h p msg =
  infoM consensus ("C " <> h <> ":" <> show p <> " " <> msg)

