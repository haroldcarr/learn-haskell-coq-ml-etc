#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}

{-

* Read a list of file names from the command line
* Parse each file into lines, treat each line as a URL
    * Line could be an invalid URL, then you print an error message
* Fill up a job queue with the URLs
* Have 8 workers consuming from the job queue
* Download the contents of the URL, and pass it to sha256sum

-}

module S_GetUrlsContents where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Exception.Safe
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8With)
import           Data.Text.Encoding.Error        (lenientDecode)
import           Network.HTTP.Simple
import           Say
import           System.Environment              (getArgs)
import           System.Process.Typed

workerCount :: Int
workerCount = 8

main :: IO ()
main = do
  urlQueue <- newTBMQueueIO $ workerCount * 2
  fillQueue urlQueue `concurrently_` drainQueue urlQueue

fillQueue :: TBMQueue Request -> IO ()
fillQueue urlQueue = do
  args <- getArgs
  forConcurrently_ args $ \filepath -> do
    bs <- B.readFile filepath
    let text = decodeUtf8With lenientDecode bs
        ls = T.lines text
        addLine :: T.Text -> IO ()
        addLine line =
          case parseRequest $ T.unpack line of
            Left e -> say $ T.concat
              [ "While parsing URL: "
              , T.pack $ show line
              , ", encountered error message: "
              , T.pack $ show e
              ]
            Right req -> atomically $ writeTBMQueue urlQueue req
    mapM_ addLine ls
  atomically $ closeTBMQueue urlQueue

drainQueue :: TBMQueue Request -> IO ()
drainQueue = replicateConcurrently_ workerCount . drainQueueWorker

drainQueueWorker :: TBMQueue Request -> IO ()
drainQueueWorker urlQueue = do
  mreq <- atomically $ readTBMQueue urlQueue
  case mreq of
    Nothing -> return ()
    Just req -> do
      eres <- tryAny $ httpLBS req
      case eres of
        Left e -> sayShow e
        Right res -> do
          handleResponse res
          drainQueueWorker urlQueue

handleResponse :: Response BL.ByteString -> IO ()
handleResponse res = do
  let pc = setStdin (byteStringInput (getResponseBody res))
         $ proc "shasum" ["-a", "256"]
  runProcess_ pc
