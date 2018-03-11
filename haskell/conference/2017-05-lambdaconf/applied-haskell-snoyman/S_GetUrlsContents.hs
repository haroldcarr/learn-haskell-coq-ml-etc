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

import qualified Control.Concurrent.Async        as Async
import qualified Control.Concurrent.STM          as STM
import qualified Control.Concurrent.STM.TBMQueue as TBMQ
import qualified Control.Exception.Safe          as CES
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import qualified Data.Text.Encoding.Error        as TEE
import qualified Network.HTTP.Simple             as HTTP
import qualified Say
import           System.Environment              (getArgs)
import qualified System.Process.Typed            as P

workerCount :: Int
workerCount = 8

main :: IO ()
main = do
  args <- getArgs
  main2 args

main2 :: [String] -> IO ()
main2 args = do
  urlQueue <- TBMQ.newTBMQueueIO $ workerCount * 2
  fillQueue args urlQueue `Async.concurrently_` drainQueue urlQueue

fillQueue :: [String] -> TBMQ.TBMQueue HTTP.Request -> IO ()
fillQueue args urlQueue = do
  Async.forConcurrently_ args $ \filepath -> do
    bs <- BS.readFile filepath
    let text = TE.decodeUtf8With TEE.lenientDecode bs
        ls = T.lines text
        addLine :: T.Text -> IO ()
        addLine line =
          case HTTP.parseRequest $ T.unpack line of
            Left e -> Say.say $ T.concat
              [ "While parsing URL: "
              , T.pack $ show line
              , ", encountered error message: "
              , T.pack $ show e
              ]
            Right req -> STM.atomically $ TBMQ.writeTBMQueue urlQueue req
    mapM_ addLine ls
  STM.atomically $ TBMQ.closeTBMQueue urlQueue

drainQueue :: TBMQ.TBMQueue HTTP.Request -> IO ()
drainQueue = Async.replicateConcurrently_ workerCount . drainQueueWorker

drainQueueWorker :: TBMQ.TBMQueue HTTP.Request -> IO ()
drainQueueWorker urlQueue = do
  mreq <- STM.atomically $ TBMQ.readTBMQueue urlQueue
  case mreq of
    Nothing -> return ()
    Just req -> do
      eres <- CES.tryAny $ HTTP.httpLBS req
      case eres of
        Left e -> Say.sayShow e
        Right res -> do
          handleResponse res
          drainQueueWorker urlQueue

handleResponse :: HTTP.Response BSL.ByteString -> IO ()
handleResponse res = do
  let pc = P.setStdin (P.byteStringInput (HTTP.getResponseBody res))
         $ P.proc "shasum" ["-a", "256"]
  P.runProcess_ pc
