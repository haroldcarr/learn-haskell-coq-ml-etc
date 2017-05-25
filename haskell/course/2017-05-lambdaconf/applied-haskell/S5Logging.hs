#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module S5Logging where

import           Control.Exception.Safe         (onException)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Logger.CallStack (logDebug, logError, logInfo,
                                                 runStdoutLoggingT)
import qualified Data.Text                      as T (pack)

s5 = main

main :: IO ()
main = runStdoutLoggingT $ do
  let fp = "/tmp/JUNK/S5Logging"
  logInfo $ T.pack $ "Writing to file: " ++ fp
  liftIO (writeFile fp "Hey there!") `onException`
    logError "Writing to file failed"
  logInfo $ T.pack $ "Reading from file: " ++ fp
  content <- liftIO (readFile fp) `onException`
    logError "Reading from file failed"
  logDebug $ T.pack $ "Content read: " ++ content
