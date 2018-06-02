{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import           Import
import           RIO
import qualified RIO.ByteString.Lazy as BSL
import           RIO.Process
import qualified RIO.Text            as T
import           System.Environment
import           System.Exit

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  args <- liftIO getArgs
  case args of
    [dir,pat] ->
      withWorkingDir dir $ do
        (o,_e) <- proc "ls" ["-alF"] readProcess_
        case decodeUtf8' (BSL.toStrict o) of -- TODO toStrict
          Left  e  -> do
            logError (displayBytesUtf8 "decode error: " <> displayShow e)
            liftIO exitFailure
          Right o' ->
            logInfo (displayBytesUtf8
                     (encodeUtf8
                      (T.unlines
                        (RIO.filter (T.isSuffixOf (T.pack pat)) (T.lines o')))))
    _ -> do
      logError "provide a directory path"
      liftIO exitFailure
