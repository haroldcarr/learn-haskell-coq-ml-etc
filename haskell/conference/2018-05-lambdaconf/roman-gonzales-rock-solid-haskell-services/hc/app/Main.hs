{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Run
import RIO.Process
------------------------------------------------------------------------------
import HCConfig

main :: IO ()
main = do
  (config, _fileWarnings) <- buildConfig
  logOptions <- buildLogOptions config
  pc <- mkDefaultProcessContext
  withLogFunc logOptions $ \logFunc ->
    let app = App { appLogFunc = logFunc
                  , appProcessContext = pc
                  }
     in runRIO app (run config)
