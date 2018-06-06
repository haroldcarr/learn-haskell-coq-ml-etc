{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run
  ( run
  )
where

import qualified RIO.Text                as T
import qualified Control.Monad.Component as CMC
import qualified Text.Show.Pretty        as SP
------------------------------------------------------------------------------
import           HCConfig
import           Import

buildApplication :: CMC.ComponentM App
buildApplication = do
  (config, _fileWarnings) <- buildConfig
  logFunc <- buildLogger config
  liftIO $ runRIO logFunc $
    logInfo $ "Config: " <> display (T.pack (SP.ppShow config))
  return (App logFunc)

run :: IO ()
run = CMC.runComponentM "component-program" buildApplication $ \app ->
  runRIO app $
  logInfo "We're inside the application!"

