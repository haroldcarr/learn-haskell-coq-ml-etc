{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Run
  ( run
  )
where

import qualified Control.Monad.Component as CMC
import qualified Data.Aeson              as JSON
import qualified Data.Aeson.Types        as JSON
import qualified Data.FileEmbed          as DFE
import           RIO
import qualified RIO.Text                as T
import qualified System.Etc              as Etc
import qualified Text.Show.Pretty        as SP
------------------------------------------------------------------------------
import           Import

specBytes :: ByteString
specBytes = $(DFE.embedFile "./config/spec.yaml")

run :: IO ()
run = CMC.runComponentM "component-program" buildApplication $ \app ->
  runRIO app $
  logInfo "We're inside the application!"

buildApplication :: CMC.ComponentM App
buildApplication = do
  (config, _fileWarnings) <- buildConfig
  logFunc <- buildLogger config
  liftIO $ runRIO logFunc $ logInfo $ "Config: " <> display (T.pack (SP.ppShow config))
  return (App logFunc)

buildConfig :: CMC.ComponentM (Etc.Config, Vector SomeException)
buildConfig = CMC.buildComponent_ "buildConfig" $ do
  configSpec <- parseConfigSpec
  resolveConfigSpec configSpec

parseConfigSpec :: MonadThrow m => m (Etc.ConfigSpec ())
parseConfigSpec =
  case T.decodeUtf8' specBytes of
    Left  err    -> throwM err
    Right result -> Etc.parseConfigSpec result

resolveConfigSpec :: Etc.ConfigSpec () -> IO (Etc.Config, Vector SomeException)
resolveConfigSpec configSpec = do
  let defaultConfig           = Etc.resolveDefault  configSpec
  (fileConfig, fileWarnings) <- Etc.resolveFiles    configSpec
  envConfig                  <- Etc.resolveEnv      configSpec
  cliConfig                  <- Etc.resolvePlainCli configSpec
  return ( defaultConfig <> fileConfig <> envConfig <> cliConfig
         , fileWarnings )

--------------------------------------------------------------------------------
-- Logging

parseLogHandle :: JSON.Value -> JSON.Parser Handle
parseLogHandle = JSON.withText "IOHandle" $ \handleText ->
  if      handleText == "stdout" then return stdout
  else if handleText == "stderr" then return stderr
  else JSON.typeMismatch "IOHandle" (JSON.String handleText)

buildLogOptions :: Etc.Config -> IO LogOptions
buildLogOptions config = do
  handle0 <- Etc.getConfigValueWith parseLogHandle ["logging", "handle"] config
  logOptionsHandle handle0 True

buildLogger :: Etc.Config -> CMC.ComponentM LogFunc
buildLogger config = do
  logOptions      <- liftIO $ buildLogOptions config
  (logFunc, _) <- CMC.buildComponent "logger" (newLogFunc logOptions) snd
  return logFunc


