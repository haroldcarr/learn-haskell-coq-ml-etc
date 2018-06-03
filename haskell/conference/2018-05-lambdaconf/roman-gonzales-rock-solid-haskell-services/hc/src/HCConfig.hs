{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module HCConfig where

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.FileEmbed   as DFE
import           RIO
import qualified RIO.Text         as Text
import qualified System.Etc       as Etc

specBytes :: ByteString
specBytes = $(DFE.embedFile "./config/spec.yaml")

parseConfigSpec :: MonadThrow m => m (Etc.ConfigSpec ())
parseConfigSpec =
  case Text.decodeUtf8' specBytes of
    Left err     -> throwM err
    Right result -> Etc.parseConfigSpec result

resolveConfigSpec :: Etc.ConfigSpec () -> IO (Etc.Config, Vector SomeException)
resolveConfigSpec configSpec = do
  let defaultConfig = Etc.resolveDefault configSpec
  (fileConfig, fileWarnings) <- Etc.resolveFiles configSpec
  envConfig <- Etc.resolveEnv configSpec
  cliConfig <- Etc.resolvePlainCli configSpec
  return ( defaultConfig <> fileConfig <> envConfig <> cliConfig
         , fileWarnings )

buildConfig :: IO (Etc.Config, Vector SomeException)
buildConfig = do
  configSpec <- parseConfigSpec
  resolveConfigSpec configSpec

--------------------------------------------------------------------------------
-- Logging

parseLogHandle :: JSON.Value -> JSON.Parser Handle
parseLogHandle = JSON.withText "IOHandle" $ \handleText ->
  if handleText == "stdout" then
    return stdout
  else if handleText == "stderr" then
    return stderr
  else
    JSON.typeMismatch "IOHandle" (JSON.String handleText)

buildLogOptions :: Etc.Config -> IO LogOptions
buildLogOptions config = do
  handle0 <- Etc.getConfigValueWith parseLogHandle ["logging", "handle"] config
  logOptionsHandle handle0 True



