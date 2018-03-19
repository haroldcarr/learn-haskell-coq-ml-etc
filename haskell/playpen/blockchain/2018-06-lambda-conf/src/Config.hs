{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Network                              as N
import qualified Network.Wai.Handler.Warp             as Wai
import           RIO

data Config = Config
  { cHost       :: N.HostName
  , cTxPort     :: N.PortID
  , cHttpPort   :: Wai.Port
  , cNumClients :: Int
  , cDOSEnabled :: Bool
  , cLogFuncL   :: RIO.LogFunc
  }

class HasConfig env where
  getConfig :: env -> Config
instance HasConfig Config where
  getConfig = id
instance HasLogFunc Config where
  logFuncL = RIO.lens cLogFuncL (\c l -> c { cLogFuncL = l })

defaultConfig :: LogFunc -> Config
defaultConfig lf = Config
  { cHost       = "localhost"
  , cTxPort     = N.PortNumber 44444
  , cHttpPort   = 3000
  , cNumClients = 8
  , cDOSEnabled = False -- True -- False
  , cLogFuncL   = lf
  }
