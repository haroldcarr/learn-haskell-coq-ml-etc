{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Network                  as N
import qualified Network.Wai.Handler.Warp as Wai
import           RIO

data Config = Config
  { cHost           :: N.HostName
  , cTxPort         :: N.PortID
  , cHttpPort       :: Wai.Port
  , cNumMiners      :: Int
  , cNumClients     :: Int
  , cDOSEnabled     :: Bool
  , cDOSDelay       :: Int --  seconds
  , cDOSRandomRange :: (Int, Int)
  , cDOSRandomHit   :: Int
  }

class HasConfig env where
  getConfig :: env -> Config
instance HasConfig Config where
  getConfig = id

type Env e = (HasConfig e)

defaultConfig :: Config
defaultConfig = Config
  { cHost           = "localhost"
  , cTxPort         = N.PortNumber 44444
  , cHttpPort       = 3000
  , cNumMiners      = 8
  , cNumClients     = 8
  , cDOSEnabled     = True -- False
  , cDOSDelay       = 15
  , cDOSRandomRange = (1,10)
  , cDOSRandomHit   = 10
  }
