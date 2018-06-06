{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           RIO

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
