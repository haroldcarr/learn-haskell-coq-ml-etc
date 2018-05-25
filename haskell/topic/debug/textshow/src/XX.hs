{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module XX where

import qualified Language.Haskell.TH     as TH
import qualified TextShow.Debug.Trace.TH as TSTH
------------------------------------------------------------------------------
import           Ex

f :: Example -> Example
f = $(TSTH.makeTraceTextShowId (TH.mkName "Example"))
