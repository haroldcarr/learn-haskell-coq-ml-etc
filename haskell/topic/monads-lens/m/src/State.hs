{-# LANGUAGE FlexibleContexts #-}

module State where

import Control.Monad.State
import Data.Map as M

s :: (String, Map String Int)
s = runState f M.empty

f :: State (Map String Int) String
f = do
  m <- get
  put (M.insert "H" 6 m)
  return "C"
