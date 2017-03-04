-- {-# OPTIONS -fglasgow-exts #-}
module Main where

import           Graphics.UI.WX
import           NetEdit
import           System.Environment (getArgs)

main = do
  as <- getArgs
  case as of
    (f:_) -> start $ editFrame (Just f)
    _     -> start $ editFrame Nothing
