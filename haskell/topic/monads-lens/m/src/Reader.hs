{-# LANGUAGE FlexibleContexts #-}

module Reader where

import Control.Monad.Reader

mult :: Reader [String] [String]
mult = do
  a <- ask
  b <- asks (["XXX"]++)
  return (a++b)

doit = runReader mult ["X","y","ZZ"]
