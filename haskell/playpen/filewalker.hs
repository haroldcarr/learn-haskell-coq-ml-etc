#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.14
  --package directory
  --package filepath
  --package unix
-}

import           Control.Monad      (foldM)
import           System.Directory   (listDirectory)
import           System.Environment (getArgs)
import           System.FilePath    ((</>))
import           System.Posix.Files (getFileStatus, isDirectory)

type Dir = FilePath
type File = FilePath

data FileTree = Dir :> [Either File FileTree]
  deriving Show

main :: IO ()
main = do
  [a] <- getArgs
  w <- walk a
  print w

walk :: Dir -> IO FileTree
walk dir = do
  paths' <- listDirectory dir
  let paths = fmap (dir </>) paths' -- need to qualify paths
  children <- foldM go [] paths
  pure (dir :> children)
 where
  go :: [Either File FileTree] -> FilePath -> IO [Either File FileTree]
  go acc filePath = do
    stat <- getFileStatus filePath
    if isDirectory stat
      then do
        tree <- walk filePath
        pure (Right tree : acc)
      else
        pure (Left filePath : acc)

