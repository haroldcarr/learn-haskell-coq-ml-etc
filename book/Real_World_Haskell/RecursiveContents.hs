module RecursiveContents (getRecursiveContents) where

-- forM is like mapM, but args reversed (i.e., list first, the function)
import Control.Monad (forM)
-- getDirectoryContents does not recurse
import System.Directory (doesDirectoryExist, getDirectoryContents)
-- system-independent path separator
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)

