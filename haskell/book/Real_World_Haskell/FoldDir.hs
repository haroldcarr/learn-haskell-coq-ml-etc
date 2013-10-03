-- import Char (toLower)
import Data.Char (toLower)

import Control.Monad (filterM, forM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents, Permissions(..), getModificationTime, getPermissions)

-- import System.Time (ClockTime(..)) -- deprecated
import Data.Time.Clock

import System.FilePath (takeExtension, takeFileName, (</>))
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import ControlledVisit

-- p. 230/270

-- Think of filesystem traversal as a fold over the directory hierarchy.
-- Iterate controls our fold.

data Iterate seed = Done     { unwrap :: seed } -- cease and return unwrap
                  | Skip     { unwrap :: seed } -- do not recurse
                  | Continue { unwrap :: seed } -- use "unwrap" as input to the next call of fold function
                    deriving (Show)

-- alias or the function that we fold with.
type Iterator seed = seed -> Info -> Iterate seed

-- Logically a left fold.
-- The seed for each step is the result of the prior step.

foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    -- both walk and fold return a seed wrapped in an Iterate.
    fold seed subpath = getUsefulContents subpath >>= walk seed
    -- walk is tail recursive (instead of calling forM) so we can stop anytime.
    walk seed (name:names) = do
        let path' = path </> name
        info <- getInfo path'
        case iter seed info of
            done@(Done _)   -> return done
            Skip seed'      -> walk seed' names
            Continue seed'
                | isDirectory info -> do
                    next <- fold seed' path'
                    case next of
                        done@(Done _) -> return done
                        seed''        -> walk (unwrap seed'') names
                | otherwise        -> walk seed' names
    walk seed _ = return (Continue seed)

-- p. 231/271

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
    | length paths == 3
        = Done paths
    | isDirectory info && takeFileName path == ".svn"
        = Skip paths
    | extension `elem` [".jpg", ".png"]
        = Continue (path : paths)
    | otherwise
        = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info

countDirectories count info =
    Continue (if isDirectory info
                  then count + 1
                  else count)
