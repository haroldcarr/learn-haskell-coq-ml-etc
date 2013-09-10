import System.Posix.Files

-- import System.Time
import Data.Time.Clock

import Data.Time.Clock.POSIX

import System.Posix.Types

import Data.Convertible
import Data.Convertible.Instances.Time

getTimes :: FilePath -> IO (UTCTime, UTCTime, UTCTime)
getTimes fp = do
    stat <- getFileStatus fp
    return (toUTC (accessTime       stat),
            toUTC (modificationTime stat),
            toUTC (statusChangeTime stat))

toUTC :: EpochTime -> UTCTime
toUTC et = posixSecondsToUTCTime pt
    where pt = realToFrac et :: POSIXTime

toUTC' :: EpochTime -> UTCTime
toUTC' et = convert pt
    where pt = realToFrac et :: POSIXTime

-- End of file.
