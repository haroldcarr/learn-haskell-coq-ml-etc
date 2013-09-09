import System.Posix.Files

-- import System.Time
import Data.Time.Clock

import Data.Time.Clock.POSIX

import System.Posix.Types

-- getTimes :: FilePath -> IO (UTCTime, UTCTime, UTCTime)
getTimes fp = do
    stat <- getFileStatus fp
    return (xx (accessTime       stat),
            xx (modificationTime stat),
            xx (statusChangeTime stat))

xx i = i

-- Converts an Epoch time represented with an arbitrary Real to a ClockTime.
-- This input could be a CTime from Foreign.C.Types or an EpochTime from
-- System.Posix.Types.
epochToClockTime :: Real a => a -> ClockTime
epochToClockTime x =
    TOD seconds secfrac
    where ratval = toRational x
          seconds = floor ratval
          secfrac = floor $ (ratval - (seconds % 1) ) * picosecondfactor
          picosecondfactor = 10 ^ 12

-- End of file.
