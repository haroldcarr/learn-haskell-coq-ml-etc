module CompositionVizPurity where

import           System.IO

g :: Int -> IO Int
g a = do
    h <- openFile "/tmp/JUNK" ReadMode
    v <- hGetLine h
    hClose h
    return (a + read v)

f :: Int -> IO Int
f b = do
    let result = b + 3
    h <- openFile "/tmp/JUNK" WriteMode
    hPutStr h (show result)
    hClose h
    return result

{-
(return 1) >>= g >>= f
-}

-- End
