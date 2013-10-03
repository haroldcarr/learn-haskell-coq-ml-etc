import Data.List (isPrefixOf)

-- treat an entire file as a string (external to this function)
-- split it up with lines
-- step operates on a single line
-- collect the word following the prefix

dlts :: String -> String -> [String]
dlts prefix = foldr step [] . lines
    where step l ds | prefix `isPrefixOf` l = secondWord l : ds
                    | otherwise             = ds
          secondWord = head . tail . words

dlts' = dlts "#define DLT_"

{-
dlts' ("#define DLT_EN10MB 1  /* Ethernet (10Mb) */\n"   ++   "#define DLT_EN3MB  2  /* Experimental Ethernet (3Mb) */\n"   ++   "#define DLT_AX25   3  /* Amateur Radio AX.25 */")
-}
