{-
Created       : 2013 Oct 07 (Mon) 14:41:15 by carr.
Last Modified : 2013 Oct 30 (Wed) 13:31:29 by carr.
-}

module SplitLines where

-- from Real World Haskell
splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in pre : case suf of
                 ('\r':'\n':rest) -> splitLines rest
                 ('\r':rest)      -> splitLines rest
                 ('\n':rest)      -> splitLines rest
                 _                -> []
  where
    isLineTerminator c = c == '\r' || c == '\n'

-- End of file.
