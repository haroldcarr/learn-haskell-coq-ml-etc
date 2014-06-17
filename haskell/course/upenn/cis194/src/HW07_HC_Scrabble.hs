{-
Created       : 2014 Jun 15 (Sun) 17:51:15 by Harold Carr.
Last Modified : 2014 Jun 16 (Mon) 21:28:18 by Harold Carr.
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW07_HC_Scrabble where

import           Data.Char   (toLower)
import           Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

score :: Char -> Score
score c =
    Score (case toLower c of
                'a' -> 1
                'b' -> 3
                'c' -> 3
                'd' -> 2
                'e' -> 1
                'f' -> 4
                'g' -> 2
                'h' -> 4
                'i' -> 1
                'j' -> 8
                'k' -> 5
                'l' -> 1
                'm' -> 3
                'n' -> 1
                'o' -> 1
                'p' -> 3
                'q' -> 10
                'r' -> 1
                's' -> 1
                't' -> 1
                'u' -> 1
                'w' -> 4
                'x' -> 8
                'y' -> 4
                'z' -> 10
                _   -> 0)

scoreString :: String -> Score
scoreString = Score . foldr (\c acc -> getScore (score c) + acc) 0

-- End of file.
