{-
Created       : 2014 Jun 17 (Tue) 09:21:12 by Harold Carr.
Last Modified : 2014 Jun 17 (Tue) 09:33:31 by Harold Carr.
-}

module Main where

import           HW07_Buffer
import           HW07_Editor
import           HW07_HC_JoinList
import           HW07_HC_Scrabble
import           HW07_Sized

main :: IO ()
main = runEditor editor ((fromString (unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])) :: JoinList (Score, Size) String)
