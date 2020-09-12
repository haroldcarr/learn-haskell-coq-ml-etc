import System.IO
import Data.Char

main = do
    contents <- readFile "./Learn_You_a_Haskell_programs/girlfriend.txt"
    writeFile "/tmp/girlfriendcaps.txt" (map toUpper contents)