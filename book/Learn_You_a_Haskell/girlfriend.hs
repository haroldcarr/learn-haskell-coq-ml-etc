import System.IO

main = do
    contents <- readFile "./Learn_You_a_Haskell_programs/girlfriend.txt"
    putStr contents

{-
equivalent to all:

main = do
    withFile "./Learn_You_a_Haskell_programs/girlfriend.txt" ReadMode
        (\handle -> do
            contents <- hGetContents handle
            putStr contents)

main = do
    handle <- openFile "./Learn_You_a_Haskell_programs/girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
-}