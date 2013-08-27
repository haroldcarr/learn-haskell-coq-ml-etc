import System.IO
import System.Directory
import Data.List

main = do
    handle   <- openFile "./Learn_You_a_Haskell_programs/todo.txt" ReadMode
    tempdir  <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
        -- ["zero", "one", "two"]
    let todoTasks     = lines contents
        -- ["0 - zero","1 - one","2 - two"]
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TODO items:"
    -- "0 - zero\n1 - one\n2 - two\n"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString -- turn String into Num
        -- todoTasks !! 1 gives "1 - one"
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "./Learn_You_a_Haskell_programs/todo.txt"
    renameFile tempName "./Learn_You_a_Haskell_programs/todo.txt"
