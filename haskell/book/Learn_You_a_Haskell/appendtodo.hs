import System.IO

main = do
    todoItem <- getLine
    appendFile "./Learn_You_a_Haskell_programs/todo.txt" (todoItem ++ "\n")