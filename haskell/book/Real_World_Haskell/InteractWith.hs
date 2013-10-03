import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
              args <- getArgs
              case args of
                  [input,output] -> interactWith function input output
                  _              -> putStrLn "error: exactly two arguments needed"
          -- When you want to run a PURE function of type
          -- String -> String
          -- replace "id" with the name of the function
          myFunction = id