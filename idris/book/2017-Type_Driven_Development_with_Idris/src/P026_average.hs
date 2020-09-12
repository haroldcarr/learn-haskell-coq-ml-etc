module P026_average where

average :: String -> Double
average str =
    let numWords    = wordCount str
        totalLength = sum (allLengths (words str))
    in fromIntegral totalLength / fromIntegral numWords
  where
    wordCount :: String -> Int
    wordCount = length . words

    allLengths :: [String] -> [Int]
    allLengths = map length

showAverage :: String -> String
showAverage str = "The average word length is: " ++ show (average str) ++ "\n"

p026 :: IO ()
p026 = repl "Enter a string: " showAverage
  where
    repl s f = do
      putStr s
      l <- getLine
      putStrLn (showAverage l)
      repl s f
