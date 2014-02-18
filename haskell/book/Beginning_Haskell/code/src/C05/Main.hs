module Main where

import           Data.List (foldl')

-- profiling with GHC - p. 122

{-
cabal clean
cabal configure --enable-executable-profiling
cabal build

dist/build/profiling-example/profiling-example +RTS -p -RTS
profiling-example.prof

dist/build/profiling-example/profiling-example +RTS -h -RTS
dist/build/profiling-example/profiling-example +RTS -hy -RTS
hp2ps profiling-example.hp
open /Applications/Preview.app profiling-example.ps


-}

main :: IO ()
main = putStrLn $ show (length (show result))

result :: Integer
result = foldr (*) 1 [1 .. 100000]

{-
main :: IO ()
main = do
    print $ foldl' (+) 0 ([1 .. 1000000000] :: [Integer]) -- 500000000500000000
-}

-- End of file.

