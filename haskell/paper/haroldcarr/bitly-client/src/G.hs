{-
Created       : 2014 Mar 15 (Sat) 17:38:34 by Harold Carr.
Last Modified : 2014 Mar 15 (Sat) 18:23:48 by Harold Carr.

http://neilmitchell.blogspot.com/2013/10/haskell-type-graphs-with-uniplate-and.html
-}

import           Data.Generics.Uniplate.Data
import           Data.List
import           Language.Haskell.Exts
import           System.Environment

main :: IO ()
main = do
    [interest,filename] <- getArgs
    doit interest filename

doit :: String -> FilePath -> IO ()
doit interest filename =
    -- writeFile "graph.dot" . graph . interesting "Exp" . reach =<< getModule
    writeFile "graph.dot" . graph . interesting interest . reach =<< (getModule filename)

getModule :: FilePath -> IO Module
getModule filename = do
    -- src <- readFile "../../haskell-src-exts/src/Language/Haskell/Exts/Syntax.hs"
    src <- readFile filename
    return $ fromParseResult $ parseModule $ unlines $ filter (not . bad) $ lines src
        where bad x = head (words x ++ [""]) `elem` words "#if #else #endif deriving #ifdef"

reach :: Module -> [(String, [String])]
reach m = [ (prettyPrint name0, nub [prettyPrint x | TyCon x <- universeBi ctors])
          | DataDecl _ _ _ name0 _ ctors _ <- universeBi m]

interesting :: String -> [(String, [String])] -> [(String, [String])]
interesting target xs0 = [(a,b `intersect` keep) | (a,b) <- xs0, a `elem` keep]
    where keep      = f [target] xs0
          f want xs = if null new then want else f (map fst new ++ want) rest
              where (new,rest) = partition (not . null . intersect want . snd) xs

graph :: [(String, [String])] -> String
graph xs = unlines $ ["digraph g {"] ++ [from ++ " -> " ++ t ++ ";" | (from,to) <- xs, t <- to] ++ ["}"]

-- End of file.
