{-
Created       : 2013 Oct 07 (Mon) 14:41:15 by carr.
Last Modified : 2014 Mar 06 (Thu) 13:27:59 by Harold Carr.
-}

module FP06ForComp where

import           Data.Char       (toLower)
import           Data.Function   (on)
import           Data.List       (groupBy, sortBy)
import qualified Data.Map.Strict as M
import           Data.Ord        (comparing)
import           SplitLines

type Word        = String
type Sentence    = [Word]
type Occurrences = [(Char, Int)]

readDictionary :: IO [Word]
readDictionary = do
    x <- readFile "FP06ForCompTestWords.txt"
    return $ splitLines x

-- http://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
xGroupBy :: Ord a1 => (a -> a1) -> [a] -> [[a]]
xGroupBy f = groupBy ((==) `on` f) . sortBy (comparing f)

wordOccurrences :: Word -> Occurrences
wordOccurrences w =
    let gb = xGroupBy toLower w
    in map (\l@(c:_) -> (toLower c, length l)) gb

sentenceOccurrences :: [Word] -> Occurrences
sentenceOccurrences s = wordOccurrences $ foldl (++) "" s

dictionaryByOccurrences :: IO (M.Map Occurrences [Word])
dictionaryByOccurrences = do
    dictionary <- readDictionary
    let gb = xGroupBy wordOccurrences dictionary
    return $ foldl (\acc l@(h:_) -> M.insert (wordOccurrences h) l acc) M.empty gb

wordAnagrams :: Word -> M.Map Occurrences [Word] -> [Word]
wordAnagrams word = M.findWithDefault [] (wordOccurrences word)

combinations :: (Enum t1, Num t1) => [(t, t1)] -> [[(t, t1)]]
combinations = foldr step [[]]
  where
    step (char, num) acc = acc ++ [ (char,n) : pair | pair <- acc,  n <- [1..num] ]

subtract' :: Occurrences -> Occurrences -> Occurrences
subtract' x y = M.toList $ foldl step (M.fromList x) y
  where
    step accMap (char, num) =
        let newNum = (accMap M.! char) - num
        in if newNum /= 0 then M.alter (\_ -> Just newNum) char accMap
           else                M.delete char accMap

sentenceAnagrams :: [Word] -> M.Map [(Char, Int)] [Word] -> [[Word]]
sentenceAnagrams sentence0 dictByOccurrences =
    let iter [] = [[]]
        iter occurrences = [ word : sentence | combination <- combinations occurrences,
                                               word        <- M.findWithDefault [] combination dictByOccurrences,
                                               sentence    <- iter $ subtract' occurrences (wordOccurrences word),
                                               not $ null combination ]
    in iter $ sentenceOccurrences sentence0

-- End of file.


