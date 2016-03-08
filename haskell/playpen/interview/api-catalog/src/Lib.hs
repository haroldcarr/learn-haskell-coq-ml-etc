module Lib
    ( someFunc
    ) where

import           Data.Map.Strict as M
import           Data.Monoid     ((<>))
import           Prelude         as P
import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

someFunc :: IO ()
someFunc  = putStrLn "someFunc"

collectTagIdAndTags  :: String -> [(Int, [String])]
collectTagIdAndTags   = P.map mkEntry . lines
  where
    mkEntry x = let (i:xs) = words x in (read i, xs)
{-
dedupTags :: [(Int, [String])] -> ( M.Map Int    [Int]
                                  , M.Map String  Int
                                  , M.Map Int     String )
dedupTags  = P.foldr level1 ()
-}

dedupTagIdTags :: (Num k, Ord k, Ord a, Foldable t)
               => (t a, k, Map a k, Map k a)
               -> ([k], k, Map a k, Map k a)
dedupTagIdTags (ss, i0, msi0, mis0) = P.foldr level2 (mempty, i0, msi0, mis0) ss
  where
    level2 s (acc, i, msi, mis) = case M.lookup s msi of
        Just j  -> (j:acc, i,                msi,              mis)
        Nothing -> (i:acc, i+1, M.insert s i msi, M.insert i s mis)

ddt = U.t "ddt"
    (let (i, acc, msi, mis) = dedupTagIdTags ( ["foo", "bar", "baz", "foo", "baz", "baz", "foo", "qux", "new"]
                                             , -1, M.empty, M.empty)
     in  (i, acc, M.toList msi, M.toList mis))
    (                                          [    1,     3,     2,     1,     2,     2,     1,     0,    -1]
    , 4
    , [("bar",3),("baz",2),("foo",1),("new",-1),("qux",0)]
    , [(-1,"new"),(0,"qux"),(1,"foo"),(2,"baz"),(3,"bar")])

test :: IO Counts
test =
    runTestTT $ TestList {- $ -} ddt

