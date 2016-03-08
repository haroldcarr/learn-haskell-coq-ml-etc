{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad   ((>=>))
import           Data.List       (isPrefixOf)
import           Data.Map.Strict as M
import           Data.Maybe      as MB (mapMaybe)
import           Prelude         as P
import           System.IO       (IOMode (ReadMode), hGetContents, withFile)
import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

getTagsIO :: Int
          -> IO (Map Int [Int], Int, Map String Int, Map Int String)
          -> IO (Maybe [String])
getTagsIO x cache = do
    c <- cache
    return $ getTags x c

getTags   :: Int
          -> (Map Int [Int], Int, Map String Int, Map Int String)
          -> Maybe [String]
getTags x (c,_, _,mis) = M.lookup x c >>= \r -> return $ MB.mapMaybe (`M.lookup` mis) r

loadCacheFromFile :: FilePath -> IO (Map Int [Int], Int, Map String Int, Map Int String)
loadCacheFromFile filename =
    withFile filename ReadMode (hGetContents >=> return . stringToCache)

stringToCache :: String -> (Map Int [Int], Int, Map String Int, Map Int String)
stringToCache = dedupTags . collectTagIdAndTags

collectTagIdAndTags :: String -> [(Int, [String])]
collectTagIdAndTags   = P.map mkEntry . lines
  where
    mkEntry x = let (i:xs) = splitOn "," x in (read i, xs)

dedupTags :: (Num k, Ord k, Ord a, Foldable t)
          => [(k, t a)] -> ( M.Map k [k]
                           , k
                           , M.Map a  k
                           , M.Map k  a)
dedupTags  = P.foldr level1 (M.empty, -1, M.empty, M.empty)
  where
    level1 (tag, ss) (acc, i0, msi0, mis0) =
        let (ks, i, msi, mis) = dedupTagIdTags (ss, i0, msi0, mis0)
        in  (M.insert tag ks acc, i, msi, mis)

dedupTagIdTags :: (Num k, Ord k, Ord a, Foldable t)
               => (t a, k, Map a k, Map k a)
               -> ([k], k, Map a k, Map k a)
dedupTagIdTags (ss, i0, msi0, mis0) = P.foldr level2 (mempty, i0, msi0, mis0) ss
  where
    level2 s (acc, i, msi, mis) = case M.lookup s msi of
        Just j  -> (j:acc, i,                msi,              mis)
        Nothing -> (i:acc, i+1, M.insert s i msi, M.insert i s mis)

------------------------------------------------------------------------------
-- Test

tgt = U.t "tgt"
    (P.map (\i -> getTags i (stringToCache "0,foo,bar\n1,abc,foo\n2\n3,xyz,bar")) [0..4])
    [ Just ["foo","bar"]
    , Just ["abc","foo"]
    , Just []
    , Just ["xyz","bar"]
    , Nothing
    ]

tstc = U.t "tstc"
    (let (mii, i, msi, mis) = stringToCache "0,foo,bar\n1,abc,foo\n2\n3,xyz,bar"
     in  (M.toList mii, i, M.toList msi, M.toList mis))
    ( [(0,[1,-1]), (1,[2,1]), (2,[]), (3,[0,-1])]
    , 3
    , [("abc",2),("bar",-1),("foo",1),("xyz",0)]
    , [(-1,"bar"),(0,"xyz"),(1,"foo"),(2,"abc")] )

tct = U.t "tct"
    (collectTagIdAndTags "0,foo,bar\n1,abc,foo\n2\n3,xyz,bar")
    [(0,["foo","bar"]), (1,["abc","foo"]), (2,[]), (3,["xyz","bar"])]

tddt = U.t "tddt"
    (let (i, acc, msi, mis) = dedupTagIdTags ( ["foo", "bar", "baz", "foo", "baz", "baz", "foo", "qux", "new"]
                                             , -1, M.empty, M.empty)
     in  (i, acc, M.toList msi, M.toList mis))
    (                                          [    1,     3,     2,     1,     2,     2,     1,     0,    -1]
    , 4::Int
    , [("bar",3),("baz",2),("foo",1),("new",-1),("qux",0)]
    , [(-1,"new"),(0,"qux"),(1,"foo"),(2,"baz"),(3,"bar")])

test :: IO Counts
test =
    runTestTT $ TestList $ tgt ++ tstc ++ tct ++ tddt

------------------------------------------------------------------------------
-- Utililties

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
    in firstline : case remainder of
        [] -> []
        x -> if x == delim
             then [[]]
             else splitOn delim (drop (length delim) x)

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) = if func list then (x:ys,zs) else ([],list)
  where (ys,zs) = spanList func xs


