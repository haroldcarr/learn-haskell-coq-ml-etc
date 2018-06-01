{-# LANGUAGE OverloadedStrings #-}

{-
BETTER SOLUTION:
read as bytestring
convert to Text
process via row : M.insertWith (M.unionWith (+)) state (M.singleton city 1)
map to HTML : use ByteString builder
-}

module E1 where

import           Data.List (foldl')
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map.Strict as M

input :: T.Text
input = "Alice,Los Angeles,California\n\
        \Bob,New York, New York\n\
        \Charlie,San Francisco,California\n\
        \David,Portland,Oregon\n\
        \Bad\n\
        \Edward,Los Angeles,California   \n\
        \Bad,Two\n\
        \Yet,Another,Bad,Three\n\
        \Frank,New York,New York"

parse :: T.Text -> [[T.Text]]
parse t = map (map T.strip . T.splitOn ",") (T.lines t)

type State = T.Text
type City  = T.Text
type Count = Int
type StateCityCount = M.Map State (M.Map City Count)

--                      bad input , the map
mkMap :: [[T.Text]] -> ([[T.Text]], StateCityCount)
mkMap = foldl' go ([], M.empty)
 where
  go (iv,m1) [_,city,state] = case M.lookup state m1 of
    Nothing -> (iv, M.insert state (M.fromList [(city,1)]) m1)
    Just m2 -> (iv, M.insert state (case M.lookup city m2 of
                                      Nothing -> M.insert city  1      m2
                                      Just n  -> M.insert city (1 + n) m2) m1)
  go (iv,m1)  x             = (x:iv, m1)

-- TODO : use Builder
mkXml :: StateCityCount -> T.Text
mkXml s = "<ul>" <> M.foldlWithKey go1 "" s <> "</ul>"
 where
  go1 r k v = r <> "<li>" <> k <> "<dl>"  <>  M.foldlWithKey go2 "" v  <> "</dl></li>"
  go2 r k v = r <> "<dt>" <> k <> "</dt>" <> "<dd>" <> T.pack (show v) <> "</dd>"

textToXml :: T.Text -> T.Text
textToXml = mkXml . snd . mkMap . parse
{-
textToXml input

-- mkXml :: StateCityCount -> TL.Text
mkXml x = TLB.toLazyText (M.foldlWithKey go1 "" x)
 where
  go1 r k v =
    r <> "<li>" <> k <> "<dl>" <> M.foldlWithKey go2 "" v <> "</dl></li>"::TLB.Builder
  go2 r k v =
    r <> "<dt>" <> k <> "</dt>" <> "<dd>" <> TLB.fromString (show v) <> "</dd>"::TLB.Builder

-}

