-- http://stackoverflow.com/questions/20849893/how-to-plot-a-graph-using-haskell-graphviz

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2014 Feb 28 (Fri) 17:10:45 by Harold Carr.

This prints the dot versions for the examples in Data.Graph.Inductive.Example.
-}

module E1 where

import           Control.Monad                (forM_)
import           Data.Graph.Inductive.Example
import           Data.Graph.Inductive.Graph   (Graph (..))
import           Data.GraphViz                (graphToDot, nonClusteredParams,
                                               toDot)
import           Data.GraphViz.Printing       (renderDot)
import           Data.Text.Lazy               (unpack)

showDot :: (Show (gr l el), Graph gr) => (String, gr l el) -> IO ()
showDot (s,g) = do
    putStrLn "--------------------------------------------------"
    putStrLn s
    putStrLn $ show g
    putStrLn ""
    putStrLn $ unpack $ renderDot $ toDot $ graphToDot nonClusteredParams g

mapShowDot :: (Show (gr l el), Graph gr) => [(String, gr l el)] -> IO ()
mapShowDot xs = forM_ xs showDot

main :: IO ()
main = do
    mapShowDot [("a",a),("b",b),("c",c),("e",e),("loop",loop),("ab",ab),("abb",abb),("dag3",dag3)]
    mapShowDot [("e3",e3)]
    mapShowDot [("cyc3",cyc3),("g3",g3),("g3b",g3b)]
    mapShowDot [("dag4",dag4)]
    mapShowDot [("d1",d1),("d3",d3)]
    mapShowDot [("clr479",clr479),("clr489",clr489)]
    mapShowDot [("clr486",clr486)]
    mapShowDot [("clr508",clr508),("clr528",clr528)]
    mapShowDot [("clr595",clr595),("gr1",gr1)]
    mapShowDot [("kin248",kin248)]
    mapShowDot [("vor",vor)]

-- End of file.
