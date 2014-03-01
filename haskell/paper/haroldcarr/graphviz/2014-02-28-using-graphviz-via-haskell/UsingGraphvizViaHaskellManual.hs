{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2014 Feb 28 (Fri) 21:58:05 by Harold Carr.
-}

module Main where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Colors.Brewer
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised        as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                         as L
import           Data.Word
import           Shelly                                 (shelly)
import           WriteRunDot

pastel28CL :: Word8 -> ColorList
pastel28CL n = toColorList [toColor (BC (BScheme Pastel2 8) n)]

pastel28 :: Word8 -> Attribute
pastel28 n = Color $ pastel28CL n

------------------------------------------------------------------------------
-- http://speely.wordpress.com/2010/09/17/haskell-graphs-and-underpants/
-- https://github.com/mcandre/mcandre/blob/master/haskell/gnomes.hs

ex1 :: Gr Text Text
ex1 = mkGraph [ (1,"one")
              , (3,"three")
              ]
              [ (1,3,"edge label") ]

ex1Params :: GraphvizParams n L.Text L.Text () L.Text
ex1Params = nonClusteredParams { globalAttributes = ga
                               , fmtNode          = fn
                               , fmtEdge          = fe
                               }
  where fn (_,l)   = [(Label . StrLabel) l]
        fe (_,_,l) = [(Label . StrLabel) l]

        ga = [ GraphAttrs [ RankDir   FromLeft
                          , BgColor   [toWColor White]
                          ]
             , NodeAttrs  [ Shape     BoxShape
                          , FillColor (pastel28CL 2)
                          , Style     [SItem Filled []]
                          ]
             ]

------------------------------------------------------------------------------
-- http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Types-Monadic.html

ex2 :: G.DotGraph L.Text
ex2 = digraph (Str "ex2") $ do

    graphAttrs [RankDir FromLeft]
    nodeAttrs  [style filled]

    cluster (Int 0) $ do
        node "Ready"               [textLabel "ready"
                                   , Shape DoubleCircle, pastel28 1, FixedSize True, Width 1]
    cluster (Int 1) $ do
        graphAttrs [textLabel "active"]
        node "Open"                [textLabel "open"
                                   , Shape       Circle, pastel28 2, FixedSize True, Width 1]
        node "OpenExpectFragment"  [textLabel "open expect\nfragment"
                                   , Shape       Circle, pastel28 2, FixedSize True, Width 1]
        node "HalfClosed"          [textLabel "half-clsd"
                                   , Shape       Circle, pastel28 2, FixedSize True, Width 1]
        node "endMessage?"         [textLabel "end req?"
                                   , Shape DiamondShape, pastel28 6, FixedSize True, Width 1.25, Height 1.25]
        node "fragmentEndMessage?" [textLabel "end req?"
                                   , Shape DiamondShape, pastel28 6, FixedSize True, Width 1.25, Height 1.25]
        node "requestFragment"     [textLabel "FRAGMENT"
                                   , Shape     BoxShape, pastel28 5]

        "Open"                     --> "endMessage?"
        edge "endMessage?"             "HalfClosed"          [textLabel "true"]
        edge "endMessage?"             "OpenExpectFragment"  [textLabel "false"]
        "OpenExpectFragment"       --> "requestFragment"
        "requestFragment"          --> "fragmentEndMessage?"
        edge "fragmentEndMessage?"     "OpenExpectFragment"  [textLabel "false"]
        edge "fragmentEndMessage?"     "HalfClosed"          [textLabel "true"]

    cluster (Int 2) $ do
        graphAttrs [textLabel "done"]
        node "Closed"              [textLabel "closed"
                                   , Shape DoubleCircle, pastel28 1, FixedSize True, Width 1]

    -- outside the box(es)
    node "request"                 [textLabel "REQUEST"
                                   , Shape     BoxShape, pastel28 5]
    node "response"                [textLabel "RESPONSE"
                                   , Shape     BoxShape, pastel28 5]

    "Ready"      --> "request"
    "request"    --> "Open"

    "HalfClosed" --> "response"
    "response"   --> "Closed"

------------------------------------------------------------------------------
ex3 :: G.DotGraph L.Text
ex3 = digraph (Str "exe") $ do

    graphAttrs [RankDir FromLeft]

    cluster (Int 0) $ do
        nodeAttrs               [Shape DoubleCircle, FixedSize True, Width 1, style filled, pastel28 1]
        node "Open"             [textLabel "open"]
        node "Closed"           [textLabel "closed"]

    cluster (Int 1) $ do
        nodeAttrs               [Shape       Circle, FixedSize True, Width 1, style filled, pastel28 1]
        node "ClosedWaitingAck" [textLabel "clsd waiting\nACK"]

    cluster (Int 2) $ do
        nodeAttrs               [shape     BoxShape,                 Width 1, style filled, pastel28 5]
        node "cancel"           [textLabel "CANCEL"]
        node "cancelAck"        [textLabel "CANCEL_ACK"]

    "Open"             --> "cancel"
    "cancel"           --> "ClosedWaitingAck"
    "ClosedWaitingAck" --> "cancelAck"
    "cancelAck"        --> "Closed"

------------------------------------------------------------------------------
main :: IO ()
main = shelly $ do
    doDots [ ("ex1" , graphToDot ex1Params ex1) ]
    doDots [ ("ex2" , ex2)
           , ("ex3" , ex3)
           ]

-- End of file.
