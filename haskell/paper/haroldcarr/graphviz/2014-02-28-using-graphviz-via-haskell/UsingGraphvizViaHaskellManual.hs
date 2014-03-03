{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2014 Mar 02 (Sun) 14:56:32 by Harold Carr.
-}

module Main where

import           Data.Graph.Inductive
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word
import           WriteRunDot

-- http://www.colorcombos.com/color-schemes/2025/ColorCombo2025.html
myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c $ (RGB 127 108 138)
            | n == 2 = c $ (RGB 175 177 112)
            | n == 3 = c $ (RGB 226 206 179)
            | n == 4 = c $ (RGB 172 126 100)
 where c rgb = toColorList [rgb]

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n

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
  where fn (_,l)   = [textLabel l]
        fe (_,_,l) = [textLabel l]

        ga = [ GraphAttrs [ RankDir   FromLeft
                          , BgColor   [toWColor White]
                          ]
             , NodeAttrs  [ shape     BoxShape
                          , FillColor (myColorCL 2)
                          , style     filled
                          ]
             ]

------------------------------------------------------------------------------
-- http://hackage.haskell.org/package/graphviz-2999.16.0.0/docs/Data-GraphViz-Types-Monadic.html

ex2 :: G.DotGraph L.Text
ex2 = digraph (Str "ex2") $ do

    graphAttrs [RankDir FromLeft]
    nodeAttrs  [style filled]

    cluster (Int 0) $ do
        node "Ready"               [ textLabel "ready"
                                   , shape DoubleCircle, myColor 1, FixedSize True, Width 1]
    cluster (Int 1) $ do
        graphAttrs [textLabel "active"]
        node "Open"                [ textLabel "open"
                                   , shape       Circle, myColor 2, FixedSize True, Width 1]
        node "OpenExpectFragment"  [ textLabel "open expect\nfragment"
                                   , shape       Circle, myColor 2, FixedSize True, Width 1]
        node "HalfClosed"          [ textLabel "half-clsd"
                                   , shape       Circle, myColor 2, FixedSize True, Width 1]
        node "endMessage?"         [ textLabel "end req?"
                                   , shape DiamondShape, myColor 4, FixedSize True, Width 1.25, Height 1.25]
        node "fragmentEndMessage?" [ textLabel "end req?"
                                   , shape DiamondShape, myColor 4, FixedSize True, Width 1.25, Height 1.25]
        node "requestFragment"     [ textLabel "FRAGMENT"
                                   , shape     BoxShape, myColor 3]

        "Open"                     --> "endMessage?"
        edge "endMessage?"             "HalfClosed"          [textLabel "true"]
        edge "endMessage?"             "OpenExpectFragment"  [textLabel "false"]
        "OpenExpectFragment"       --> "requestFragment"
        "requestFragment"          --> "fragmentEndMessage?"
        edge "fragmentEndMessage?"     "OpenExpectFragment"  [textLabel "false"]
        edge "fragmentEndMessage?"     "HalfClosed"          [textLabel "true"]

    cluster (Int 2) $ do
        graphAttrs [textLabel "done"]
        node "Closed"              [ textLabel "closed"
                                   , shape DoubleCircle, myColor 1, FixedSize True, Width 1]

    -- outside the box(es)
    node "request"                 [ textLabel "REQUEST"
                                   , shape     BoxShape, myColor 3]
    node "response"                [ textLabel "RESPONSE"
                                   , shape     BoxShape, myColor 3]

    "Ready"      --> "request"
    "request"    --> "Open"

    "HalfClosed" --> "response"
    "response"   --> "Closed"

------------------------------------------------------------------------------
ex3 :: G.DotGraph L.Text
ex3 = digraph (Str "ex3") $ do

    graphAttrs [RankDir FromLeft]

    cluster (Int 0) $ do
        nodeAttrs               [shape DoubleCircle, FixedSize True, Width 1, style filled, myColor 1]
        node "Open"             [textLabel "open"]
        node "Closed"           [textLabel "closed"]

    cluster (Int 1) $ do
        nodeAttrs               [shape       Circle, FixedSize True, Width 1, style filled, myColor 1]
        node "ClosedWaitingAck" [textLabel "clsd waiting\nACK"]

    cluster (Int 2) $ do
        nodeAttrs               [shape     BoxShape,                 Width 1, style filled, myColor 3]
        node "cancel"           [textLabel "CANCEL"]
        node "cancelAck"        [textLabel "CANCEL_ACK"]

    "Open"             --> "cancel"
    "cancel"           --> "ClosedWaitingAck"
    "ClosedWaitingAck" --> "cancelAck"
    "cancelAck"        --> "Closed"

------------------------------------------------------------------------------

doubleCircle :: n -> Text -> Dot n
doubleCircle n l = node n [textLabel l, shape DoubleCircle, FixedSize True, Width 1, style filled, myColor 1]

circle       :: n -> Text -> Dot n
circle       n l = node n [textLabel l, shape       Circle, FixedSize True, Width 1, style filled, myColor 1]

rectangle    :: n -> Text -> Dot n
rectangle    n l = node n [textLabel l, shape     BoxShape,                 Width 1, style filled, myColor 3]

open, closed, waiting, cancel, cancelAck :: Dot L.Text
open      = doubleCircle "Open"             "open"
closed    = doubleCircle "Closed"           "closed"
waiting   = circle       "ClosedWaitingAck" "clsd waiting\nACK"
cancel    = rectangle    "cancel"           "CANCEL"
cancelAck = rectangle    "cancelAck"        "CANCEL_ACK"

ex4 :: G.DotGraph L.Text
ex4 = digraph (Str "ex4") $ do

    graphAttrs [RankDir FromLeft]
    open; closed; waiting; cancel; cancelAck

    "Open"             --> "cancel"
    "cancel"           --> "ClosedWaitingAck"
    "ClosedWaitingAck" --> "cancelAck"
    "cancelAck"        --> "Closed"

------------------------------------------------------------------------------

main :: IO ()
main = do
    doDots [ ("ex1" , graphToDot ex1Params ex1) ]
    doDots [ ("ex2" , ex2)
           , ("ex3" , ex3)
           , ("ex4" , ex4)
           ]


-- End of file.
