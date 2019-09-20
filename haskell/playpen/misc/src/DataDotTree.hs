{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DataDotTree where

import           Data.Tree
import qualified Prelude
import           Protolude

xx :: Tree (Maybe Text, Maybe (Either Int Text))
xx  =
  Node (Nothing, Just (Left (-1)))
       [ Node (Just "B1", Nothing) []
       , Node (Just "B2", Just (Right "Q2"))
              [ Node (Just "B3", Nothing) []
              , Node (Just "B4", Just (Right "Q4"))
                     [ Node (Just "B5", Just (Right "Q5"))
                            [ Node (Just "B6", Just (Right "Q6")) [] ] ] ] ]

toString :: Tree (Maybe Text, Maybe (Either Int Text)) -> Tree Prelude.String
toString (Node label xs)    = Node (show label) (map toString xs)

{-
import           Data.Tree.Pretty
import           Data.Tree
putStrLn $ drawTree (toString xx)
putStrLn $ drawVerticalTree (toString xx)
-}
