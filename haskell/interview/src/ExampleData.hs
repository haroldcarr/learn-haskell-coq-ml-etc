{-
Created       : 2014 Jul 13 (Sun) 06:39:22 by Harold Carr.
Last Modified : 2014 Jul 13 (Sun) 06:43:51 by Harold Carr.
-}

{-# LANGUAGE OverloadedStrings #-}

module ExampleData where

import           Data.RDF
import           Database.HSparql.Connection

example :: [[BindingValue]]
example =
    [
      [ Bound $ UNode("http://openhc.org/haskell.curryATprojectdelta.com")
      , Bound $ UNode("http://openhc.org/memberOf")
      , Bound $ UNode("http://openhc.org/mathGroup")
      ]
    , [ Bound $ UNode("http://openhc.org/haskell.curryATprojectdelta.com")
      , Bound $ UNode("http://openhc.org/emailAddress")
      , Bound $ LNode(PlainL("haskell.curry@projectdelta.com"))
      ]
    , [ Bound $ UNode("http://openhc.org/haskell.curryATprojectdelta.com")
      , Bound $ UNode("http://openhc.org/isA")
      , Bound $ UNode("http://openhc.org/user")
      ]
    , [ Bound $ UNode("http://openhc.org/initialized")
      , Bound $ UNode("http://openhc.org/initialized")
      , Bound $ UNode("http://openhc.org/initialized")
      ]
    , [ Bound $ UNode("http://openhc.org/mathGroup")
      , Bound $ UNode("http://openhc.org/readPermission")
      , Bound $ LNode(PlainL("https://reports.finance.projectdelta.com/"))
      ]
    , [ Bound $ UNode("http://openhc.org/mathGroup")
      , Bound $ UNode("http://openhc.org/writePermission")
      , Bound $ LNode(PlainL("https://scheduling.office.projectdelta.com/"))
      ]
    , [ Bound $ UNode("http://openhc.org/mathGroup")
      , Bound $ UNode("http://openhc.org/readPermission")
      , Bound $ LNode(PlainL("https://scheduling.office.projectdelta.com/"))
      ]
    , [ Bound $ UNode("http://openhc.org/mathGroup")
      , Bound $ UNode("http://openhc.org/writePermission")
      , Bound $ LNode(PlainL("math@projectdelta.com"))
      ]
    , [ Bound $ UNode("http://openhc.org/mathGroup")
      , Bound $ UNode("http://openhc.org/readPermission")
      , Bound $ LNode(PlainL("math@projectdelta.com"))
      ]
    , [ Bound $ UNode("http://openhc.org/mathGroup")
      , Bound $ UNode("http://openhc.org/groupName")
      , Bound $ LNode(PlainL("mathGroup"))
      ]
    , [ Bound $ UNode("http://openhc.org/mathGroup")
      , Bound $ UNode("http://openhc.org/isA")
      , Bound $ UNode("http://openhc.org/group")
      ]
    ]
