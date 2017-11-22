{-
stack exec main
stack exec main -- --help
stack exec main -- --color=True
stack exec main -- --datadir=/tmp/junk
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           System.Console.CmdArgs.Implicit

{-# ANN module "HLint: ignore Use camelCase" #-}

data HLint = HLint
    { report      :: [FilePath]
    , hint        :: [FilePath]
    , color       :: Bool
    , ignore_     :: [String]
    , show_       :: Bool
    , extension   :: [String]
    , language    :: [String]
    , utf8        :: Bool
    , encoding    :: String
    , find        :: [FilePath]
    , test_       :: Bool
    , datadir     :: [FilePath]
    , cpp_define  :: [String]
    , cpp_include :: [FilePath]
    , files       :: [FilePath]
    }
    deriving (Data,Typeable,Show,Eq)

hlint = HLint
  { report      = def &= opt "report.html" &= typFile &= help "Generate a report in HTML"
  , hint        = def &= typFile        &= help "Hint/ignore file to use"
  , color       = def &= name "c"       &= name "colour" &= help "Color the output (requires ANSI terminal)"
  , ignore_     = def &= typ "MESSAGE"  &= help "Ignore a particular hint"
  , show_       = def &= help "Show all ignored ideas"
  , extension   = def &= typ "EXT"      &= help "File extensions to search (defaults to hs and lhs)"
  , language    = def &= name "X"       &= typ "LANG" &= help "Language extension (Arrows, NoCPP)"
  , utf8        = def &= help "Use UTF-8 text encoding"
  , encoding    = def &= typ "ENC"      &= help "Choose the text encoding"
  , find        = def &= typFile        &= help "Find hints in a Haskell file"
  , test_       = def &= help "Run in test mode"
  , datadir     = def &= typDir         &= help "Override the data directory"
  , cpp_define  = def &= typ "NAME[=VALUE]" &= help "CPP #define"
  , cpp_include = def &= typDir         &= help "CPP include path"
  , files       = def &= args           &= typ "FILES/DIRS"
  }
  &= verbosity
  &= help    "Suggest improvements to Haskell source code"
  &= summary "HLint v0.0.0, (C) Neil Mitchell"
  &= details [ "Hlint gives hints on how to improve Haskell code"
             , ""
             , "To check all Haskell files in 'src' and generate a report type:"
             , "  hlint src --report"
             ]

mode = cmdArgsMode hlint

main = print =<< cmdArgs hlint
