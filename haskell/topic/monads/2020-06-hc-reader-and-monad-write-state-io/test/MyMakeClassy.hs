{-# LANGUAGE OverloadedStrings #-}

module MyMakeClassy where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Control.Lens.Internal.FieldTH    as FTH
import qualified Language.Haskell.TH.Lib.Internal as THLI
import qualified Language.Haskell.TH.Syntax       as THS
------------------------------------------------------------------------------

myMakeClassy :: THS.Name -> THLI.DecsQ
myMakeClassy  = makeLensesWith
  (classyRules { FTH._classyLenses = \n ->
                   case THS.nameBase n of
                     x:xs -> Just (THS.mkName ("RW" ++ x:xs), THS.mkName ("l" ++ x:xs))
                     []   -> Nothing })
