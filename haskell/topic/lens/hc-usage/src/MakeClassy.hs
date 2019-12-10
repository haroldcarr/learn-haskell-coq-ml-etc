{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module MakeClassy where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Control.Lens.Internal.FieldTH    as FTH
import qualified Language.Haskell.TH.Lib.Internal as THLI
import qualified Language.Haskell.TH.Syntax       as THS
import           Protolude                        hiding (get, gets, round, to)
------------------------------------------------------------------------------

obmClassyRules :: FTH.LensRules
obmClassyRules  =
  classyRules { FTH._classyLenses = \n ->
                  case THS.nameBase n of
                    x:xs -> Just (THS.mkName ("RW" ++ x:xs), THS.mkName ("l" ++ x:xs))
                    []   -> Nothing }

obmMakeClassy :: THS.Name -> THLI.DecsQ
obmMakeClassy = makeLensesWith obmClassyRules

obmFieldRules :: FTH.LensRules
obmFieldRules  =
  defaultFieldRules
