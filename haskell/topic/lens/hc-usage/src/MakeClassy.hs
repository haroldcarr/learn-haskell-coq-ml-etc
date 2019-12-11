{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MakeClassy where

------------------------------------------------------------------------------
import           Control.Lens
import qualified Control.Lens.Internal.FieldTH    as FTH
import           Data.Char                        (isUpper, toLower, toUpper)
import qualified Language.Haskell.TH.Lib.Internal as THLI
import qualified Language.Haskell.TH.Syntax       as THS
import qualified Prelude
import           Protolude                        hiding (get, gets, round, to)
------------------------------------------------------------------------------

obmMakeClassy :: THS.Name -> THLI.DecsQ
obmMakeClassy  = makeLensesWith
  (classyRules { FTH._classyLenses = \n ->
                   case THS.nameBase n of
                     x:xs -> Just (THS.mkName ("RW" ++ x:xs), THS.mkName ("l" ++ x:xs))
                     []   -> Nothing })

obmMakeFields :: THS.Name -> THLI.DecsQ
obmMakeFields  = makeLensesWith
  (defaultFieldRules { FTH._fieldToDef = obmNamer })

obmNamer :: FieldNamer
obmNamer _ _ field =
  let field' = getFieldName (THS.nameBase field)
      cls    = "RW" ++ overHead toUpper field'
   in [MethodName (THS.mkName cls) (THS.mkName field')]
 where
  overHead f = \case (x:xs) -> f x : xs; [] -> []

  getFieldName field0 =
    if any isUpper field0
      then (reverse . snd . foldl' go (False, "")) field0
      else Prelude.tail field0 -- CAREFUL!
   where
    go (b, acc) c
      | b         = (b   ,         c : acc)
      | isUpper c = (True, toLower c : acc)
      | otherwise = (b   ,             acc)
