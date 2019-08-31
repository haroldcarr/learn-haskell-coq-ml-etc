{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module X where

import           Parser                     (IniAsNestedMap,
                                             parseIniToNestedMap)
import           Tortellini
------------------------------------------------------------------------------
import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           Data.List.NonEmpty
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
------------------------------------------------------------------------------

parseIniToNestedMap' :: Text -> Either (NonEmpty TortError) IniAsNestedMap
parseIniToNestedMap' s = first (pure . ErrorInParsing . T.pack) $ parseIniToNestedMap s

iniAsNestedMapToRep
  :: ReadDocumentSections (Rep record)
  => IniAsNestedMap -> Either (NonEmpty TortError) (Rep record x)
iniAsNestedMapToRep  = runExcept . readDocumentSections

iniRepToRecord :: Generic a => Rep a x -> a
iniRepToRecord  = to

parseIni'
  :: ReadDocumentSections (Rep record)
  => Text
  -> Rep record x
parseIni' s =
  case parseIniToNestedMap s of
    Left e -> error e
    Right m -> case runExcept $ readDocumentSections m of
      Left _  -> error "X"
      Right r -> r

