{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Tortellini where

import           Parser                     (parseIniDocument)
------------------------------------------------------------------------------
import           Control.Monad.Trans.Except
import qualified Data.Attoparsec.Text       as AP
import           Data.Bifunctor
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.List.NonEmpty
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import           GHC.TypeLits
------------------------------------------------------------------------------
{-
Build a generic representation of type that can be transformed to a concrete record.

- M1: meta information proxies used to construct parts of the rep.
  - D1 = M1 D: datatype        metadata
  - C1 = M1 C: constructor     metadata
  - S1 = M1 S: record selector metadata
- K1: containers for types (e.g., concrete types in record parameters)
- U1: no-arg constructors
-}
------------------------------------------------------------------------------
-- Usage: requires that target record have a Generic instance (see test dir)

parseIni
  :: Generic record
  => ReadDocumentSections (Rep record)
  => Text
  -> Either TortErrors record
parseIni s = do
  doc <- first (pure . ErrorInParsing . T.pack) $ parseIniDocument s
  -- value created by applying 'to' to transform Generic Rep to value of target type
  runExcept $ to <$> readDocumentSections doc

data TortError
  = Error                   Text
  | ErrorAtDocumentProperty Text TortError
  | ErrorAtSectionProperty  Text TortError
  | ErrorInParsing          Text
  deriving (Show)

type TortErrors = NonEmpty TortError

------------------------------------------------------------------------------
-- Reading the document body

class ReadDocumentSections (f :: * -> *) where
  readDocumentSections :: HashMap Text (HashMap Text Text) -> Except TortErrors (f a)

instance ReadDocumentSections a => ReadDocumentSections (D1 meta a) where
  readDocumentSections hm = M1 <$> readDocumentSections @a hm

instance ReadDocumentSections a => ReadDocumentSections (C1 meta a) where
  readDocumentSections hm = M1 <$> readDocumentSections @a hm

instance
  ( ReadDocumentSections a
  , ReadDocumentSections b
  ) => ReadDocumentSections (a :*: b) where
  readDocumentSections hm = (:*:) <$> readDocumentSections @a hm <*> readDocumentSections @b hm

instance
  ( KnownSymbol name
  , Generic t
  , rep ~ Rep t
  , ReadSection rep
  ) => ReadDocumentSections (S1 ('MetaSel ('Just name) z x c) (K1 r t)) where
  readDocumentSections hm =
    case HM.lookup (T.toLower name) hm of
      Nothing -> throwE . pure . ErrorAtDocumentProperty name . Error $ "Missing field in document"
      Just x -> do
        value <- withExcept' $ to <$> readSection @rep x
        pure $ M1 (K1 value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtDocumentProperty name

------------------------------------------------------------------------------
-- Reading the section body

class ReadSection (f :: * -> *) where
  readSection :: HashMap Text Text -> Except TortErrors (f a)

-- apply M1 to the inner type's rep
instance ReadSection a => ReadSection (D1 meta a) where
  readSection hm = M1 <$> readSection @a hm

-- apply M1 to the inner type's rep
instance ReadSection a => ReadSection (C1 meta a) where
  readSection hm = M1 <$> readSection @a hm

-- no arg constructor is the same as itself
instance ReadSection U1 where
  readSection _ = pure U1

-- make a product of left and right hand sides
instance
  ( ReadSection a
  , ReadSection b
  ) => ReadSection (a :*: b) where
  readSection hm = (:*:) <$> readSection @a hm <*> readSection @b hm

-- for a given record field, access the known symbol name and read the field into the type t
instance
  ( KnownSymbol name
  , ReadIniField t
  ) => ReadSection (S1 ('MetaSel ('Just name) z x c) (K1 r t)) where
  readSection hm =
    case HM.lookup (T.toLower name) hm of
      Nothing -> throwE . pure . ErrorAtSectionProperty name . Error $ "Missing field in section"
      Just x -> do
        value <- withExcept' $ readIniField x
        pure $ M1 (K1 value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtSectionProperty name

------------------------------------------------------------------------------
-- Reading Fields

-- class used to parse strings into target types
class ReadIniField a where
  readIniField :: Text -> Except TortErrors a

instance ReadIniField Text where
  readIniField = pure

instance ReadIniField Int where
  readIniField s = case AP.parseOnly AP.decimal s of
    Left  e -> throwE (pure . Error . T.pack $ e)
    Right x -> pure x

instance ReadIniField Bool where
  readIniField s
    | T.toLower s == T.pack "true"  = pure True
    | T.toLower s == T.pack "false" = pure False
    | otherwise = throwE . pure . Error $ "expected true/false, got " <> s

instance (ReadIniField a) => ReadIniField [a] where
  readIniField s = traverse readIniField $ T.splitOn "," s


