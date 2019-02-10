{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ReaderFun where

import           Data.Map    as Map
import           Data.Text   as T
import           Protolude

doit :: (Maybe Int, [(Text,Int)])
doit = runReader f (Map.fromList [("count",3)])

f :: Reader (Map Text Int) (Maybe Int, [(Text,Int)])
f = do
  count' <- asks (Map.lookup "count")
  bindings <- ask
  return (count', Map.toList bindings)

len :: Reader Text Int
len =
  T.length <$> ask

localLen :: Reader Text Int
localLen =
  local ("AS"<>) $
    T.length <$> ask

main :: IO ()
main = do
  print doit
  let s = "12345";
  print $ "len: " <> T.pack (show $ runReader len      s)
  print $ "mod: " <> T.pack (show $ runReader localLen s)
