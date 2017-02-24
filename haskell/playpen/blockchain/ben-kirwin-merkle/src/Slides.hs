module Slides where

import           Data.ByteString as BS
import           Data.List       as L (genericIndex, stripPrefix)
import           Data.Maybe      (fromMaybe)
import           Numeric         (readHex)
import           Prelude         as P

type Word4 = Int -- TODO : really make it 4 bits
type Key = [Word4] -- each part of key is a HEX digit

mkKey :: String -> [Word4]
mkKey = P.map (\x -> let ((hex,_):_) = readHex [x] in hex)

type Value = ByteString

defaultValue = BS.empty

type AssocList = [(Key, Value)]

alookup :: AssocList -> Key -> Maybe Value
alookup l k = P.lookup k l

aupdate :: AssocList -> Key -> Value -> AssocList
aupdate l k v = (k, v) : l

data Trie
  = TB [Trie] (Maybe Value)
  | TE
  deriving (Eq, Show)

tlookup :: Trie -> Key -> Maybe Value
tlookup                  TE      _  = Nothing
tlookup (TB        _ value)    []  = value
tlookup (TB children     _) (k:ks) =
  let child = genericIndex children k
  in tlookup child ks

tupdate :: Trie -> Key -> Value -> Trie
-- new value
tupdate TE                  []  v =
  TB mkEmptyChildren (Just v)
-- replace existing
tupdate (TB children _)     []  v =
  TB children               (Just v)
-- grow a new branch
tupdate TE               (k:ks) v =
  TB (tinsert mkEmptyChildren k (tupdate TE ks v)) Nothing
-- traverse a existing branch
tupdate (TB children v') (k:ks) v =
  TB (tinsert children k (tupdate (genericIndex children k) ks v)) v' -- TODO : doesn't share

mkEmptyChildren = P.replicate 16 TE

tinsert :: [Trie] -> Word4 -> Trie -> [Trie]
tinsert = tinsert' 0
 where
  tinsert' i (t:ts) k tv | i == k    = tv : ts
                         | otherwise = t : tinsert' (i + 1) ts k tv
  tinsert' _    []  _  _             = error "X"

data PTrie
  = PB [PTrie] Value
  | PS [Word4] (Either PTrie Value) -- shortcut
  | PE
  deriving (Eq, Show)

plookup :: PTrie -> Key -> Value
plookup (PS prefix result) key =
  case (L.stripPrefix prefix key, result) of
    (Just     [], Right value) -> value
    (Just suffix, Left  child) -> plookup child suffix
    _                          -> defaultValue
plookup _ _ = undefined
