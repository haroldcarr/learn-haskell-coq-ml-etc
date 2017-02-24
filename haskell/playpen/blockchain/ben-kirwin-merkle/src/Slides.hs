module Slides where

import           Data.ByteString as BS
import           Data.List       as L (genericIndex, stripPrefix)
import           Data.Maybe      (fromMaybe)
import           Numeric         (readHex)
import           Prelude         as P

------------------------------------------------------------------------------

type Word4 = Int -- TODO : really make it 4 bits
type Key   = [Word4] -- each part of key is a HEX digit
type Value = ByteString

mkKey :: String -> [Word4]
mkKey = P.map (\x -> let ((hex,_):_) = readHex [x] in hex)

------------------------------------------------------------------------------

type AssocList = [(Key, Value)]

alookup :: AssocList -> Key -> Maybe Value
alookup l k = P.lookup k l

aupdate :: AssocList -> Key -> Value -> AssocList
aupdate l k v = (k, v) : l

------------------------------------------------------------------------------

data Trie
  = TB [Trie] (Maybe Value)
  | TE
  deriving (Eq, Show)

tlookup :: Trie -> Key -> Maybe Value
tlookup                  TE     _  = Nothing
tlookup (TB        _ value)    []  = value
tlookup (TB children     _) (k:ks) =
  let child = genericIndex children k
  in tlookup child ks

tupdate :: Trie -> Key -> Value -> Trie
-- new value
tupdate TE                  []  v =
  TB tmkEmptyChildren (Just v)
-- replace existing
tupdate (TB children _)     []  v =
  TB children               (Just v)
-- grow a new branch
tupdate TE               (k:ks) v =
  TB (tinsert tmkEmptyChildren k (tupdate TE ks v)) Nothing
-- traverse an existing branch
tupdate (TB children v') (k:ks) v =
  TB (tinsert children k (tupdate (genericIndex children k) ks v)) v'

tmkEmptyChildren = emptyChildren TE

------------------------------------------------------------------------------

data PTrie
  = PB [PTrie] (Maybe Value)
  | PS [Word4] (Either PTrie Value) -- shortcut
  | PE
  deriving (Eq, Show)

plookup :: PTrie -> Key -> Maybe Value
plookup (PS prefix result) key =
  case (L.stripPrefix prefix key, result) of
    (Just     [], Right value) -> Just value
    (Just  (_:_), Right     _) -> error "1"
    (Just     [], Left      _) -> error "2"
    (Just suffix, Left  child) -> plookup child suffix
    (Nothing    ,           _) -> Nothing
plookup              PE     _   = Nothing
plookup (PB        _ v)    []   = v
plookup (PB children _) (k:ks)  =
  let child = genericIndex children k
  in plookup child ks

pupdate :: PTrie -> Key -> Value -> PTrie
pupdate p [] v = case p of
  PE      -> PB pmkEmptyChildren (Just v) -- new value
  PS ks _ -> PS ks (Right v)              -- replace existing
  PB cs _ -> PB cs (Just v)               -- replace existing
pupdate p a@(k:ks) v = case p of
  PE       -> PS a (Right v)              -- new value
  PS pks r -> pupdateShortcut pks r a     -- traverse shortcur
  PB cs v' -> PB (tinsert cs k (pupdate (genericIndex cs k) ks v)) v' -- traverse an existing branch
 where
  pupdateShortcut pks r [] = undefined    -- TODO

{-
(PS "F00D" (Right X))            "FO"   Z

(PS "F0"   (Left (PB [... PS "0D" (Right X) ...]
                     Right Z)))
-}

pmkEmptyChildren = emptyChildren PE

------------------------------------------------------------------------------

tinsert :: [a] -> Word4 -> a -> [a]
tinsert = tinsert' 0
 where
  tinsert' i (t:ts) k tv | i == k    = tv : ts
                         | otherwise = t  : tinsert' (i + 1) ts k tv
  tinsert' _    []  _  _             = error "X"

emptyChildren = P.replicate 16
