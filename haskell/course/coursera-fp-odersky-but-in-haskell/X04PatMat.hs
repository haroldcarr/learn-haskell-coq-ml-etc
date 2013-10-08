{-
Created       : 2013 Oct 07 (Mon) 14:41:15 by carr.
Last Modified : 2013 Oct 07 (Mon) 18:44:35 by carr.
-}

module X04PatMat where

import Data.List (isInfixOf)

data CodeTree =
      Fork CodeTree CodeTree String Int
    | Leaf Char Int
    deriving (Eq, Show)

weight :: CodeTree -> Int
weight (Fork _ _ _ w) = w
weight (Leaf _ w)     = w

{-# ANN chars "HLint: ignore Use String" #-}
chars :: CodeTree -> String
chars (Fork _ _ cs _) = cs
chars (Leaf c _)      = [c]

makeCodeTree :: CodeTree -> CodeTree -> CodeTree
makeCodeTree left right =
   Fork left  right  (chars left ++ chars right) (weight left + weight right)

times :: String -> [(Char, Int)]
times chars = loop chars []
  where
    loop [] acc = acc
    loop cs acc = loop (tail cs) (doChar (head cs) acc)
    doChar c [] = [(c, 1)]
    doChar c (acchead:acctail) = if   c == fst acchead then  (c, snd acchead + 1) :          acctail
                                 else                               acchead       : doChar c acctail

makeOrderedLeafList :: [(Char, Int)] -> [CodeTree] -- [Leaf]
makeOrderedLeafList freqs = loop freqs []
  where
    loop [] acc = acc
    loop fs acc = loop (tail fs) (doChar (head fs) acc)
    doChar ci [] = [uncurry Leaf ci]  -- [Leaf (fst ci) (snd ci)]
    doChar ci acc@(acchead@(Leaf ac ai):acctail) = if   snd ci < ai then  uncurry Leaf ci :           acc
                                                   else                   acchead         : doChar ci acctail
singleton :: [CodeTree] -> Bool
singleton trees = length trees == 1

combine :: [CodeTree] -> [CodeTree]
combine trees =
    if length trees < 2 then trees
    else                     insert (makeCodeTree (head trees) (head (tail trees)))
                                    (tail (tail trees))
  where
    insert ct [] = [ct]
    insert ct l@(h:t) = if weight ct <= weight h then ct : l
                        else                          h  : insert ct t

until' :: ([CodeTree] -> Bool) -> ([CodeTree] -> [CodeTree]) -> [CodeTree] -> [CodeTree]
until' isSingleton combiner trees =
    if isSingleton trees then trees
    else until' isSingleton  combiner (combiner trees)

createCodeTreeFromUnorderPairs :: [(Char, Int)] -> CodeTree
createCodeTreeFromUnorderPairs pairs = head $ until singleton combine $ makeOrderedLeafList pairs

createCodeTree :: String -> CodeTree
createCodeTree chars = createCodeTreeFromUnorderPairs (times chars)

type Bit = Int

decode :: CodeTree -> [Bit] -> String
decode tree0 bits0 = iter tree0 bits0
  where
    iter t [] = chars t
    iter (Fork l r _ _) (hbs:tbs) =     iter (if hbs == 0 then l else r) tbs -- TODO : hlint bug (remove if brackets)
    iter (Leaf c _)     bs        = c : iter tree0                        bs

encode :: CodeTree -> String -> [Bit]
encode tree0 text0 = iter tree0 text0
  where
    iter :: CodeTree -> String -> [Bit]
    iter _ [] = []
    iter (Fork l r _ _) cs@(h:t) = if [h] `isInfixOf` chars l then 0 : iter l cs -- TODO remove cons: [h]
                                   else                            1 : iter r cs
    iter (Leaf _ _)        (_:t) = iter tree0 t

type CodeTable = [(Char, [Bit])]

codeBits :: CodeTable -> Char -> [Bit]
codeBits [] char = error "NoSuchElementException"
codeBits ((f,s):t) char = if f == char then s else codeBits t char

convert :: CodeTree -> CodeTable
convert (Leaf _ _)     = error "IllegalArgumentException"
convert (Fork l r _ _) = mergeCodeTables (loop l 0) (loop r 1)
  where
    loop (Fork l r _ _) directionBit = addDirectionBit directionBit $ mergeCodeTables (loop l 0) (loop r 1)
    loop (Leaf cl _)    directionBit = [(cl, [directionBit])]
    addDirectionBit _ [] = []
    addDirectionBit directionBit ((f,s):t) = (f, directionBit : s) : addDirectionBit directionBit t

mergeCodeTables :: CodeTable -> CodeTable -> CodeTable
mergeCodeTables t b = foldr (:) b t

quickEncode :: CodeTree -> String -> [Bit]
quickEncode tree = foldl (\ acc h -> acc ++ t2b h) []
  where t2b = codeBits $ convert tree

-- End of file.
