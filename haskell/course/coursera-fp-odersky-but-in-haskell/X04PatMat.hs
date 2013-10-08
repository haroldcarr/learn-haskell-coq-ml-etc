{-
Created       : 2013 Oct 07 (Mon) 14:41:15 by carr.
Last Modified : 2013 Oct 07 (Mon) 22:38:54 by carr.
-}

module X04PatMat where

import Data.List (find)

data CodeTree =
      --   left     right    chars  weight
      Fork CodeTree CodeTree String Int
      --   char weight
    | Leaf Char Int
    deriving (Eq, Show)

weight :: CodeTree -> Int
weight (Fork _ _ _ w) = w
weight (Leaf _ w)     = w

chars :: CodeTree -> String
chars (Fork _ _ cs _) = cs
chars (Leaf c _)      = [c]

makeCodeTree :: CodeTree -> CodeTree -> CodeTree
makeCodeTree left right =
   Fork left  right  (chars left ++ chars right) (weight left + weight right)

times :: String -> [(Char, Int)]
times = foldl doChar []
  where
    doChar []           c = [(c, 1)]
    doChar (h@(c',w):t) c = if   c == c' then  (c, w + 1) :        t
                            else                        h : doChar t c

makeOrderedLeafList :: [(Char, Int)] -> [CodeTree] -- [Leaf]
makeOrderedLeafList = foldl doChar []
  where
    doChar []                        (c,w) = [Leaf c w]
    doChar acc@(h@(Leaf ac ai):t) cw@(c,w) = if   w < ai then Leaf c w : acc
                                             else             h        : doChar t cw

combine :: [CodeTree] -> [CodeTree]
combine trees
    | length trees < 2 = trees
    | otherwise        = case trees of (h:m:t) -> insert (makeCodeTree h m) t
  where
    insert ct []      = [ct]
    insert ct l@(h:t) = if weight ct <= weight h then ct : l
                        else                          h  : insert ct t

until' :: ([CodeTree] -> Bool) -> ([CodeTree] -> [CodeTree]) -> [CodeTree] -> [CodeTree]
until' isSingleton combiner trees =
    if isSingleton trees then trees
    else until' isSingleton  combiner (combiner trees)

createCodeTreeFromUnorderPairs :: [(Char, Int)] -> CodeTree
createCodeTreeFromUnorderPairs pairs = head $ until (\ trees -> length trees == 1) combine $ makeOrderedLeafList pairs

createCodeTree :: String -> CodeTree
createCodeTree chars = createCodeTreeFromUnorderPairs (times chars)

type Bit = Int

decode :: CodeTree -> [Bit] -> String
decode tree0 bits0 = iter tree0 bits0
  where
    iter t              []        = chars t
    iter (Fork l r _ _) (hbs:tbs) =     iter (if hbs == 0 then l else r) tbs -- TODO : hlint bug (remove if brackets)
    iter (Leaf c _)     bs        = c : iter tree0                        bs

encode :: CodeTree -> String -> [Bit]
encode tree0 text0 = iter tree0 text0
  where
    iter _              []       = []
    iter (Fork l r _ _) cs@(h:t) = if h `elem` chars l then 0 : iter l cs
                                   else                     1 : iter r cs
    iter (Leaf _ _)        (_:t) = iter tree0 t

type CodeTable = [(Char, [Bit])]

codeBits :: CodeTable -> Char -> [Bit]
codeBits ct char =
    case find (\ (c,_) -> c == char) ct of
        Just (_,b) -> b
        Nothing    -> error "NoSuchElementException"

convert :: CodeTree -> CodeTable
convert (Leaf _ _)     = error "IllegalArgumentException"
convert (Fork l r _ _) = mergeCodeTables (loop l 0) (loop r 1)
  where
    loop (Fork l r _ _) directionBit = addDirectionBit directionBit $ mergeCodeTables (loop l 0) (loop r 1)
    loop (Leaf c _)     directionBit = [(c, [directionBit])]
    addDirectionBit db = foldl (\ acc (char,bits) -> (char, db : bits) : acc) []
    mergeCodeTables    = foldr (:)

quickEncode :: CodeTree -> String -> [Bit]
quickEncode tree = foldl (\ acc h -> acc ++ t2b h) []
  where t2b = codeBits $ convert tree

-- End of file.
