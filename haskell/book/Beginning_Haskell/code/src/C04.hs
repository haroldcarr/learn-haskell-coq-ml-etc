{-
Created       : 2014 Feb                   by Harold Carr.
Last Modified : 2014 Jun 30 (Mon) 02:44:44 by Harold Carr.
-}

{-# LANGUAGE LambdaCase #-}

module C04 where

import           C02
import           Data.Graph
import qualified Data.Map   as M
import           Data.Monoid  (Monoid, mempty, (<>))
import qualified Data.Set   as S

m1,m2,m3,m4,m6,m7,m8 :: M.Map [Char] Integer
m1 = M.singleton "hello" 3
m2 = M.insert    "bye"   2 m1
m3 = M.insert    "hello" 5 m2
m4 = M.insertWith (+) "hello" 7 m3
m5 :: (M.Map [Char] Integer, M.Map [Char] Integer, M.Map [Char] Integer, M.Map [Char] Integer)
m5 = (m1,m2,m3,m4)
m6 = M.delete "hello"                          $ M.fromList [("hello", 3), ("bye", 4)]
m7 = M.adjust (+7) "hello"                     $ M.fromList [("hello", 3), ("bye", 4)]
m8 = M.alter (\(Just v) -> Just (v+7)) "hello" $ M.fromList [("hello", 3), ("bye", 4)]

-- exercise 4-2 - p. 87

insrt :: Ord k => k -> a -> M.Map k a -> M.Map k a
insrt k v m = M.alter (\_ -> Just v)  k m

delet :: Ord k => k -> M.Map k a -> M.Map k a
delet k   m = M.alter (\_ -> Nothing) k m

adjut :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjut f k m = M.alter (\case { Just v -> Just (f v); Nothing -> Nothing }) k m

-- sets - p. 87

s1,s2,set1,set2 :: S.Set [Char]
s1 = S.insert "welcome" $ S.singleton "hello"
s2 = S.fromList ["hello","bye","hello"]
l3 :: [String]
l3 = S.toList $ S.fromList ["duplicate","boom","duplicate"]
set1 = S.insert "welcome" $ S.singleton "hello"
set2 = S.fromList ["hello","bye"]
ss0 :: (S.Set [Char], Bool, S.Set Int)
ss0  = ( set1 `S.intersection` set2
       , "welcome" `S.member` set1
       , S.map length set2 )

-- exercise 4-3 - p. 89

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Eq, Ord, Show)


classifyClients1 :: [Client] -> M.Map ClientKind (S.Set (Client))
classifyClients1 = foldr f (M.insert GovOrgKind S.empty
                                 $ M.insert CompanyKind S.empty
                                       $ M.insert IndividualKind S.empty M.empty)
  where
    f   x a = case x of
                GovOrg     {} -> g GovOrgKind     x a
                Company    {} -> g CompanyKind    x a
                Individual {} -> g IndividualKind x a
    g t x a = M.adjust (S.insert x) t a

classifyClients2 :: [Client] -> M.Map ClientKind (S.Set (Client))
classifyClients2 cs =
  let (g,c,i) = foldr (\x (gg,cc,ii) -> case x of
                                         GovOrg     {} -> (x:gg,  cc,  ii)
                                         Company    {} -> (  gg,x:cc,  ii)
                                         Individual {} -> (  gg,  cc,x:ii))
                      ([],[],[]) cs
  in M.insert GovOrgKind (S.fromList g)
         $ M.insert CompanyKind (S.fromList c)
               $ M.insert IndividualKind (S.fromList i) M.empty
-- classifyClients1 clients ==  classifyClients2 clients

-- trees - p. 80

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 [] ]

-- preorder
-- flatten pictureTree

-- breadth-first
-- levels pictureTree

-- flatten $ fmap (*2) pictureTree

-- graphs - p. 91

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
  ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
  ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]

timeMachinePrecedence :: (Graph, Vertex -> (String,String,[String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

tmp :: [String]
tmp =
    let (g,v,_) = timeMachinePrecedence
    in map (\x -> let (k,_,_) = v x in k) $ topSort g

timeMachineTravel :: Graph
timeMachineTravel = buildG (103,2013)
                      [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
                      ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]

-- path timeMachineTravel 1302 917
-- reachable timeMachineTravel 1302
-- filter (\(Node { subForest = s }) -> s /= []) $ scc timeMachineTravel
-- map flattenSCC $ stronglyConnComp timeMachineGraph

-- binary tree p. 101

data TravelGuide = TravelGuide { title   :: String
                               , authors :: [String]
                               , price   :: Double }
                   deriving (Show, Eq, Ord)


data BinaryTree = BNode TravelGuide BinaryTree BinaryTree
                | Leaf
                deriving Show

treeFind :: TravelGuide -> BinaryTree -> Maybe TravelGuide
treeFind t (BNode v l r) = case compare t v of
                             EQ -> Just v
                             LT -> treeFind t l
                             GT -> treeFind t r
treeFind _ Leaf          = Nothing

treeInsert :: TravelGuide -> BinaryTree -> BinaryTree
treeInsert t n@(BNode v l r) = case compare t v of
                                 EQ -> n
                                 LT -> BNode v (treeInsert t l) r
                                 GT -> BNode v l                (treeInsert t r)
treeInsert t Leaf            = BNode t Leaf Leaf

-- polymorphic binary tree - p. 103

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a) | Leaf2
                     deriving Show
treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
                              EQ -> Just v
                              LT -> treeFind2 t l
                              GT -> treeFind2 t r
treeFind2 _ Leaf2         = Nothing

-- exercise 4-7 - p. 103

treeInsert2 :: (Ord a, Eq a) => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
                                 EQ -> n
                                 LT -> Node2 v (treeInsert2 t l) r
                                 GT -> Node2 v l                (treeInsert2 t r)
treeInsert2 t Leaf2            = Node2 t Leaf2 Leaf2

-- Binary Trees with Monoidal Cache - p. 104


-- tree with cache (c) of minimum value in brach
data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                     deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r) = case compare v v2 of
                                      EQ -> Node3 v2 c2 l r
                                      LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
                                      GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3             = Node3 v c Leaf3 Leaf3

-- p. 105

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) =
    case compare v v2 of
      EQ -> Node3 v2 c2 l r
      LT -> let newLeft = treeInsert4 v c l
                newCache = c2 <> cached newLeft <> cached r
            in Node3 v2 newCache newLeft r
      GT -> let newRight = treeInsert4 v c r
                newCache = c2 <> cached l <> cached newRight
            in Node3 v2 newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty

-- End of file.
