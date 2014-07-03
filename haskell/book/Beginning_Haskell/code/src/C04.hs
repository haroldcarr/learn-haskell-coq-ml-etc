{-
Created       : 2014 Feb                   by Harold Carr.
Last Modified : 2014 Jul 03 (Thu) 14:55:55 by Harold Carr.
-}

{-# LANGUAGE LambdaCase #-}

module C04 where

import           C02
import qualified Data.Foldable   as F (foldr)
import qualified Data.Graph      as G
import           Data.List            (sort)
import qualified Data.Map        as M
import           Data.Monoid
import qualified Data.Set        as S
import           Data.Tree            (flatten, levels)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 4-2 - p. 87

insrt :: Ord k => k -> a -> M.Map k a -> M.Map k a
insrt k v m = M.alter (\_ -> Just v)  k m

delet :: Ord k => k -> M.Map k a -> M.Map k a
delet k   m = M.alter (\_ -> Nothing) k m

adjut :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjut f k m = M.alter (\case { Just v -> Just (f v); Nothing -> Nothing }) k m

m1,m2,m3,m4,m5,m6,m7,m8 :: M.Map [Char] Integer
m1 = M.singleton                       "hello" 3
m2 =   insrt                           "bye"   2 m1
m3 =   insrt                           "hello" 5 m2
m4 = M.insertWith (+)                  "hello" 7 m3
m5 =   delet                           "hello"   m4
m6 =   insrt                           "hello" 5 m2
m7 =   adjut                     (+7)  "hello"   m6
m8 = M.alter (\(Just v) -> Just (v+7)) "hello"   m7

e42 :: T.Test
e42 = T.TestList
    [
      U.teq "e42m1" m1 $ M.fromList [("hello",3)]
    , U.teq "e42m2" m2 $ M.fromList [("bye",2),("hello",3)]
    , U.teq "e42m3" m3 $ M.fromList [("bye",2),("hello",5)]
    , U.teq "e42m4" m4 $ M.fromList [("bye",2),("hello",12)]
    , U.teq "e42m5" m5 $ M.fromList [("bye",2)]
    , U.teq "e42m6" m6 $ M.fromList [("bye",2),("hello",5)]
    , U.teq "e42m7" m7 $ M.fromList [("bye",2),("hello",12)]
    , U.teq "e42m8" m8 $ M.fromList [("bye",2),("hello",19)]
    ]

------------------------------------------------------------------------------
-- sets - p. 87

s1,s2,set1,set2 :: S.Set [Char]
s1   = S.insert "welcome" $ S.singleton "hello"
set1 = S.insert "welcome" $ S.singleton "hello"
s2   = S.fromList ["hello","bye","hello"]
set2 = S.fromList ["hello","bye"]

l3 :: [String]
l3 = S.toList $ S.fromList ["duplicate","boom","duplicate"]

ss0 :: (S.Set [Char], Bool, S.Set Int)
ss0  = (
         set1 `S.intersection` set2
       , "welcome" `S.member` set1
       , S.map length set2
       )

sets :: T.Test
sets = T.TestList
    [
      U.teq   "s1"   s1 $ S.fromList  ["hello","welcome"]
    , U.teq   "s2"   s2 $ S.fromList  ["bye","hello"]
    , U.teq   "l3"   l3               ["boom","duplicate"]
    , U.teq "set1" set1 $ S.fromList  ["hello","welcome"]
    , U.teq "set2" set2 $ S.fromList  ["bye","hello"]
    , U.teq  "ss0"  ss0   (S.fromList ["hello"], True, S.fromList [3,5])
    ]

------------------------------------------------------------------------------
-- Exercise 4-3 - p. 89

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Eq, Ord, Show)

-- single pass of list, inserting into map as it goes
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

-- first partition list, then insert results lists into map
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

e43result :: M.Map ClientKind (S.Set (Client))
e43result = M.fromList [(GovOrgKind,S.fromList [GovOrg "NSA"])
                       ,(CompanyKind,S.fromList [Company "Acme" 3 (Person "First" "Last" Unknown) "Director"])
                       ,(IndividualKind
                        ,S.fromList [Individual (Person "Flavia" "Cervino-Wood" Female) True
                                    ,Individual (Person "Harold" "Carr" Male) True
                                    ,Individual (Person "Mr." "Alejandro" Male) True
                                    ,Individual (Person "Suni" "Coke" Unknown) True])
                       ]

-- TODO: which of above is more efficient?

e43 :: T.Test
e43 = T.TestList
    [
      U.teq "e431" (classifyClients1 clients) e43result
    , U.teq "e432" (classifyClients2 clients) e43result
    ]

------------------------------------------------------------------------------
-- trees - p. 89

pictureTree :: G.Tree Int
pictureTree = G.Node 1 [ G.Node 2 [ G.Node 3 []
                              , G.Node 4 []
                              , G.Node 5 [] ]
                     , G.Node 6 [] ]

trees :: T.Test
trees = T.TestList
    [
      -- preorder
      U.teq "trees-flatten"           (flatten pictureTree)   [1,2,3,4,5,6]
      -- breadth-first
    , U.teq "trees-levles"             (levels pictureTree)   [[1],[2,6],[3,4,5]]
      -- fmap works
    , U.teq "trees-fmap"  (flatten $ fmap (*2) pictureTree)   [2,4,6,8,10,12]
    , U.teq "trees-foldr"       (F.foldr (+) 0 pictureTree)   21
    ]

------------------------------------------------------------------------------
-- graphs - p. 91
--                   (value , key   , edge-to
timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
    [ ("wood"    ,"wood"    ,["walls"])
    , ("plastic" ,"plastic" ,["walls","wheels"])
    , ("aluminum","aluminum",["wheels","door"])
    , ("walls"   ,"walls"   ,["done"])
    , ("wheels"  ,"wheels"  ,["done"])
    , ("door"    ,"door"    ,["done"])
    , ("done"    ,"done"    ,[])
    ]

timeMachinePrecedence :: (G.Graph, G.Vertex -> (String,String,[String]), String -> Maybe G.Vertex)
timeMachinePrecedence = G.graphFromEdges timeMachineGraph

plan :: [String]
plan =
    let (g,v,_) = timeMachinePrecedence
    in map (\x -> let (k,_,_) = v x in k) $ G.topSort g

timeMachineTravel :: G.Graph
timeMachineTravel = G.buildG (103,2013)
                      [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
                      ,(2013,1408),(1408,1993),(1408,917),(1993,917),(907,103),(103,917)]
filterResult :: [G.Tree Int]
filterResult = [G.Node 2013 [G.Node 1302 [G.Node 1614 []]]]

graphs :: T.Test
graphs = T.TestList
    [
      U.teq "graph-plan"      plan
                              ["wood","plastic","walls","aluminum","door","wheels","done"]
    , U.teq "graph-path"      (G.path timeMachineTravel 1302 917)                                             True
    , U.teq "graph-reachable" (G.reachable timeMachineTravel 1302)                                            [1302,2013,1408,917,1993,1614]
--  , U.teq "graph-filter"    (filter (\(G.Node { G.subForest = s }) -> s /= []) $ G.scc timeMachineTravel)   filterResult
    , U.teq "graph-filter"    (filter (\(G.Node _ subForest) -> subForest /= []) $ G.scc timeMachineTravel)   filterResult
    , U.teq "graph-strongly"  (map G.flattenSCC $ G.stronglyConnComp timeMachineGraph)
                              [["done"],["door"],["walls"],["wood"],["wheels"],["plastic"],["aluminum"]]
    ]

------------------------------------------------------------------------------
-- Exercise 4-4 - p. 96

class Priceable p where
    pricee :: p -> Double

data TG a = TG a Double

instance Priceable (TG a) where
    pricee (TG _ d) = d

totalPricee :: Priceable p => [p] -> Double
totalPricee = foldr ((+) . pricee) 0.0

e44 :: T.Test
e44 = T.TestList
    [
      U.teq "e440" (pricee (TG "foo" 1.1)) 1.1
    , U.teq "e441" (totalPricee [TG "foo" 1.1, TG "foo" 2.2]) 3.3000000000000003
    ]

------------------------------------------------------------------------------
-- Exercise 4-5 - p. 99

data ClientI i = GovOrgI     { clientIId :: i, clientIName :: String}
               | CompanyI    { clientIId :: i, clientIName :: String
                             , personI :: PersonI, dutyI :: String}
               | IndividualI { clientIId :: i, personI :: PersonI}
               deriving (Show)

data PersonI = PersonI { firstNameI :: String, lastNameI :: String} deriving (Show)

instance Eq i => Eq (ClientI i) where
    GovOrgI     li ln       == GovOrgI     ri rn       = li == ri && ln == rn
    CompanyI    li ln lp ld == CompanyI    ri rn rp rd = li == ri && ln == rn && lp == rp && ld == rd
    IndividualI li    lp    == IndividualI ri    rp    = li == ri             && lp == rp
    _                       == _                       = False

instance Eq PersonI where
    PersonI lf ll == PersonI rf rl = lf == rf && ll == rl

e45 :: T.Test
e45 = T.TestList
    [
      U.teq "e450" ((GovOrgI (1::Int) "FOO") == (GovOrgI (1::Int) "FOO")) True
    , U.teq "e450" ((GovOrgI (1::Int) "FOO") == (GovOrgI (2::Int) "FOO")) False
    ]

------------------------------------------------------------------------------
-- Exercise 4.6 - p. 100

-- SKIPPED: finer grained: e.g., company responsibility, etc.

-- order: gov < companies < individuals
instance Eq i => Ord (ClientI i) where
    GovOrgI     _ ln       `compare` GovOrgI     _ rn      = ln `compare` rn
    CompanyI    _ ln _  _  `compare` CompanyI    _ rn _  _ = ln `compare` rn
    IndividualI _    lp    `compare` IndividualI _    rp   = lp `compare` rp
    IndividualI {}         `compare` _                     = GT
    _                      `compare` IndividualI {}        = LT
    CompanyI {}            `compare` GovOrgI {}            = GT
    GovOrgI {}             `compare` CompanyI {}           = LT


instance Ord PersonI where
    PersonI lf ll `compare` PersonI rf rl = (lf ++ ll) `compare` (rf ++ rl)

e46 :: T.Test
e46 = T.TestList
    [
      U.teq "e460" (sort clients) [GovOrg "NSA"
                                  ,Company "Acme" 3 (Person "First" "Last" Unknown) "Director"
                                  ,Individual (Person "Flavia" "Cervino-Wood" Female) True
                                  ,Individual (Person "Harold" "Carr" Male) True
                                  ,Individual (Person "Mr." "Alejandro" Male) True
                                  ,Individual (Person "Suni" "Coke" Unknown) True
                                  ]
    ]

------------------------------------------------------------------------------
-- binary tree p. 101

data TravelGuide = TravelGuide { title   :: String
                               , authors :: [String]
                               , tgprice :: Double }
                   deriving (Eq, Ord, Show)


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

------------------------------------------------------------------------------
-- polymorphic binary tree - p. 103

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a) | Leaf2
                     deriving (Eq, Show)

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case compare t v of
                                EQ -> Just v
                                LT -> treeFind2 t l
                                GT -> treeFind2 t r
treeFind2 _ Leaf2         = Nothing

------------------------------------------------------------------------------
-- Exercise 4-7 - p. 103

treeInsert2 :: (Ord a, Eq a) => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v l r) = case compare t v of
                                    EQ -> n
                                    LT -> Node2 v (treeInsert2 t l) r
                                    GT -> Node2 v l                 (treeInsert2 t r)
treeInsert2 t Leaf2           = Node2 t Leaf2 Leaf2

-- concatenation of binary trees via repeated insertion of all the elements in one of the binary trees into the other
treeConcat2 :: (Ord a, Eq a) => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
treeConcat2 (Node2 v l r) nr = treeConcat2 l (treeConcat2 r (treeInsert2 v nr))
treeConcat2 Leaf2         nr = nr

fi2 :: [Client] -> BinaryTree2 Client
fi2 = foldr treeInsert2 Leaf2

ti2 :: BinaryTree2 Client
ti2 = Node2 (Individual (Person "Mr." "Alejandro" Male) True)
            (Node2 (GovOrg "NSA")
                   Leaf2
                   (Node2 (Individual (Person "Flavia" "Cervino-Wood" Female) True)
                          (Node2 (Company "Acme" 3 (Person "First" "Last" Unknown) "Director")
                                 Leaf2
                                 Leaf2)
                          (Node2 (Individual (Person "Harold" "Carr" Male) True)
                                 Leaf2
                                 Leaf2)))
            (Node2 (Individual (Person "Suni" "Coke" Unknown) True)
                   Leaf2
                   Leaf2)

e47 :: T.Test
e47 = T.TestList
    [
      U.teq "e470" (fi2 clients)                                                ti2
    , U.teq "e471" (treeConcat2 (fi2 (take 3 clients)) (fi2 (drop 3 clients)))  ti2
    ]

------------------------------------------------------------------------------
-- Binary Trees with Monoidal Cache - p. 104

-- tree with cache (c) of some value in branch
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
-- generalize: cache element is monoid
treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) =
    case compare v v2 of
        EQ -> Node3 v2 c2 l r
        LT -> let newLeft  = treeInsert4 v c l
                  newCache = c2 <> cached newLeft <> cached r
              in Node3 v2 newCache newLeft r
        GT -> let newRight = treeInsert4 v c r
                  newCache = c2 <> cached l       <> cached newRight
              in Node3 v2 newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty

newtype Min = Min Double deriving (Eq, Show)

instance Monoid Min where
    mempty                  = Min infinity where infinity = 1/0
    mappend (Min x) (Min y) = Min $ min x y

-- TODO: can fi4s and fi4p be factored?
fi4s :: [Client] -> BinaryTree3 Client (Sum Int)
fi4s = foldr (\x acc -> treeInsert4 x (Sum (length (clientName x))) acc) Leaf3

fi4p :: [Client] -> BinaryTree3 Client (Product Int)
fi4p = foldr (\x acc -> treeInsert4 x (Product (length (clientName x))) acc) Leaf3

fi4m :: [Client] -> BinaryTree3 Client (Min)
fi4m = foldr (\x acc -> treeInsert4 x (Min (fromIntegral (length (clientName x)))) acc) Leaf3

-- TODO: understand the numbers
emon :: T.Test
emon = T.TestList
    [
      U.teq "emon-fi4s" (fi4s clients)
                        (Node3 (Individual (Person "Mr." "Alejandro" Male) True) (Sum {getSum = 211})
                               (Node3 (GovOrg "NSA") (Sum {getSum = 83})
                                      Leaf3
                                      (Node3 (Individual (Person "Flavia" "Cervino-Wood" Female) True) (Sum {getSum = 38})
                                             (Node3 (Company "Acme" 3 (Person "First" "Last" Unknown) "Director") (Sum {getSum = 4})
                                                    Leaf3
                                                    Leaf3)
                                             (Node3 (Individual (Person "Harold" "Carr" Male) True) (Sum {getSum = 11})
                                                    Leaf3
                                                    Leaf3)))
                               (Node3 (Individual (Person "Suni" "Coke" Unknown) True) (Sum {getSum = 9})
                                      Leaf3
                                      Leaf3))
    , U.teq "emon-fi4s" (fi4p clients)
                        (Node3 (Individual (Person "Mr." "Alejandro" Male) True) (Product {getProduct = 8237495200588422912})
                               (Node3 (GovOrg "NSA") (Product {getProduct = 14486208})
                                      Leaf3
                                      (Node3 (Individual (Person "Flavia" "Cervino-Wood" Female) True) (Product {getProduct = 3344})
                                             (Node3 (Company "Acme" 3 (Person "First" "Last" Unknown) "Director") (Product {getProduct = 4})
                                                    Leaf3
                                                    Leaf3)
                                             (Node3 (Individual (Person "Harold" "Carr" Male) True) (Product {getProduct = 11})
                                                    Leaf3
                                                    Leaf3)))
                               (Node3 (Individual (Person "Suni" "Coke" Unknown) True) (Product {getProduct = 9})
                                      Leaf3
                                      Leaf3))
    , U.teq "emon-fi4m" (fi4m clients)
                        (Node3 (Individual (Person "Mr." "Alejandro" Male) True) (Min 3.0)
                               (Node3 (GovOrg "NSA") (Min 3.0)
                                      Leaf3
                                      (Node3 (Individual (Person "Flavia" "Cervino-Wood" Female) True) (Min 4.0)
                                             (Node3 (Company "Acme" 3 (Person "First" "Last" Unknown) "Director") (Min 4.0)
                                                    Leaf3
                                                    Leaf3)
                                             (Node3 (Individual (Person "Harold" "Carr" Male) True) (Min 11.0)
                                                    Leaf3
                                                    Leaf3)))
                               (Node3 (Individual (Person "Suni" "Coke" Unknown) True) (Min 9.0)
                                      Leaf3
                                      Leaf3))
    ]

------------------------------------------------------------------------------
-- Exercise 4-8 - p. 107

newtype MMaybe a = MMaybe (Maybe a) deriving (Eq, Show)

instance Functor MMaybe where
    fmap _ (MMaybe Nothing)  = MMaybe Nothing
    fmap f (MMaybe (Just x)) = MMaybe (Just (f x))

instance Functor BinaryTree2 where
    fmap f (Node2 v l r) = Node2 (f v) (fmap f l) (fmap f r)
    fmap _ Leaf2         = Leaf2

e48 :: T.Test
e48 = T.TestList
    [
      U.teq "e480" (fmap (+(2::Int)) (MMaybe Nothing))
                                     (MMaybe Nothing)
    , U.teq "e481" (fmap (+(2::Int)) (MMaybe (Just 2)))
                                     (MMaybe (Just 4))
    , U.teq "e482" (fmap (+(2::Int)) (Node2 2 (Node2 4 Leaf2 Leaf2) (Node2 6 Leaf2 Leaf2)))
                                     (Node2 4 (Node2 6 Leaf2 Leaf2) (Node2 8 Leaf2 Leaf2))
    ]

------------------------------------------------------------------------------
-- Exercise 4-9 - p. 108

e49 :: T.Test
e49 = T.TestList
    [
--      U.teq "e440" (pricee (TG "foo" 1.1)) 1.1
    ]

------------------------------------------------------------------------------

c04 :: IO T.Counts
c04 = do
    _ <- T.runTestTT e42
    _ <- T.runTestTT sets
    _ <- T.runTestTT e43
    _ <- T.runTestTT trees
    _ <- T.runTestTT graphs
    _ <- T.runTestTT e44
    _ <- T.runTestTT e45
    _ <- T.runTestTT e46
    _ <- T.runTestTT e47
    _ <- T.runTestTT emon
    _ <- T.runTestTT e48
    T.runTestTT e49

-- End of file.
