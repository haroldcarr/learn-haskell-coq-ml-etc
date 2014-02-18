{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module C06 where

import           Control.Lens
import           Data.Default
import           Data.List
import qualified Data.Map     as M

-- k-means - p. 134

{-
- observed fact is point in n-dimensional space
- similarity between facts corresponds to proximity of points
- divide set of points into K partitions so aggregated distance between point minimized
- K must be given in advance (for dynamic approach, see: http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set
-}

-- 2D vector for distance and proximity

class (Default v, Ord v) => Vector v where
    distance :: v   -> v -> Double
    centroid :: [v] -> v

instance Vector (Double, Double) where
    distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
    centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0.0,0.0) lst
                       n     = fromIntegral $ length lst
                   in (u / n, v / n)

-- p. 135

-- to translate a data item into vector

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
    toVector = id

-- p. 136

{-
- K-means describes cluster via one vector for each, called centroid of the cluster.
- Each element in data set is assigned to the cluster whose centroid is nearer to the data point.

- K-means algorithm
  - first phase
    - generates k vectors : used as initial centroids
    - each point assigned to cluster of nearest centroid (i.e., first partition of data points created)
  - compute new centroids
    - updated centroid of each cluster is average of all points in that cluster
  - new centroids become input of new cluster-point assignment and centroid updating phases
  - when clusters have no more change: final centroids are result

- make parameter: method for generating initial vectors
-}

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                      (distance y $ toVector p))
                                                     centroids
                      in M.adjust (p:) chosenCentroid m)
             initialMap points

-- p. 137

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v])  -- initialization function
                                       -> Int                  -- number of centroids
                                       -> [e]                  -- the data
                                       -> Double               -- threshold
                                       -> ([v], Int)           -- final centroids and recursivion counter
kMeans i k points = kMeans' 1 (i k points) points


kMeans' :: (Vector v, Vectorizable e v) => Int-> [v] -> [e] -> Double -> ([v], Int)
kMeans' recursionCounter centroids points threshold =
    let assignments     = clusterAssignmentPhase centroids points
        oldNewCentroids = newCentroidPhase assignments
        newCentroids    = map snd oldNewCentroids
    in if shouldStop oldNewCentroids threshold
       then (newCentroids, recursionCounter)
       else kMeans' (recursionCounter + 1) newCentroids points threshold

-- test - p. 137

initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- kMeans initializeSimple 2 ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)]) 0.001 -- [(1.0,1.5),(4.0,4.5)]

-- lens - p. 138

data ClientI i = GovOrgI     i String
               | CompanyI    i String PersonI String
               | IndividualI i PersonI

data PersonI = PersonI String String


-- simple lenses : type of structure does not change when value is changed
firstNameSL :: Simple Lens PersonI String
firstNameSL = lens (\(PersonI f _) -> f)
                   (\(PersonI _ l) newF -> PersonI newF l)

lastNameSL :: Simple Lens PersonI String
lastNameSL = lens (\(PersonI _ l) -> l)
                  (\(PersonI f _) newL -> PersonI f newL)

-- to change type, e.g., ClientI Int to ClientI Double
identifierL :: Lens (ClientI i) (ClientI j) i j
identifierL = lens (\x -> case x of
                            (GovOrgI i _)      -> i
                            (CompanyI i _ _ _) -> i
                            (IndividualI i _)  -> i)
                   (\client newId -> case client of
                       GovOrgI _ n      -> GovOrgI newId n
                       CompanyI _ n p r -> CompanyI newId n p r
                       IndividualI _ p  -> IndividualI newId p)

-- lens can be used for anything that has well-defined way to get/return value

fullNameSL :: Simple Lens PersonI String
fullNameSL = lens (\(PersonI f l) -> f ++ " " ++ l)
                  (\_ newFullName -> case words newFullName of
                                         f:l:_ -> PersonI f l
                                         _     -> error "Incorrect name")

-- to auto generate basic lens to get/set fields in a structure

data Client i = GovOrg { _identifier :: i
                       , _name       :: String }
              | Company { _identifier :: i
                        , _name       :: String
                        , _person     :: Person
                        , _duty       :: String }
              | Individual { _identifier :: i
                           , _person     :: Person }
              deriving Show
data Person = Person { _firstName :: String
                     , _lastName  :: String }
              deriving Show

makeLenses ''Client
makeLenses ''Person

-- exactly same as above, but use Person
fullName :: Simple Lens Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                         f:l:_ -> Person f l
                                         _     -> error "Incorrect name")



-- see C06/Main.hs
-- because can't load above into ghci:
{-
GHCi runtime linker: fatal error: I found a duplicate definition for symbol
   _lenszm4zi0zi3_ControlziLensziInternalziReflection_zdwzdctypeOf126_info
whilst processing object file
   /Users/carr/Library/Haskell/ghc-7.6.3/lib/lens-4.0.3/lib/libHSlens-4.0.3.a
-}

-- End of file.
