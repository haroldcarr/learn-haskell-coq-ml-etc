{-
Created       : 2014 Feb                   by Harold Carr.
Last Modified : 2014 Jun 30 (Mon) 02:45:29 by Harold Carr.
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Lens
import           "mtl" Control.Monad.Reader
import           "mtl" Control.Monad.State
import           "mtl" Control.Monad.Writer
import           Data.Char            (toUpper)
import           Data.Default
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe

------------------------------------------------------------------------------
-- k-means - p. 134

{-
- fact is point in n-dimensional space
- similarity between facts corresponds to proximity of points
- divide set of points into K partitions so aggregated distance between point minimized
- K must be given in advance (for dynamic approach, see: http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set)
-}

-- 2D vector for distance and proximity

class (Default v, Ord v) => Vector v where
    distance ::  v  -> v -> Double    -- distance between to facts (i.e., points/Vectors)
    centroid :: [v] -> v              -- a virtual point representing the average location of facts

instance Vector (Double, Double) where
    distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
    centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0.0,0.0) lst
                       n     = fromIntegral $ length lst
                   in (u / n, v / n)  -- average point

-- p. 135

-- to translate a data item into vector

class Vector v => Vectorizable e v where
    toVector :: e -> v

instance Vectorizable (Double,Double) (Double,Double) where
    toVector = id

-- p. 136

{-
- K-means describes a cluster via centroid for each cluster.
- Each element in data set is assigned to the cluster whose centroid is nearer to the data point.

- K-means algorithm
  - first phase :  partition data points into K clusters
    - generates k vectors/points : used as initial centroids
    - each point assigned to cluster of nearest initial centroid
  - compute new centroids
    - updated centroid of each cluster is average of all points in that cluster
  - new centroids become input of new cluster-point assignment and centroid updating phases
  - when clusters have no more change: final centroids are result
-}

clusterAssignmentPhase0 :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase0 centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                      (distance y $ toVector p))
                                                     centroids
                      in M.adjust (p:) chosenCentroid m)
             initialMap points

-- p. 137

-- list of (old,new)
newCentroidPhase0 :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase0 = M.toList . fmap (centroid . map toVector)

shouldStop0 :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop0 centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

kMeans0 :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v])  -- function for generating initial vectors
                                           -> Int               -- number of centroids
                                           -> [e]               -- the data
                                           -> Double            -- threshold
                                           -> ([v], Int)        -- final centroids and recursivion counter
kMeans0 f k points = kMeans0' 1 (f k points) points


kMeans0' :: (Vector v, Vectorizable e v) => Int-> [v] -> [e] -> Double -> ([v], Int)
kMeans0' recursionCounter centroids points threshold =
    let assignments        = clusterAssignmentPhase0 centroids points
        oldAndNewCentroids = newCentroidPhase0 assignments
        newCentroids       = map snd oldAndNewCentroids
    in if shouldStop0 oldAndNewCentroids threshold
       then (newCentroids, recursionCounter)
       else kMeans0' (recursionCounter + 1) newCentroids points threshold

-- test - p. 137

-- this generator does not use the data points (v), but other versions might
-- generates [(n,n), (n-1,n-1), ... (1,1)]
generateCentroids :: Int -> [e] -> [(Double,Double)]
generateCentroids 0 _ = []
generateCentroids n v = (fromIntegral n, fromIntegral n) : generateCentroids (n-1) v

-- ([(1.0,1.5),(4.0,4.5)],3)
tryKMeans0 :: IO ()
tryKMeans0 = print $ show $ kMeans0 generateCentroids 2 ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)]) 0.001

------------------------------------------------------------------------------
-- lens - p. 138

data ClientI i = GovOrgI     i String
               | CompanyI    i String PersonI String
               | IndividualI i PersonI

data PersonI = PersonI String String


-- simple lenses : type of structure does not change when value is changed
firstNameSL :: Simple Lens PersonI String
firstNameSL = lens (\(PersonI f _) -> f)
                   (\(PersonI _ l) newF -> PersonI newF l)

-- p. 139

lastNameSL :: Simple Lens PersonI String
lastNameSL = lens (\(PersonI _ l) -> l)
                  (\(PersonI f _) newL -> PersonI f newL)

-- to change type, e.g., ClientI Int to ClientI Double
identifierL :: Lens (ClientI i) (ClientI j) i j
identifierL = lens (\x -> case x of
                            (GovOrgI     i _)     -> i
                            (CompanyI    i _ _ _) -> i
                            (IndividualI i _)     -> i)
                   (\client newId -> case client of
                       GovOrgI     _ n     -> GovOrgI     newId n
                       CompanyI    _ n p r -> CompanyI    newId n p r
                       IndividualI _ p     -> IndividualI newId p)

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

-- exactly same as above, but use Person instead of PersonI
fullName :: Simple Lens Person String
fullName = lens (\(Person f l)  -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                       f:l:_ -> Person f l
                                       _     -> error "Incorrect name")

-- because can't load above into ghci:
{-
GHCi runtime linker: fatal error: I found a duplicate definition for symbol
   _lenszm4zi0zi3_ControlziLensziInternalziReflection_zdwzdctypeOf126_info
whilst processing object file
   /Users/carr/Library/Haskell/ghc-7.6.3/lib/lens-4.0.3/lib/libHSlens-4.0.3.a
-}

lensEx :: IO ()
lensEx = do
    -- p. 140
    -- querying a value via view function or (^.) operator
    let p = Person "John" "Smith"
    putStrLn $ show $ (view firstName p, p^.lastName)
    let client = Individual (3 :: Int) (Person "John" "Smith")
    -- compose lenses with .
    putStrLn $ show $ view (person . lastName) client
    putStrLn $ show $ client^.person.fullName
    -- update via set function or (.~) operator
    -- new copy of data structure with field updated is returned (not an update in-place)
    putStrLn $ show $ set identifier (4 :: Int) client
    putStrLn $ show $ person.lastName .~ "Kox" $ client
    -- (&) operator flips parameters so value being updated at the beginning
    putStrLn $ show $ client & person.fullName .~ "Marianne Kox"
    -- many lens combinators resemble C/Java +=, *=, etc.
    -- name schema: name of operator followed by tilde:
    putStrLn $ show $ client & identifier +~ 2
    -- those ops are specific instances of general function `over` (infix (%~))
    -- takes function to apply to the field pointed by the lens
    putStrLn $ show $ client & over identifier (+2)
    putStrLn $ show $ client & person.fullName %~ (map toUpper)
    -- lenses _1 to _9 for tuple access (error if out-of-bounds)
    putStrLn $ show $ ("a","b") & set _1 "c"
    -- (^?) operator returns Maybe value
    -- (^?!) signals error if element not available
    -- update via same operators above
    putStrLn $ show $ "abc"^?_head
    putStrLn $ show $ "abc"^?!_tail
    putStrLn $ show $ "abc" & (_head .~ 'd')
    putStrLn $ show $ "abc" & (_tail %~ map toUpper)
    -- _last and _init
    putStrLn $ show $ "abc"^?_init
    putStrLn $ show $ "abc" & (_last %~ toUpper)
    -- check out lenses for lists, maps, sets, ...
    -- traversed lens: go inside a instance of Traversable and update each of element using a further lens
    -- e.g., given array of people, uppercase all first names
    let people = [Person "Jack" "Smith", Person "Marianne" "Branson"]
    putStrLn $ show $ people & traversed.firstName %~ map toUpper

-- exercise 6-2 - p. 142
-- given list of time machines, increase price of each by given percentage

data TimeMachine = TM { _manufacturer :: String
                      , _year         :: Integer
                      , _price        :: Float }
                   deriving (Eq, Show)

makeLenses ''TimeMachine

changePrice :: [TimeMachine] -> Float -> [TimeMachine]
changePrice tms x = tms & traversed.price *~ x

e62 :: IO ()
e62 = do
    let tms = [TM "foo" 1950 50.00, TM "bar" 2000 100.0, TM "baz" 2050 150.00]
    putStrLn $ show $ changePrice tms 0.50

------------------------------------------------------------------------------
-- kmeans with lenses - p. 142

-- kMeans' : series of changes in above state
-- - creates assignments
-- - updates centroids, error, number of steps (via lenses)
-- - check stopping condition by comparing error to threshold (in state)
-- - return centroids from state

data KMeansState e v = KMeansState { _centroids :: [v]
                                   , _points    :: [e]
                                   , _err       :: Double
                                   , _threshold :: Double
                                   , _steps     :: Int }
                     deriving Show

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState f n pts t = KMeansState (f n pts) pts (1.0/0.0) t 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans f n pts t = view centroids $ kMeans' (initializeState f n pts t)

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state' =
  let assignments = clusterAssignments state'
      state1 = state' &  centroids.traversed
                      %~ (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
      state2 = state1 &  err .~ sum (zipWith distance (state'^.centroids) (state1^.centroids))
      state3 = state2 &  steps +~ 1
   in if state3^.err < state3^.threshold then state3 else kMeans' state3

-- exercise 6-3 - p. 143

clusterAssignments :: (Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignments state' =
    let initialMap = M.fromList $ zip (state'^.centroids) (repeat [])
    in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                      (distance y $ toVector p))
                                                     (state'^.centroids)
                      in M.adjust (p:) chosenCentroid m)
             initialMap (state'^.points)

e63 :: IO ()
e63 = do
    -- [(4.0,4.5),(1.0,1.5)] -- Note - kMeans0 returned [(1.0,1.5),(4.0,4.5)]
    putStrLn $ show $ kMeans  generateCentroids  2  ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)])  0.001

------------------------------------------------------------------------------
-- discovering monads - p. 143
-- incomplete date (Maybe)

-- uses `catMaybes` : filters out Nothing elements

meanPurchase :: Integer -- the client identifier
             -> Double  -- the mean purchase
meanPurchase clientId = let p = purchasesByClientId clientId
                        in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  case numberItemsByPurchaseId purchaseId of
    Nothing -> Nothing
    Just n  -> case productIdByPurchaseId purchaseId of
                 Nothing        -> Nothing
                 Just productId -> case priceByProductId productId of
                                     Nothing    -> Nothing
                                     Just price' -> Just $ (fromInteger n) * price'

-- stubs
purchasesByClientId :: Integer -> [Integer]
purchasesByClientId _ = [1,2,3]
numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId _ = Just 10
productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId _ = Just 1
priceByProductId :: Integer -> Maybe Double
priceByProductId _ = Just 1.00

thenDoM :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDoM Nothing  _ = Nothing
thenDoM (Just x) f = f x

purchaseValue' :: Integer -> Maybe Double
purchaseValue' purchaseId =
    numberItemsByPurchaseId purchaseId `thenDoM` (\n ->
    productIdByPurchaseId   purchaseId `thenDoM` (\productId ->
    priceByProductId        productId  `thenDoM` (\price' ->
    Just $ fromInteger n * price'                )))

------------------------------------------------------------------------------
-- state - p. 145

-- represent a function that manipulates state : a -> s -> (b,s)
-- state prior to function execution is extra arg to function
-- - given function a -> b
-- - add state via a -> s -> b
-- functions needs to say state after execution
-- - state that is later passed to next function expecting state
-- - given function that returns b, pair it s

-- thenDoS :: (s -> (a,s)) -> (a -> s -> (b,s)) -> (s -> (b,s))

-- (a,s) -> (a -> s -> (b,s)) -> (b,s) might seem better
-- - thread state directly from initial computation to next function
-- - but real version more useful
--   - enables combining stateful computation for which the initial state is not yet present

type MyState s a = s -> (a, s)

-- now easier to see that it is like Maybe
thenDoS :: MyState s a -> (a -> MyState s b) -> MyState s b
-- thenDoS :: (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)
thenDoS f g s = let (resultOfF, stateAfterF) = f s
                in g resultOfF stateAfterF

-- note: thenDo can be defined:
-- thenDo f g = uncurry g . f

-- kmeans using state - p. 146

-- partitions args into info that is threaded and state and others that are not
-- does not use lenses (to focus discussion on state)

data KMeansStateS v = KMeansStateS { centroidsS :: [v]
                                   , thresholdS :: Double
                                   , stepsS     :: Int }
                      deriving Show

newCentroidsS :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroidsS = M.elems . fmap (centroid . map toVector)

clusterAssignmentsS :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentsS centrs points' =
    let initialMap = M.fromList $ zip centrs (repeat [])
    in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p)
                                                                      (distance y $ toVector p))
                                                     centrs
                      in M.adjust (p:) chosenCentroid m)
              initialMap points'

kMeansS' :: (Vector v, Vectorizable e v) => [e] -> MyState (KMeansStateS v) [v]
kMeansS' points' =
    (\s -> (centroidsS s,s))                                 `thenDoS` (\prevCentrs  ->
    (\s -> (clusterAssignmentsS prevCentrs points', s))      `thenDoS` (\assignments ->
    (\s -> (newCentroidsS assignments, s))                   `thenDoS` (\newCentrs   ->
    (\s -> ((), s { centroidsS = newCentrs }))               `thenDoS` (\_           ->
    (\s -> ((), s { stepsS = stepsS s + 1 }))                `thenDoS` (\_           ->
    (\s -> (thresholdS s, s))                                `thenDoS` (\t           ->
    (\s -> (sum $ zipWith distance prevCentrs newCentrs, s)) `thenDoS` (\err'        ->
    if err' < t then (\s -> (newCentrs, s)) else (kMeansS' points')    )))))))

initialState :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansStateS v
initialState i k pts t = KMeansStateS (i k pts) t 0

kMeansS :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansS i k pts t = fst $ kMeansS' pts (initialState i k pts t)

--------------------------------------------------
-- factor out state functions
-- return () when not used
remainS :: a -> (s -> (a,s)) -- i.e., RETURN (i.e., "unit")
remainS x = \s -> (x,s)
accessS :: (s -> a) -> (s -> (a,s))
accessS f = \s -> (f s, s)
modifyS :: (s -> s) -> (s -> ((), s))
modifyS f = \s -> ((), f s)

-- TODO: factor - only difference is calling kMeans'''
kMeansS'' :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansS'' i k pts t = fst $ kMeansS''' pts (initialState i k pts t)

kMeansS''' :: (Vector v, Vectorizable e v) => [e] -> MyState (KMeansStateS v) [v]
kMeansS''' points' =
    accessS centroidsS                                    `thenDoS` (\prevCentrs ->
    remainS (clusterAssignmentsS prevCentrs points')      `thenDoS` (\assignments ->
    remainS (newCentroidsS assignments)                   `thenDoS` (\newCentrs ->
    modifyS (\s -> s { centroidsS = newCentrs })          `thenDoS` (\_ ->
    modifyS (\s -> s { stepsS = stepsS s + 1 })           `thenDoS` (\_ ->
    accessS thresholdS                                    `thenDoS` (\t ->
    remainS (sum $ zipWith distance prevCentrs newCentrs) `thenDoS` (\err' ->
    if err' < t then remainS newCentrs else kMeansS''' points'      )))))))

kms :: IO ()
kms = do
    putStrLn $ show $ kMeansS    generateCentroids  2  ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)])  0.001
    putStrLn $ show $ kMeansS''  generateCentroids  2  ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)])  0.001

------------------------------------------------------------------------------
-- Monad - p. 148

-- exercise 6-5 - p. 149

purchaseValue'' :: Integer -> Maybe Double
purchaseValue'' purchaseId =
    numberItemsByPurchaseId purchaseId >>= (\n ->
    productIdByPurchaseId   purchaseId >>= (\productId ->
    priceByProductId        productId  >>= (\price' ->
    return $ fromInteger n * price'        )))

-- do - p. 151

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseId = do
    n         <- numberItemsByPurchaseId purchaseId
    productId <- productIdByPurchaseId purchaseId
    price'    <- priceByProductId productId
    return $ fromInteger n * price'

------------------------------------------------------------------------------
-- kmeans monadic - p. 152

kMeansM' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansStateS v) [v]
kMeansM' points' = do
    prevCentrs   <- fmap centroidsS get
    let assignments = clusterAssignmentsS prevCentrs points'
        newCentrs   = newCentroidsS assignments
    modify (\s -> s { centroidsS = newCentrs })
    modify (\s -> s { stepsS = stepsS s + 1 })
    t <- fmap thresholdS get
    let err' = sum $ zipWith distance prevCentrs newCentrs
    if err' < t then return newCentrs else kMeansM' points'

-- note: version in book left out initialization function
kMeansM :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansM i n pts t = evalState (kMeansM' pts) (initializeStateM i n pts t)

-- note: book left this out
initializeStateM :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansStateS v
initializeStateM    i                      n      pts    t       = KMeansStateS (i n pts) t 0

kmm :: IO ()
kmm = do
    putStrLn $ show $ kMeansM    generateCentroids  2  ([(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)])  0.001

------------------------------------------------------------------------------
-- state and lenses - p. 153

-- Instead of `get` and then applying lens with view
-- directly access via `use`
-- `use` gives result in State monad (eliminates need to call fmap or return)

-- before to use update functions for lenses, had to write structure to be applied by explicitly using either $ or &
-- inside State can use internal state
-- For each update function ending in tilde (such as .~, %~ or +~)
-- lens library has corresponding function ending in equals sign (.=, %= or += in the previous cases)
-- which changes the internal state

-- if state is data type with several fields with lenses for it
-- then can use syntax close to C to change state

data KMeansStateL v = KMeansStateL { _centroidsL :: [v]
                                   , _thresholdL :: Double
                                   , _stepsL     :: Int }
                   deriving Show

makeLenses ''KMeansStateL

-- `use` to get information or temporarily save it
-- (.=) and (+=) to update centroids/steps
kMeansL' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansStateL v) [v]
kMeansL' points' = do
    prevCentrs <- use centroidsL
    let assignments = clusterAssignmentsS prevCentrs points'
        newCentrs   = newCentroidsS assignments
    centroidsL .= newCentrs
    stepsL += 1
    let err' = sum $ zipWith distance prevCentrs newCentrs
    t <- use thresholdL
    if err' < t then return newCentrs else kMeansL' points'

------------------------------------------------------------------------------
-- lenses ZOOM - p. 154

-- Zooming takes
-- - lens and computation
-- uses as internal state the information contained in that lens
-- focuses on a small part of structure

-- e.g., increment identifiers of list of Clients and update names to uppercase
data ExampleState = ExampleState { _increment :: Int
                                 , _clients   :: [Client Int] }
                    deriving Show

makeLenses ''ExampleState

zoomExample :: State ExampleState ()
zoomExample = do
    n <- use increment
    zoom (clients.traversed) $ do
        identifier += n
        person.fullName %= map toUpper

doZoom :: IO ()
doZoom = do
  let client1 = Individual 4 (Person "John" "Smith")
  let client2 = Individual 3 (Person "Albert" "Einstein")
  putStrLn $ show $ execState zoomExample (ExampleState 2 [client1, client2])

------------------------------------------------------------------------------
-- Reader, Writer and RWS - p. 155

-- READER monad (in "mtl") for cases where state contains constant values
   -- e.g., K-means number-of-clusters, points, threshold
-- use type system to ensure no modifications

-- context viewed as extra, hidden, argument to functions, is a monad
-- monad representing computations that use context that cannot change

{-
-- return :: a -> (r -> a)
-- return x = \r -> x
-- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
-- f >>= g = \r -> g (f r) r

instance Monad ((->) r) where
    f >>= g = \r -> g (f r) r
    return x = \r -> x

-- `ask` : retrieves complete context (like State `get`)
-- `asks` : applies function to context and returns result (similar to State `gets`)
    - useful to query a specific field in a structure

-- use-case : app config
-- - instead of explicit Settings parameter in every function of application
-- - wrap it in Reader monad
-}
data Settings e v = Settings { i    :: Int -> [e] -> [v]
                             , k    :: Int
                             , th   :: Double
                             , user :: Person }

kMeansMain :: (Vector v, Vectorizable e v) => [e] -> Reader (Settings e v) [v]
kMeansMain points' = do
    i' <- asks i
    k' <- asks k
    t' <- asks th
    return $ kMeans i' k' points' t'

-- function to execute monad, given the constnat context : `runReader`
-- mtl includes mechanism for executing code with a local context via `local`, given
-- - function to modify current state
-- - computation to perform
-- in computation, calls to `ask` or `asks` refer to modified context
-- will return to original context after local call done
-- e.g., compare run of K-means with k clusters and k+1 clusters
compareClusters :: (Vector v, Vectorizable e v) => [e] -> Reader (Settings e v) ([v], [v])
compareClusters points' = do
    c1 <- kMeansMain points'
    c2 <- local (\s -> s { k = k s + 1 })
                (kMeansMain points')
    return (c1, c2)

------------------------------------------------------------------------------
-- WRITER monad - p. 156

-- generate state, but never look back at it (e.g., logging)

-- combine new value to existing output state via an instance of Monoid
-- e.g., log of strings: monoid is list; neutral element is empty list; combine two strings via concatenation
-- e.g., number-of-steps in K-means : monoid is integer; 0; sum
   -- because numbers have two monoid structurs (one for addition and one for product)
   -- wrap values inside Sum newtype to use addition as operation.
   -- modify output state with a new value via `tell` function

accessDatabase :: Writer String ()
accessDatabase = do
    tell "Start database access"
    info <- readInformation
    _    <- computeValue info
    tell "Finish database access"

-- stubs
readInformation :: WriterT String Identity Int
readInformation = do return 1
computeValue :: Int -> WriterT String Identity Int
computeValue _ = do return 1

-- initial value for output information taken as neutral element of corresponding monoid
-- so do not need an extra argument to run a Writer monad value using runWriter
-- that returns tuple with both return value of computation and output info

------------------------------------------------------------------------------
-- exercise 6-6 - p. 157

-- RIGHT HERE

-- ===========================================================================

main :: IO ()
main = do
    tryKMeans0
    lensEx
    e62
    e63
    kms
    kmm
    doZoom

-- End of file.
