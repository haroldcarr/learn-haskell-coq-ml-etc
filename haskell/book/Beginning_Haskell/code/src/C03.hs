{-
Created       : 2014 Feb                   by Harold Carr.
Last Modified : 2014 Jun 30 (Mon) 09:41:31 by Harold Carr.
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ViewPatterns      #-}

module C03 where

import           C02
import           Data.Char       (toUpper)
import           Data.List       (delete, find, unfoldr)
import qualified Data.Map        as M
import           GHC.Exts        (groupWith, sortWith, the)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- exercise 3-1 - p. 50

swapTriple :: (a,b,c) -> (b,c,a)
swapTriple    (x,y,z)  = (y,z,x)

duplicate :: a -> (a,a)
duplicate    x  = (x,x)

nothing :: a -> Maybe a
nothing    _  = Nothing

index :: [a]   -> [(Int,a)]
index    []     = []
index    [x]    = [(0,x)]
index    (x:xs) = let indexed@((n,_):_) = index xs
                  in  (n+1,x):indexed

maybeA :: [a] -> Char
maybeA    _    = 'a'

------------------------------------------------------------------------------
-- exercise 3-2 - p. 53

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (==1)

filterANumber :: Eq a => a -> [a] -> [a]
filterANumber n = filter (==n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\x a -> if not $ f x then x:a else a) []

isGovOrg :: Client -> Bool
isGovOrg (GovOrg _) = True
isGovOrg _          = False

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter isGovOrg

e32 :: T.Test
e32 = T.TestList
    [
      U.teq "321" (filterOnes      [1,2,1,2,3,2,1])   [1,1,1]
    , U.teq "322" (filterANumber 3 [1,2,1,2,3,2,1])   [3::Int]
    , U.teq "323" (filterNot (==1) [1,2,1,2,3,2,1])   [2,2,3,2::Int]
    , U.teq "324" (isGovOrg (clients !! 3))           True
    , U.teq "325" (filterGovOrgs clients)             [GovOrg "NSA"]
    ]

------------------------------------------------------------------------------
-- smart constructors and views - p.59

-- does not prevent incorrect values
data Range = Range Integer Integer deriving Show

-- provide "smart constructor" : range check/creation function
range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

-- enforce use of this constructor by not exporting Range constructor -- only the type:
-- module Chapter3.Ranges (Range(), range) where
-- But need regular constructor to do pattern matching outside of this module: case ... of Range x y -> ...
-- Solution: create new data type that encodes observed values of that type and then use views when pattern matching.

data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b

-- export RangeObs constructor (R) and pattern match using a view (use ViewPatterns extension)

prettyRange :: Range -> String
prettyRange rng = case rng of
                      (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"

------------------------------------------------------------------------------
-- exercise 3-3 - p. 64

-- product       : computes the product of a list of integers
-- minimumClient : computes the Client with the shortest name
-- all           : computes conjunction (&&) of a list of Boolean values

-- Write the functions using pattern matching, without using higher-order function.
-- Write the functions as folds. in each case, first try to find the aggregation operation, and from that derive a sensible initial value.

pduct :: Num a => [a] -> a
pduct [] = 1
pduct (x:xs) = x * pduct xs
pduct' :: Num a => [a] -> a
pduct' = foldr (*) 1

shorterName :: Client -> Client -> Client
shorterName x y | length (clientName x) < length (clientName y) = x
                | otherwise                                     = y

minClient  :: [Client] -> Client
minClient     []  = error "bad"
minClient  (x:[]) = x
minClient  (x:xs) = shorterName x $ minClient xs
minClient' :: [Client] -> Client
minClient'    []  = error "bad"
minClient' (x:xs) = foldr shorterName x xs

alll :: [Bool] -> Bool
alll [] = True
alll (x:xs) = x && alll xs
alll' :: [Bool] -> Bool
alll' = foldr (&&) True

-- common structure of explicit recursion
com :: (a -> a -> a) -> a -> [a] -> a
com _ s [] = s
com f s (x:xs) = f x $ com f s xs

cpduct :: Num a => [a] -> a
cpduct = com (*) 1
cminClient :: [Client] -> Client
cminClient [] = error "Bad"
cminClient (x:[]) = x
cminClient (x:xs) = com shorterName x xs
calll :: [Bool] -> Bool
calll = com (&&) True

-- TODO: In which cases is it true that using foldr and foldl give the same results?

-- example: minimumBy (\x -> -x) [1,2,3] should return 3.
minimumBy :: (Num b, Ord b) => (a -> b) -> [a] -> a
minimumBy f xs =
    let (fResult, mapResult) = foldl (\(acc, accMap) x ->
                                       let m = min (f x) acc in (m, M.insert m x accMap))
                               (0, M.fromList [])
                               xs
    in mapResult M.! fResult

e33 :: T.Test
e33 = T.TestList
    [
      U.teq "e3310" (pduct  [1,2,3::Int])                   6
    , U.teq "e3311" (pduct' [1,2,3::Int])                   6
    , U.teq "e3312" (cpduct [1,2,3::Int])                   6
    , U.teq "e3320" (minClient  clients)                    (GovOrg "NSA")
    , U.teq "e3321" (minClient' clients)                    (GovOrg "NSA")
    , U.teq "e3322" (cminClient clients)                    (GovOrg "NSA")
    , U.teq "e3330" (alll  [True,True,True])                True
    , U.teq "e3331" (alll' [True,True,True])                True
    , U.teq "e3332" (calll [True,True,True])                True
    , U.teq "e3333" (alll  [True,False,True])               False
    , U.teq "e3334" (alll' [True,False,True])               False
    , U.teq "e3335" (calll [True,False,True])               False

    , U.teq "e3350" (minimumBy (\x -> (-x)) [1,2,3::Int])   3

    ]

------------------------------------------------------------------------------
-- LambdaCase - p. 64

skipUntilGov :: [Client] -> [Client]
skipUntilGov = dropWhile (\case { GovOrg {} -> False ; _ -> True })
-- skipUntilGov clients

-- p. 66

elem' :: Eq a => a -> [a] -> Bool
elem' x l = case find (==x) l of
                Just _  -> True
                Nothing -> False
-- elem' 2 [1,2,3]

-- ordering - p. 66

compareClient :: ClientR -> ClientR -> Ordering
compareClient (IndividualR {person = p1}) (IndividualR {person = p2})
                                 = compare (firstName p1) (firstName p2)
compareClient (IndividualR {}) _ = GT
compareClient _ (IndividualR {}) = LT
compareClient c1 c2              = compare (clientNameR c1) (clientNameR c2)

listOfClients :: [ClientR]
listOfClients = [ IndividualR (PersonR "H. G." "Wells" Male)   True
                , GovOrgR     "NTTF"
                , CompanyR    "Wormhole Inc." 1 (PersonR "Karl" "Schwarzschild" Male) "Physicist"
                , IndividualR (PersonR "Doctor" ""     Female) True
                , IndividualR (PersonR "Sarah" "Jane"  Female) True
                ]
-- sortBy compareClient listOfClients

-- list comprehensions - p. 70

doubleOdds :: Integral t => [t] -> [t]
doubleOdds list = [ 2 * x | x <- list, odd x ]
-- doubleOdds [1,2,3]

govOrgs :: [String]
govOrgs = [ clientNameR x | x@(GovOrgR {}) <- listOfClients ]

foo :: [(Integer, Integer, Integer)]
foo = [(x,y,x*y) | x <- [1 .. 4], y <- [1 .. 10]]

baz :: [Char]
baz = [toUpper c | s <- ["This","is","a","list"], c <- ' ':s ]

bar :: [(Integer, Integer)]
bar = [(x,y)     | x <- [0 .. 6], y <- [x .. 6]]

boo :: [Double]
boo = [ sqrt v   | (x,y) <- [(1,2),(3,8)], let v = x*x+y*y ]

grd :: [(Integer, Integer)]
grd = [(x,y)     | x <- [1 .. 6], y <- [1 .. 6], x <= y]

-- TransformListComp - p. 71

thn :: [Integer]
thn = [x*y | x <- [-1,1,-2], y <- [1,2,3], then reverse]

srw :: [Integer]
srw = [x*y | x <- [-1,1,-2], y <- [1,2,3], then sortWith by x]

gby :: [(Bool, [Integer])]
gby = [ (the p, m) | x <- [-1,1,-2]
                   , y <- [1,2,3]
                   , let m = x*y
                   , let p = m > 0
                   , then group by p using groupWith ]

{-
companyAnalytics :: [ClientR] -> [(String, [(PersonR, String)])]
companyAnalytics clients = [ (the client, zip person duty)
                           | client@(CompanyR { .. }) <- clients
                           , then sortWith by duty
                           , then group by clientNameR using groupWith
                           , then sortWith by length client
                           ]
-}

-- ParallelListComp - p. 73

usualNesting :: [Integer]
usualNesting = [ x*y | x <- [1,2,3] , y <- [1,2,3] ] -- [1,2,3,2,4,6,3,6,9]

zipping :: [Integer]
zipping      = [ x*y | x <- [1,2,3] | y <- [1,2,3] ] -- [1,4,9]

-- unfold - p. 75.

enumUnfold :: Int -> Int -> [Int]
enumUnfold n m = unfoldr (\x -> if x > m then Nothing else Just (x, x+1)) n

minSort :: [Integer] -> [Integer]
minSort = unfoldr (\case [] -> Nothing
                         xs -> Just (m, delete m xs) where m = minimum xs)

------------------------------------------------------------------------------
c03 :: IO T.Counts
c03 = do
    _ <- T.runTestTT e32
    T.runTestTT e33

-- End of file.
