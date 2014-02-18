{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ViewPatterns      #-}

module C03 where

import           C02
import           Data.Char (toUpper)
import           Data.List (delete, find, unfoldr)
import           GHC.Exts  (groupWith, sortWith, the)

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

-- exercise 3-2 - p. 53

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (==1)
-- filterOnes [1,2,1,2,3,2,1]

filterANumber :: Eq a => a -> [a] -> [a]
filterANumber n = filter (==n)
-- filterANumber 3 [1,2,1,2,3,2,1]

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\x a -> if not $ f x then x:a else a) []
-- filterNot (==1) [1,2,1,2,3,2,1]

isGovOrg :: Client -> Bool
isGovOrg (GovOrg _) = True
isGovOrg _          = False

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter isGovOrg

-- filterGovOrgs clients


-- smart constructors and views

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

-- exercise 3-3 - p. 64

-- product : computes the product of a list of integers
-- minimumClient : computes the cClient with the shortest name
-- all : . computes conjunction (&&) of a list of Boolean values

-- Write the functions using pattern matching, without resorting to any higher-order function.
-- Write the functions as folds. in each case, first try to find the aggregation operation, and from that derive a sensible initial value.

pduct :: Num a => [a] -> a
pduct [] = 1
pduct (x:xs) = x * pduct xs
pduct' :: [Integer] -> Integer
pduct' = foldr (*) 1
-- pduct [1,2,3]
-- pduct' [1,2,3]

shorterName :: Client -> Client -> Client
shorterName x (GovOrg "BAD") = x
shorterName x y  = if length (clientName x) < length (clientName y) then x else y

minClient :: [Client] -> Client
minClient [] = GovOrg "BAD"
minClient (x:xs) = shorterName x $ minClient xs
minClient' :: [Client] -> Client
minClient' = foldr shorterName (GovOrg "BAD")
-- minClient clients
-- minClient' clients

alll :: [Bool] -> Bool
alll [] = True
alll (x:xs) = x && alll xs
alll' :: [Bool] -> Bool
alll' = foldr (&&) True
-- alll  [True,True,True]
-- alll' [True,True,True]
-- alll  [True,False,True]
-- alll' [True,False,True]

-- TODO: In which cases is it true that using foldr and foldl give the same results?

-- minimumBy (\x -> -x) [1,2,3] should return 3.
minimumBy :: (Num b, Ord b) => (a -> b) -> [a] -> b
minimumBy g = foldr (\x a -> min (g x) a) 0
-- minimumBy (\x -> (-x)) [1,2,3]

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

-- End of file.
