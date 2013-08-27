
import RecursiveContents -- from this Directory
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Either
import Data.List
import Data.Ord
import Data.Maybe (fromJust)
import Debug.Trace
import System.FilePath

debug = flip trace

-- 2 and 3

lastButOne  ::  [a] -> a
lastButOne       [] = error "empty list"
lastButOne      [_] = error "list of one element"
lastButOne    [x,_] = x
lastButOne   (_:xs) = lastButOne xs

--                           ID  TITLE  AUTHORS
data BookInfo     = Book     Int String [String]
    deriving (Show)

data MagazineInfo = Magazine Int String [String]
    deriving (Show)

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

data MyBool = MyFalse | MyTrue

type CardHolder  = String
type CardNumber  = String
type Address     = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                 deriving (Show)

type Vector = (Double, Double)
data Shape  = Circle Vector Double
            | Poly  [Vector]

bookID      (Book id _     _      ) = id
bookTitle   (Book _  title _      ) = title
bookAuthors (Book _  _     authors) = authors

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
} deriving (Show)

-- "a" is a type variable
data MyMaybe a = MyJust a
               | MyNothing

data MList a = MCons a (MList a)
             | MNil
               deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Eq, Show)

t0  = Empty
t1  = Node 1 Empty                Empty
t3  = Node 2 t1                   (Node 3 Empty Empty)
t4  = Node 2 t1                   (Node 3 Empty (Node 4 Empty Empty))

t0' = Empty
t1' = Node 1 Empty                Empty
t3' = Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)
t4' = Node 2 (Node 1 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))

-- 1

-- from haskell list to "my" list
fromHList (      x:xs) = MCons x (fromHList xs)
fromHList           [] = MNil

fromMList (MCons x xs) = x:fromMList xs
fromMList         MNil = []

-- 2

data Tree' a = Tree' a (Maybe (Tree' a)) (Maybe (Tree' a)) deriving (Show)
t0'' = Nothing
t1'' = Tree' 1 Nothing     Nothing
t3'' = Tree' 2 (Just t1'') (Just (Tree' 3 Nothing Nothing))
t4'' = Tree' 2 (Just t1'') (Just (Tree' 3 Nothing (Just (Tree' 4 Nothing Nothing))))

lastButOne' :: [a] -> Maybe a
lastButOne'     []  = Nothing
lastButOne'    [_]  = Nothing
lastButOne'  [x,_]  = Just x
lastButOne' (_:xs)  = lastButOne' xs

lend amount balance  = let reserve    = 100
                           newBalance = balance - amount
                       in if balance < reserve
                          then Nothing
                          else Just newBalance

lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

myFromMaybe defaultValue wrapped =
    case wrapped of
        Nothing    -> defaultValue
        Just value -> value

nodesAreSame (Node a _ _) (Node b _ _) | a == b = Just a
nodesAreSame            _            _          = Nothing

lend3 amount balance | amount <= 0            = Nothing
                     | amount > reserve * 0.5 = Nothing
                     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount

dropper n xs = if n <= 0 || null xs
               then xs
               else dropper (n - 1) (tail xs)

dropper' n     xs | n <= 0 = xs
dropper' _     []          = []
dropper' n (_:xs)          = dropper (n - 1) xs

-- 1 and 2
myLength :: Num a => [t] -> a
myLength     [] = 0
myLength (x:xs) = 1 + (myLength xs)

testMyLength l = myLength l == length l

-- 3
mean l = sum l / fromIntegral (length l)

-- 4
palindrome x = x ++ (reverse x)

-- 5
-- TODO extend this to handle lists of odd length
isPalindrome x | not (even (length x)) = False
               | otherwise =
                     let n = truncate $ fromIntegral (length x) / 2 `debug` show (truncate $ fromIntegral (length x) / 2)
                         t = (take n x)                             `debug` show (take n x)
                         d = (drop n x)                             `debug` show (drop n x)
                     in t == reverse d

-- 6
six = sortBy (\x y -> if length x < length y
                      then LT
                      else if length x == length y
                           then EQ
                           else GT)
             [[1,2], [1,2,3,4], [1], []]

-- 7 and 8

intersperse' :: a -> [[a]] -> [a]
intersperse' s xs = concat (i s xs)
    where i _    []   = []
          i s (x:[] ) = [x]
          i s (x:xs') = x : [s] : (i s xs')

-- 9 max height of tree
-- TODO: UNDERSTAND BETTER

flatten traversal = reverse . traversal (:) []

-- "fold" f through the values in a tree
traverse :: (t2 -> (t -> t) -> (t -> t) -> t -> t)
            -> (t1 -> t2)
            -> t
            -> Tree t1
            -> t
traverse step f z tree = go tree z
  where
    go Empty        z = z
    go (Node v l r) z = step (f v) (go l) (go r) z

preorder  :: (t -> b -> b) -> b -> Tree t -> b
preorder   = traverse $ \n l r -> r . l . n

inorder   :: (t -> b -> b) -> b -> Tree t -> b
inorder    = traverse $ \n l r -> r . n . l

postorder :: (t -> b -> b) -> b -> Tree t -> b
postorder  = traverse $ \n l r -> n . r . l

test1p = flatten preorder  t3  -- [2,1,3]
test1i = flatten inorder   t3  -- [1,2,3]
test1o = flatten postorder t3  -- [1,3,2]

-- exercise answer
ninep3 = preorder max minBound t3
-- 3

allMax  = map (\f -> map (f (max) minBound) [t0,t1,t3,t4]) [(preorder),(inorder),(postorder)]
-- [[-9223372036854775808,1,3,4],[-9223372036854775808,1,3,4],[-9223372036854775808,1,3,4]]

allCons = map (\f -> map (f (:)   [])       [t0,t1,t3,t4]) [(preorder),(inorder),(postorder)]
-- [[[],[1],[3,1,2],[4,3,1,2]],[[],[1],[3,2,1],[4,3,2,1]],[[],[1],[2,3,1],[2,3,4,1]]]

-- just traverse left or right

leftorder  = traverse $ \n l r -> l . n
rightorder = traverse $ \n l r -> r . n

treemin = leftorder  min maxBound
treemax = rightorder max minBound

test2l = treemin t3 :: Int
test2r = treemax t3 :: Int

-- 10

data Point = Point Int Int     deriving (Eq, Show)
data Direction = DLeft     Point Point Point
               | DStraight Point Point Point
               | DRight    Point Point Point
                 deriving (Eq, Show)

-- 11

-- TODO: need trigonometry to do this...
turn p1@(Point x1 y1) p2@(Point x2 y2) p3@(Point x3 y3) =
    DLeft p1 p2 p3

turn' :: Point -> Point -> Point -> Direction
turn' p1 p2 p3 =
    DLeft p1 p2 p3

-- 12

turns :: [Point] -> [Direction]
turns l@(p1:p2:p3:ps) = turn p1 p2 p3 : turns (tail l)
turns               _ = []
-- let ts = turns [Point 1 1, Point 2 2, Point 3 1, Point 5 6, Point (-1) 3, Point 4 0]
-- length ts

-- 13 TODO - p 70/110

data a `Pair` b = a `Pair` b deriving (Show)
fooPair = Pair 1 2.0
barPair = True `Pair` "quux"

-- 1

safeHead :: [a] -> Maybe a
safeHead     []  = Nothing
safeHead (x:xs)  = Just x

safeTail :: [a] -> Maybe [a]
safeTail     []  = Nothing
safeTail (x:xs)  = Just xs

safeLast :: [a] -> Maybe a
safeLast     []  = Nothing
safeLast    [x]  = Just x
safeLast (x:xs)  = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit     []  = Nothing
safeInit    [x]  = Just []
safeInit (x:xs)  = Just (x : (fromJust (safeInit xs)))

-- 2
-- similar to words but takes predicate and works on any type

-- Note:
--   both versions split on true (rather than false of exercise)
--   both versions retain the split character (rather than discard it)

-- Version written January 2012
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f x = splitWith' f x []
    where splitWith' _     [] acc             = [reverse acc]
          splitWith' f (x:xs) acc | f x       =  reverse acc : splitWith' f xs [x]
                                  | otherwise =                splitWith' f xs (x : acc)

-- Version written July 2013 (i.e., no reverse)
swt _ [] = []
swt f xs =
    let (sp,cont) = sw f xs -- `debug` show (sw f xs)
    in
        case sp of
            [] -> [cont]
            _  -> case cont of
                      []     -> [sp]
                      x':xs' -> sp : (x' : head step) : (tail step) where step = swt f xs'

sw _     [] = ([], [])
sw f (x:xs) | f x       = ([], x:xs) -- TODO use @
            | otherwise = (x:sp, cont) where (sp,cont) = sw f xs

-- The beginning of a third version July 2013
sw' _ (x:[]) = ([], x, [])
sw' f (x:xs) | f x       = ([], x, xs) -- use @
             | otherwise = (x:b, sp, cont) where (b,sp,cont) = sw' f xs

-- 3 print first word of each line

firstWord x = map (head . words) (lines x)

-- 4 transpose text (e.g., "hello\nworld\n" to "hw\neo\nlr\nll\nod\n")

transposeText x = unlines $ map (\(x,y) -> x:y:[]) (zip (lins!!0) (lins!!1)) where lins = lines x

-- "zero" is initial value and accumulator
myFoldl :: (a -> b -> a) -> a -> [b]   -> a
myFoldl    step             zero (x:xs) = myFoldl step (step zero x) xs
myFoldl    _                zero []     = zero

foldlSum xs = myFoldl step 0 xs
    where step acc x = acc + x

niceSum :: [Integer] -> Integer
niceSum = foldl (+) 0

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []                 = []
filter' p (x:xs) | p x       = x : filter' p xs
                 | otherwise =     filter' p xs

myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise =     ys

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr step [] xs
    where step x ys = f x : ys

myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' f z xs = foldr step id xs z
    where step x g a = g (f a x)

identity :: [a] -> [a]
identity xs = foldr (:) [] xs

-- ++
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- 1, 2 and 3 write asInt from p 85/125 (repeated below) using fold? and error

loop :: Int -> String -> Int
loop acc       [] = acc
loop acc ('-':xs) = - (loop acc xs)
loop acc   (x:xs) = let acc' = acc * 10 + digitToInt x
                    in loop acc' xs
asInt :: String -> Int
asInt xs = loop 0 xs

asInt' xs = if not (null xs) && head xs == '-'
            then - ai (tail xs)
            else   ai       xs
    where ai xs = foldl (\acc x -> acc * 10 + dig x) 0 xs
          dig x = if isDigit x then digitToInt x else error ("wrong: " ++ (show x))

-- 4 Use Data.Either with above
-- TODO : there must be a better way
aaInt' xs = if not (null xs) && head xs == '-'
            then m (ai (tail xs))
            else    ai       xs
    where m (Left  a)   = Left    a
          m (Right b)   = Right (-b)
          ai xs         = foldl (\acc x -> d acc x) (Right 0) xs
          d (Left  a) _ = Left a
          d (Right b) x = dig b x
          dig acc x     = if isDigit x
                          then Right (acc * 10 + (digitToInt x))
                          else Left ("wrong: " ++ (show x))

-- 5 and 6 concat using foldr

concat' = foldr (++) []

-- 7 takeWhile recursive

tw _     []             = []
tw f (x:xs) | f x       = x:(tw f xs)
            | otherwise = []

-- 7 takeWhile foldr

tw' f = foldr (\x acc -> if (f x) then x:acc else []) []

-- 8 and 9 Data.List (groupBy) : use ghci to figure out what it does then write your own with a fold

gbr   :: (a -> a -> Bool) -> [a] -> [[a]]
gbr _ []       = []
gbr f xs       = foldr (gb') [[last xs]] (init xs)
    where gb' y ((y':ys):yss) = if f y y' then ((y:y':ys):yss) else [y]:((y':ys):yss)

-- only difference from gbr is order of operands to F in where clause
gbr'  :: (a -> a -> Bool) -> [a] -> [[a]]
gbr' _ []      = []
gbr' f xs      = foldr (gb') [[last xs]] (init xs)
    where gb' y ((y':ys):yss) = if f y' y then ((y:y':ys):yss) else [y]:((y':ys):yss)

gbl   :: (a -> a -> Bool) -> [a] -> [[a]]
gbl _ []       = []
gbl f (x:xs)   = foldl (gb') [[x]] xs
    where gb' ((y':ys):yss) y = if f y y' then (((y':ys)++[y]):yss) else ((y':ys):yss++[[y]])

-- only difference from gbl is order of operands to F in where clause
gbl'  :: (a -> a -> Bool) -> [a] -> [[a]]
gbl' _ []      = []
gbl' f (x:xs)  = foldl (gb') [[x]] xs
    where gb' ((y':ys):yss) y = if f y' y then (((y':ys)++[y]):yss) else [y]:((y':ys):yss)

-- only difference from gbl' is reverse
-- NOTE: this one behaves like groupBy (courtesy REVERSE)
gbl'' :: (a -> a -> Bool) -> [a] -> [[a]]
gbl'' _ []     = []
gbl'' f (x:xs) = reverse $ foldl (gb') [[x]] xs
    where gb' ((y':ys):yss) y = if f y' y then (((y':ys)++[y]):yss) else [y]:((y':ys):yss)

-- TODO try to write using a fold and span (see official groupBy definition)

testGb gb = map (\(op,name) -> map (\xs -> let mygb = gb (op) xs
                                               hsgb = groupBy (op) xs
                                           in if mygb == hsgb then ("", [], [[]], [[]]) else (name, xs, mygb,hsgb))
                                   [[4,2,3,1,3,5,2,3,2,1], [], [1,2,3,4,5,6,7,8,9], [1,2,2,4,5,5,7,2,5], [1,2,2,2,3,4,4,2]])
                [((>)                         , ">")
                ,((==)                        , "==")
                ,((/=)                        , "/=")
                ,((\x y -> x `mod` y == 0)    , "(\\x y -> x `mod` y == 0)")
                ,((\x y -> (x*y `mod` 3) == 0), "(\\x y -> (x*y `mod` 3) == 0)")
                ]

printTestGb :: Show a => [[a]] -> IO [()]
printTestGb     [] = return [()]
printTestGb (x:xs) = do
    putStrLn $ show x
    printTestGb xs

groupBy'                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _  []           =  []
groupBy' eq (x:xs)       =  (x:ys) : groupBy' eq zs
                            where (ys,zs) = span (eq x) xs

-- 10 Write using folds if possible

-- any using foldr
anyr f = foldr (\x acc -> acc || f x) False

-- any using foldl
anyl f = foldl (\acc x -> acc || f x) False

-- cycle
-- cannot be implemented as a fold since producing an infinite list

-- words
-- the REAL definition:
words' :: String -> [String]
words' s =  case dropWhile isSpace s of
                "" -> []
                s' -> w : words s''
                    where (w, s'') = break isSpace s'
-- TODO: not sure if this can be a fold

-- unlines

unlinesr = foldr (\x acc -> if acc == "" then x++"\n" else x++"\n"++acc) ""
unlinesl = foldl (\acc x -> if acc == "" then x++"\n" else acc++x++"\n") ""

{-
John Hughes : "The Design of a Pretty-Printing library"
http://citeseer.ist.psu.edu/hughes95design.html
Improved by Simon Peyton Jones
Included in Haskell

This chapter based on simpler Philip Wadler's "A prettier printer"
http://citeseerx.ist.psu.edu/viewdoc/summary?doi =10.1.1.19.635
Extended by Daan Leijen.
Install:
cabal install wl-pprint.

ghci

SimpleJSON.hs
PutJSON.hs

-- produces
--   *.hi : interface file for use when compiling modules that use it
--   *.o  : object file
ghc -c SimpleJSON.hs
ghc -c PutJSON.hs

:l SimpleJSON
getString (JString "hello")                   ==  Just "hello"
getString (JNumber 3)                         ==  Nothing
:l PutJSON
let json = JObject [("foo", JNumber 1), ("bar", JBool False), ("boo", JArray [JString "baz", JNull])]
print json
renderJValue json
putJValue json

-- intercalate is used by PutJSON
:module Data.List
:i intercalate
intercalate :: [a] -> [[a]] -> [a] 	-- Defined in Data.List
-- NO: see type: intercalate  0  [ 1,  2,  3,  4,  5]
intercalate [0] [[1],[2],[3],[4],[5]]         ==  [1,0,2,0,3,0,4,0,5]
intercalate "," ["a","b","c","d"]             ==  "a,b,c,d"

:i intersperse
intersperse :: a -> [a] -> [a] 	-- Defined in Data.List
intersperse  0  [ 1,  2,  3,  4,  5]          ==  [1,0,2,0,3,0,4,0,5]
intersperse [0] [[1],[2],[3],[4],[5]]         ==  [[1],[0],[2],[0],[3],[0],[4],[0],[5]]
intersperse ',' "abcd"                        ==  "a,b,c,d"

-- following file cats PrettyJSON and Prettify together so I can get inside
PrettyJSON.hs
Prettify.hs
PrettyJSONPrettify.hs
:l PrettyJSONPrettify
text "foo" <> text "bar"                      ==  Concat (Text "foo") (Text "bar")
text "foo" <> empty                           ==  Text "foo"
empty <> text "bar"                           ==  Text "foo"
let json = JObject [("foo", JNumber 1), ("bar", JBool False), ("boo", JArray [JString "baz", JNull])]
:t json
json :: JValue
json
let jvalue = renderJValue json
:type jvalue
jvalue :: Doc
jvalue
compact jvalue
putStrLn (compact jvalue)
empty </> char 'a'                            ==  Concat (Union (Char ' ') Line) (Char 'a')
2 `fits` " a"                                 ==  True
2 `fits` "          a"                        ==  False
putStrLn (pretty 10 jvalue)
putStrLn (pretty 20 jvalue)
putStrLn (pretty 30 jvalue)

-- exercises - p 130/170

-- fill TODO
fill :: Int -> Doc -> Doc

-- add support for nesting TODO



-- creating a package using Cabal - p 131/171

ghc-pkg        list
ghc-pkg --user list

PrettyJSON.cabal
PrettyJSONSetup.hs

runghc PrettyJSONSetup configure
runghc PrettyJSONSetup build

ll -R dist

-- TODO INSTALL
-- DOES NOT WORK
cabal install prettyjson --dry-run

-}

{-

Typeclasses enable defining generic interfaces that provide a common
feature set over a variety of types.

Typeclasses define a set of functions that have different
implementations depending on the type of data they are given.

"class" below has NOTHING to do with OO "class"

-}

-- provides defaults for each function
-- instance only needs to implement one
class BasicEq a where
    isEqual    :: a -> a -> Bool
    isEqual       x    y = not (isNotEqual x y)
    isNotEqual :: a -> a -> Bool
    isNotEqual    x    y = not (isEqual    x y)

-- types are made instances of a typeclass by implementing
-- the functions necessary for that typeclass
instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

{-
-- Haskell's definition
class Eq a where
    (==), (/=) :: a -> a -> Bool
    -- Minimal complete definition:
    -- (==) or (/=)
    x /= y = not (x == y)
    x == y = not (x /= y)

-- Built-in Typeclasses

-- to convert values to Strings
Show

define a Show instance for your own types
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

-- to convert String to a instance of a type
Read

:type (read "5")
:type (read "5")::Integer
(read "5")::Integer
:type (read "5")::Double
(read "5")::Double

-- define an instance of Read (a parser) for your types
-- Must return the result AND the part of the input that was not
-- parsed so that the system can integrate the parsing of different types
-- together.
-- NOTE: most people use Parsec instead of Read instances.

instance Read Color where
    readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where tryParse [] = [] -- fail
              tryParse ((attempt, result):xs) =
                  if (take (length attempt) value) == attempt
                  -- match, return result and remaining input
                  then [(result, drop (length attempt) value)]
                  else tryParse xs
-}

-- http://www.haskell.org/pipermail/haskell-cafe/2010-July/080920.html

data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray  [JValue]
              deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
    toJValue   :: a       -> JValue
    fromJValue :: JValue  -> Either JSONError a

instance JSON JValue where
    toJValue               = id
    fromJValue             = Right

instance JSON Bool where
    toJValue               = JBool
    fromJValue   (JBool b) = Right b
    fromJValue           _ = Left "not a JSON boolean"

instance JSON Int where
    toJValue               = JNumber . realToFrac
    fromJValue             = doubleToJValue round

instance JSON Integer where
    toJValue               = JNumber . realToFrac
    fromJValue             = doubleToJValue round

instance JSON Double where
    toJValue               = JNumber
    fromJValue             = doubleToJValue id

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left "not a JSON number"

{-
toJValue $ JString "foo"
toJValue $ JBool True
toJValue JNull
toJValue $ JNumber 3.4
[fromJValue (JBool True), Right JNull]
[fromJValue (JBool True), Right True]
[fromJValue (JNumber 2.1), Right 2.1]
[fromJValue (JNumber 2.1), Right (JNumber 2.1)]
[fromJValue "foo", Left "bar"]
fromJValue (JBool False) :: Either JSONError Bool
fromJValue (JBool False) :: Either JSONError JValue
-}

{-
ghci

-- 213/254
cat RecursiveContents.hs
:l RecursiveContents
getRecursiveContents ".."

-- 215/255
cat SimpleFinder.hs
:l SimpleFinder
simpleFind id "."

:m +System.FilePath
:t takeExtension
simpleFind (\p -> takeExtension p == ".hs") "."

-- 217/257
:m +System.Directory
:t doesFileExist
doesFileExist "."
doesDirectoryExist "."
:i getPermissions
:i Permissions
getPermissions "."
getModificationTime "."

-- 218/258
cat BetterPredicate.hs
:l BetterPredicate
betterFind myTest "."
:t betterFind (sizeP `equalP` 1024)
betterFind myTest2 "."
betterFind myTest3 "."
betterFind myTest4 "."

-- controlling traversal - p 226/266
cat ControlledVisit.hs
:l ControlledVisit

traverse id "."
let filterP = foldl (\acc x -> let test = maybe False executable . infoPerms in if test x then x:acc else acc) []
traverse filterP "."

-- another way 230/270
cat FoldDir.hs
:l FoldDir
foldTree atMostThreePictures [] "."
foldTree countDirectories    0  "."

-- exercises - p 232/272
-- TODO

-- exercises - p 234/274
-- TODO
-}

{-
cat PNM.hs
:l PNM

cat Parse.hs
:l Parse.hs
:t parse (identity 1) undefined
parse (identity 1) undefined
parse (identity "foo") undefined
let before = ParseState (L8.pack "foo") 0
let after = modifyOffset before 3
before
after

cat TreeMap.hs
:l TreeMap.hs
let tree = Node (Leaf "foo") (Node (Leaf "x") (Leaf "quux"))
treeLengths tree
treeMap length tree
treeMap (odd . length) tree
 map length ["foo", "quux"]
fmap length ["foo", "quux"]
 map length (Node (Leaf "Livingstone") (Leaf "I presume"))
fmap length (Node (Leaf "Livingstone") (Leaf "I presume"))

:l Parse
parse parseByte L.empty
parse (id <$> parseByte) L.empty
let input = L8.pack "foo"
L.head input
parse parseByte input
parse (id <$> parseByte) input
parse ((chr . fromIntegral) <$> parseByte) input
parse (chr <$> fromIntegral <$> parseByte) input

-- RIGHT HERE
-}

{-
cabal --dry-run install HDBC
cabal           install HDBC
cabal --dry-run install HDBC-postgresql
cabal           install HDBC-postgresql
:module Database.HDBC Database.HDBC.PostgreSQL
:t connectPostgreSQL
conn <- connectPostgreSQL "host=/tmp dbname=hcdb"
:t conn
quickQuery' conn "SELECT * from books" []
quickQuery' conn "SELECT * from authors" []
r <- quickQuery' conn "SELECT table_name FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='public'" []
fromSql (head (head r)) :: String
fromSql $ head $ head r :: String
map (\hr -> fromSql $ head hr :: String) r


disconnect conn
-}
