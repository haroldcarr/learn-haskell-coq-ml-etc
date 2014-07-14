{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo") with an appropriate solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

module Course.List where

import           Course.Core
import           Course.Optional
import qualified Numeric            as N
import qualified Prelude            as P
import qualified System.Environment as E

import qualified Test.HUnit         as T
import qualified Test.HUnit.Util    as U


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Course.Core(even, id, const)
-- >>> import qualified Prelude as P(fmap, foldr)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil) arbitrary

-- BEGIN Helper functions and data types

-- The custom list type
data List t =
  Nil
  | t :. List t
  deriving (Eq, Ord)

-- Right-associative
infixr 5 :.

instance Show t => Show (List t) where
  show = show . foldRight (:) []

-- The list of integers from zero to infinity.
infinity ::
  List Integer
infinity =
  infinityFrom 0

-- HC : open up so I can use below.
infinityFrom :: Num t => t -> List t
infinityFrom x = x :. infinityFrom (x+1)

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

-- HC: this is a strict foldLeft (via `seq`).
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :. t) = let b' = f b h in b' `seq` foldLeft f b' t

-- END Helper functions and data types

-- | Returns the head of the list or the given default.
--
-- >>> headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- >>> headOr 3 Nil
-- 3
--
-- prop> x `headOr` infinity == 0
--
-- prop> x `headOr` Nil == x
headOr ::
  a
  -> List a
  -> a
headOr = foldRight const
-- HC
-- headOr a Nil = a
-- headOr _ (h:._) = h

tho1 :: [T.Test]
tho1 = U.tt "tho1"
       [ headOr                      (-1)        infinity
       ,          foldRight  const   (-1)        infinity             -- def of headOr
       , const 0 (foldRight (const) ((-1)::Int) (infinityFrom (0+1))) -- def of foldRight non-Nil case
       ]
       0                                                              -- def of const

tho2 :: [T.Test]
tho2 = U.tt "tho2"
       [ headOr          (-1) Nil
       , foldRight const (-1) Nil                              -- def of headOr
       ]
       ((-1)::Int)                                             -- def of foldRight Nil case

-- | The product of the elements of a list.
--
-- >>> product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
product ::
  List Int
  -> Int
product =
  foldRight (*) 1

productC :: List Int -> Int
productC = foldLeft (*) 1

tp1 :: [T.Test]
tp1 = U.tt "tp1"
      [ product         (2:.                 3:.Nil)
      , foldRight (*) 1 (2:.                 3:.Nil)                  -- def of product
      ,           (*)    2 (foldRight (*) 1 (3:.Nil))                 -- def of foldRight non-Nil
      ,           (*)    2           ((*)    3 (foldRight (*) 1 Nil)) -- "
      ,           (*)    2           ((*)    3                1)      -- def of foldRight Nil
      ]
      6                                                               -- def of (*)

tp2 :: [T.Test]
tp2 = U.tt "tp2"
      [ productC (2:.3:.Nil)
      , (*) 1 (2::Int) `seq` foldLeft (*) ((*) 1  2) (3:.Nil)
      ,                      foldLeft (*)      1 (2:. 3:.Nil)
      ,       (2::Int) `seq` foldLeft (*)         2  (3:.Nil)
      ,        2       `seq` ((*) 2 3) `seq` foldLeft (*) ((*) 2 3) Nil
      ,       (2::Int) `seq` 6         `seq` foldLeft (*) 6         Nil
      ,       2 `seq` 6         `seq`              6                  -- http://www.haskell.org/haskellwiki/Seq
      ]
      6

-- | Sum the elements of the list.
--
-- >>> sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- >>> sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- prop> foldLeft (-) (sum x) x == 0
sum ::
  List Int
  -> Int
sum =
  foldRight (+) 0

sumC :: List Int -> Int
sumC = foldLeft (+) 0

-- | Return the length of the list.
--
-- >>> length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- prop> sum (map (const 1) x) == length x
length ::
  List a
  -> Int
length =
  foldLeft (const . succ) 0
-- HC: length = foldRight (\_ acc -> acc + 1) 0

tlc :: [T.Test]
tlc = U.tt "tlc"
      [ length                    (10:.                                          20:.Nil)
      , foldLeft (const . succ) 0 (10:.                                          20:.Nil)
      , (const . succ) 0 10 `seq` foldLeft (const . succ) ((const . succ) 0 10) (20:.Nil)
      , 1                   `seq` foldLeft (const . succ) 1                     (20:.Nil)
      , 1                   `seq` (const . succ) 1 20 `seq` foldLeft (const . succ) ((const . succ) 1 20) Nil
      , 1                   `seq` 2                   `seq` foldLeft (const . succ) 2                     Nil
      , 1                   `seq` 2                   `seq`                         2
      ]
      2

-- | Map the given function on each element of the list.
--
-- >>> map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- prop> headOr x (map (+1) infinity) == 1
--
-- prop> map id x == x
map ::
  (a -> b)
  -> List a
  -> List b
map f =
  foldRight (\x acc -> f x :. acc) Nil

-- | Return elements satisfying the given predicate.
--
-- >>> filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- prop> headOr x (filter (const True) infinity) == 0
--
-- prop> filter (const True) x == x
--
-- prop> filter (const False) x == Nil
filter ::
  (a -> Bool)
  -> List a
  -> List a
filter f =
  foldRight (\a     -> if f a then (a:.)   else id)  Nil -- anon return partially applied cons or id - then applied to acc in foldRight
-- HC:
-- filter f =
--foldRight (\a acc -> if f a then  a:.acc else acc) Nil

-- | Append two lists to a new list.
--
-- >>> (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- prop> headOr x (Nil ++ infinity) == 0
--
-- prop> headOr x (y ++ infinity) == headOr 0 y
--
-- prop> (x ++ y) ++ z == x ++ (y ++ z)
--
-- prop> x ++ Nil == x
(++) ::
  List a
  -> List a
  -> List a
(++) =
  flip (foldRight (:.))
-- HC: (++) xsl xsr = foldRight (:.) xsr xsl

infixr 5 ++

-- | Flatten a list of lists to a list.
--
-- >>> flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
--
-- prop> headOr x (flatten (infinity :. y :. Nil)) == 0
--
-- prop> headOr x (flatten (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> sum (map length x) == length (flatten x)
flatten ::
  List (List a)
  -> List a
flatten =
  foldRight (++) Nil

-- HC: this layout is not completely accurate, but it does give some insight.
tft :: [T.Test]
tft = U.tt "tft"
      [ flatten            ((1 :. Nil) :.                   (6 :. Nil) :. Nil)
      , foldRight (++) Nil ((1 :. Nil) :.                   (6 :. Nil) :. Nil)
      ,           (++)      (1 :. Nil) (foldRight (++) Nil ((6 :. Nil) :. Nil))
      , foldRight (:.)                 (foldRight (++) Nil ((6 :. Nil) :. Nil))                              (1 :. Nil)
      , foldRight (:.)                           ((++)      (6 :. Nil) (foldRight (++) Nil Nil))             (1 :. Nil)
      , foldRight (:.)                 (foldRight (:.)                 (foldRight (++) Nil Nil)  (6 :. Nil)) (1 :. Nil)
      , foldRight (:.)                 (foldRight (:.)                 Nil                       (6 :. Nil)) (1 :. Nil)
      , foldRight (:.)                                                                           (6 :. Nil)  (1 :. Nil)
      ]
      (1:.6:.Nil)

-- | Map a function then flatten to a list.
--
-- >>> flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
-- [1,2,3,2,3,4,3,4,5]
--
-- prop> headOr x (flatMap id (infinity :. y :. Nil)) == 0
--
-- prop> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y
--
-- prop> flatMap id (x :: List (List Int)) == flatten x

-- HC:
-- This walks the initial list only once.
-- But it repeatedly traverses/appends intermediate results.
flatMap ::
  (a -> List b)
  -> List a
  -> List b
flatMap g =
  foldRight (\x acc -> g x ++ acc) Nil

-- HC:
-- This walks the initial list once to map and then the result list once to flatten.
-- When flattening result list it repeatedly traverses/appends intermediate results.
flatMapC :: (a -> List b) -> List a -> List b
flatMapC f = flatten . map f

testFun :: Num t => t -> List t
testFun x = x :. x * 10 :. Nil

tfm :: [T.Test]
tfm = U.tt "tfm"
      [ flatMap                                  testFun               (1 :. 11 :. Nil)
      ,                     foldRight (\x acc -> testFun x ++ acc) Nil (1 :. 11 :. Nil)
      ,       testFun 1 ++ (foldRight (\x acc -> testFun x ++ acc) Nil      (11 :. Nil))
      , (++) (testFun 1)   (foldRight (\x acc -> testFun x ++ acc) Nil      (11 :. Nil))
      , foldRight (:.)     (foldRight (\x acc -> testFun x ++ acc) Nil      (11 :. Nil)) (testFun 1)
      , foldRight (:.)     (foldRight (\x acc -> testFun x ++ acc) Nil      (11 :. Nil)) (1:.10:.Nil)
      -- ...
      ]
      (1:.10:.11:.110:.Nil)

-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- prop> let types = x :: List (List Int) in flatten x == flattenAgain x
flattenAgain ::
  List (List a)
  -> List a
flattenAgain =
  error "todo"

-- | Convert a list of optional values to an optional list of values.
--
-- * If the list contains all `Full` values,
-- then return `Full` list of values.
--
-- * If the list contains one or more `Empty` values,
-- then return `Empty`.
--
-- * The only time `Empty` is returned is
-- when the list contains one or more `Empty` values.
--
-- >>> seqOptional (Full 1 :. Full 10 :. Nil)
-- Full [1,10]
--
-- >>> seqOptional Nil
-- Full []
--
-- >>> seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
-- Empty
--
-- >>> seqOptional (Empty :. map Full infinity)
-- Empty

seqOptional ::
  List (Optional a)
  -> Optional (List a)
seqOptional = seqOptional' Nil
  where
    seqOptional'  _  (Empty   :._) = Empty
    seqOptional' acc           Nil = Full (reverse acc)  -- TODO: avoid reverse
    seqOptional' acc ((Full x):.t) = seqOptional' (x :. acc) t

seqOptionalCannotHandleInfinity :: List (Optional a) -> Optional (List a)
seqOptionalCannotHandleInfinity xs =
  let filtered = filter onlyFull xs
      onlyFull Empty    = False
      onlyFull (Full _) = True
  in if length filtered /= length xs -- cannot handle infinite lists
     then Empty
     else Full $ foldRight (\(Full x) acc -> x :. acc) Nil filtered

seqOptionalC :: List (Optional a) -> Optional (List a)
seqOptionalC = foldRight (twiceOptional (:.)) (Full Nil) -- TODO: understand better

tsq :: [T.Test]
tsq = U.tt "tsq"
      [ seqOptionalC                              (Full 1 :. Full 10 :. Nil)
      , foldRight (twiceOptional (:.)) (Full Nil) (Full 1 :. Full 10 :. Nil)
      ,           (twiceOptional (:.))            (Full 1)    (foldRight (twiceOptional (:.)) (Full Nil) (Full 10 :. Nil))
      , bindOptional            (\aa -> mapOptional ((:.) aa) (foldRight (twiceOptional (:.)) (Full Nil) (Full 10 :. Nil)))  (Full 1)
      -- ...
      ]
      (Full (1:.10:.Nil))

-- | Find the first element in the list matching the predicate.
--
-- >>> find even (1 :. 3 :. 5 :. Nil)
-- Empty
--
-- >>> find even Nil
-- Empty
--
-- >>> find even (1 :. 2 :. 3 :. 5 :. Nil)
-- Full 2
--
-- >>> find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- Full 2
--
-- >>> find (const True) infinity
-- Full 0
find ::
  (a -> Bool)
  -> List a
  -> Optional a
find _ Nil    = Empty
find p (h:.t) = if p h then Full h else find p t -- stop on the first one

findC :: (a -> Bool) -> List a -> Optional a
findC p x =
  case filter p x of                             -- traverse entire list
    Nil -> Empty
    h:._ -> Full h

-- | Determine if the length of the given list is greater than 4.
--
-- >>> lengthGT4 (1 :. 3 :. 5 :. Nil)
-- False
--
-- >>> lengthGT4 Nil
-- False
--
-- >>> lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- True
--
-- >>> lengthGT4 infinity
-- True
lengthGT4 ::
  List a
  -> Bool
lengthGT4 = lengthGT4' 0
  where
    lengthGT4' n     Nil = gt4 n
    lengthGT4' n (_:.xs) = if gt4 n then True else lengthGT4' (n + 1) xs
    gt4 = (>4)

lengthGT4C :: List a -> Bool
lengthGT4C (_:._:._:._:._:._) = True
lengthGT4C _ = False

-- | Reverse a list.
--
-- >>> reverse Nil
-- []
--
-- prop> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)
--
-- prop> let types = x :: Int in reverse (x :. Nil) == x :. Nil
reverse ::
  List a
  -> List a
reverse = reverse' Nil
  where
    reverse' acc Nil    = acc
    reverse' acc (h:.t) = reverse' (h:.acc) t -- this version only creates what is used

reverseC :: List a -> List a
reverseC = foldLeft (flip (:.)) Nil -- this version creates unused (partial) copies of the result

trv :: [T.Test]
trv = U.tt "trv"
      [ reverseC (1:.2:.Nil)
      , (flip (:.)) Nil 1 `seq` (foldLeft (flip (:.)) ((flip (:.)) Nil 1) (2:.Nil))
      ,        (1 :. Nil) `seq` (foldLeft (flip (:.))          (1 :. Nil) (2:.Nil))
      ,        (1 :. Nil) `seq` (flip (:.)) (1 :. Nil) 2 `seq` (foldLeft (flip (:.)) ((flip (:.)) (1 :. Nil) 2) Nil)
      ,        (1 :. Nil) `seq` (2 :. 1 :. Nil)          `seq` (foldLeft (flip (:.)) (2 :. 1 :. Nil)            Nil)
      ,        (1 :. Nil) `seq` (2 :. 1 :. Nil)          `seq`                       (2 :. 1 :. Nil)
      ]
      (2:.1:.Nil)

-- | Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
--
-- >>> let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
-- [0,1,2,3]
--
-- >>> let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
-- [1,2,4,8]
produce ::
  (a -> a)
  -> a
  -> List a
produce f a = a:.produce f (f a)

-- | Do anything other than reverse a list.
-- Is it even possible?
--
-- >>> notReverse Nil
-- []
--
-- prop> let types = x :: List Int in notReverse x ++ notReverse y == notReverse (y ++ x)
--
-- prop> let types = x :: Int in notReverse (x :. Nil) == x :. Nil
notReverse ::
  List a
  -> List a
notReverse l@(_:.Nil) = l  -- unnecessary: this is the same thing as the next line for single item lists
notReverse xs = reverse xs

notReverseC :: List a -> List a
notReverseC = reverse -- impossible

hlist ::
  List a
  -> [a]
hlist =
  foldRight (:) []

listh ::
  [a]
  -> List a
listh =
  P.foldr (:.) Nil

putStr ::
  Chars
  -> IO ()
putStr =
  P.putStr . hlist

putStrLn ::
  Chars
  -> IO ()
putStrLn =
  P.putStrLn . hlist

readFile ::
  Filename
  -> IO Chars
readFile =
  P.fmap listh . P.readFile . hlist

writeFile ::
  Filename
  -> Chars
  -> IO ()
writeFile n s =
  P.writeFile (hlist n) (hlist s)

getLine ::
  IO Chars
getLine =
  P.fmap listh P.getLine

getArgs ::
  IO (List Chars)
getArgs =
  P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf ::
  Eq a =>
  List a
  -> List a
  -> Bool
isPrefixOf Nil _ =
  True
isPrefixOf _  Nil =
  False
isPrefixOf (x:.xs) (y:.ys) =
  x == y && isPrefixOf xs ys

isEmpty ::
  List a
  -> Bool
isEmpty Nil =
  True
isEmpty (_:._) =
  False

span ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
span p x =
  (takeWhile p x, dropWhile p x)

break ::
  (a -> Bool)
  -> List a
  -> (List a, List a)
break p =
  span (not . p)

dropWhile ::
  (a -> Bool)
  -> List a
  -> List a
dropWhile _ Nil =
  Nil
dropWhile p xs@(x:.xs') =
  if p x
    then
      dropWhile p xs'
    else
      xs

takeWhile ::
  (a -> Bool)
  -> List a
  -> List a
takeWhile _ Nil =
  Nil
takeWhile p (x:.xs) =
  if p x
    then
      x :. takeWhile p xs
    else
      Nil

zip ::
  List a
  -> List b
  -> List (a, b)
zip =
  zipWith (,)

zipWith ::
  (a -> b -> c)
  -> List a
  -> List b
  -> List c
zipWith f (a:.as) (b:.bs) =
  f a b :. zipWith f as bs
zipWith _ _  _ =
  Nil

unfoldr ::
  (a -> Optional (b, a))
  -> a
  -> List b
unfoldr f b  =
  case f b of
    Full (a, z) -> a :. unfoldr f z
    Empty -> Nil

lines ::
  Chars
  -> List Chars
lines =
  listh . P.fmap listh . P.lines . hlist

unlines ::
  List Chars
  -> Chars
unlines =
  listh . P.unlines . hlist . map hlist

words ::
  Chars
  -> List Chars
words =
  listh . P.fmap listh . P.words . hlist

unwords ::
  List Chars
  -> Chars
unwords =
  listh . P.unwords . hlist . map hlist

listOptional ::
  (a -> Optional b)
  -> List a
  -> List b
listOptional _ Nil =
  Nil
listOptional f (h:.t) =
  let r = listOptional f t
  in case f h of
       Empty -> r
       Full q -> q :. r

any ::
  (a -> Bool)
  -> List a
  -> Bool
any p =
  foldRight ((||) . p) False

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight ((&&) . p) True

or ::
  List Bool
  -> Bool
or =
  any id

and ::
  List Bool
  -> Bool
and =
  all id

elem ::
  Eq a =>
  a
  -> List a
  -> Bool
elem x =
  any (== x)

notElem ::
  Eq a =>
  a
  -> List a
  -> Bool
notElem x =
  all (/= x)

permutations
  :: List a -> List (List a)
permutations xs0 =
  let perms Nil _ =
        Nil
      perms (t:.ts) is =
        let interleave' _ Nil r =
              (ts, r)
            interleave' f (y:.ys) r =
               let (us,zs) = interleave' (f . (y:.)) ys r
               in  (y:.us, f (t:.y:.us):.zs)
        in foldRight (\xs -> snd . interleave' id xs) (perms ts (t:.is)) (permutations is)
  in xs0 :. perms xs0 Nil

intersectBy ::
  (a -> b -> Bool)
  -> List a
  -> List b
  -> List a
intersectBy e xs ys =
  filter (\x -> any (e x) ys) xs

take ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
take n _  | n <= 0 =
  Nil
take _ Nil =
  Nil
take n (x:.xs) =
  x :. take (n - 1) xs

drop ::
  (Num n, Ord n) =>
  n
  -> List a
  -> List a
drop n xs | n <= 0 =
  xs
drop _ Nil =
  Nil
drop n (_:.xs) =
  drop (n-1) xs

repeat ::
  a
  -> List a
repeat x =
  x :. repeat x

replicate ::
  (Num n, Ord n) =>
  n
  -> a
  -> List a
replicate n x =
  take n (repeat x)

reads ::
  P.Read a =>
  Chars
  -> Optional (a, Chars)
reads s =
  case P.reads (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

read ::
  P.Read a =>
  Chars
  -> Optional a
read =
  mapOptional fst . reads

readHexs ::
  (Eq a, Num a) =>
  Chars
  -> Optional (a, Chars)
readHexs s =
  case N.readHex (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readHex ::
  (Eq a, Num a) =>
  Chars
  -> Optional a
readHex =
  mapOptional fst . readHexs

readFloats ::
  (RealFrac a) =>
  Chars
  -> Optional (a, Chars)
readFloats s =
  case N.readSigned N.readFloat (hlist s) of
    [] -> Empty
    ((a, q):_) -> Full (a, listh q)

readFloat ::
  (RealFrac a) =>
  Chars
  -> Optional a
readFloat =
  mapOptional fst . readFloats

instance IsString (List Char) where
  fromString =
    listh

type Chars =
  List Char

type Filename =
  Chars

strconcat ::
  [Chars]
  -> P.String
strconcat =
  P.concatMap hlist

stringconcat ::
  [P.String]
  -> P.String
stringconcat =
  P.concat

instance P.Monad List where
  (>>=) =
    flip flatMap
  return =
    (:. Nil)

testList :: IO T.Counts
testList =
    T.runTestTT P.$ T.TestList P.$ tho1 P.++ tho2 P.++ tp1 P.++ tp2 P.++ tlc P.++ tfm P.++ tft P.++ tsq P.++ trv

-- End of file.
