{-# LANGUAGE OverloadedStrings, PackageImports #-}
{-
:set -XOverloadedStrings
-}

module Wiki_haskell_org_all_about_monads
where

import           Control.Arrow      ((&&&))
import           Control.Monad      (MonadPlus (..), ap, foldM, guard, liftM,
                                     liftM2, mapM, mapM_, msum, sequence,
                                     sequence_, zipWithM, zipWithM_)
import           Control.Monad.Except
import           Control.Monad.Plus (mfromMaybe)
import "mtl"     Control.Monad.State
import           Data.Char          (digitToInt, isAlpha, isDigit, isHexDigit, isSpace)
import           Data.Maybe         (mapMaybe)
import           Data.Text          as T hiding (break, dropWhile, foldM, foldl,
                                          foldr, head, map, tail, words)
import           System.Random      (Random(..), getStdGen, randomR, StdGen)
import qualified Test.HUnit         as T
import qualified Test.HUnit.Util    as U
import           X_02_example       hiding (parent)

{-
Created       : 2015 Aug 15 (Sat) 09:41:08 by Harold Carr.
Last Modified : 2015 Aug 19 (Wed) 17:16:42 by Harold Carr.

https://wiki.haskell.org/All_About_Monads
http://web.archive.org/web/20061211101052/http://www.nomaware.com/monads/html/index.html

------------------------------------------------------------------------------
1.1 What is a monad?

Monads
- sequential computations
- determine how combined computations form a new computation
- frees programmer coding combination manually

1.2 Why should I make the effort to understand monads?

Monads : structuring functional programs
- Modularity
  - computations composed from other computations
  - separate combination strategy from computations
- Flexibility
  - programs more adaptable than programs written without monads
  - monad puts computational strategy in single place
    (instead of distributed in entire program)
- Isolation
  - imperative-style structures isolated from main program.

------------------------------------------------------------------------------
2 Meet the Monads

-- the type of monad m
data m a = ...

-- return is a type constructor that creates monad instances
return :: a -> m a

-- combines a monad instance 'm a' with a computation 'a -> m b'
-- to produce another monad instance 'm b'
(>>=) :: m a -> (a -> m b) -> m b

Container analogy
- type constructor 'm' is container that can hold different values 'a'
- 'm a' is container holding value of type 'a'
- 'return' puts value into monad container
- >>= takes value from monad container, passes it a function
  to produce a monad container containing a new value, possibly of a different type
  - binding function can implement strategy for combining computations in the monad

2.3 An example
-}

maternalGF1 :: Sheep -> Maybe Sheep
maternalGF1 s = case mother s of
                    Nothing -> Nothing
                    Just m  -> father m

momsPaternalGF1 :: Sheep -> Maybe Sheep
momsPaternalGF1 s = case mother s of
                        Nothing -> Nothing
                        Just m  -> case father m of
                                       Nothing -> Nothing
                                       Just gf -> father gf

mom1  = U.t "mom1"  (show (mother breedSheep))           (show (Just "Molly"))
dad1  = U.t "dad1"  (show (father breedSheep))           (show (Nothing::Maybe Sheep))
mgf1  = U.t "mgf1"  (show (maternalGF1 breedSheep))      (show (Just "Roger"))
mpgf1 = U.t "mpgf1" (show (momsPaternalGF1 breedSheep))  (show (Just "Kronos"))

{-
2.4 List is also a monad

List monad enables computations that can return 0, 1, or more values.

(>>=)     :: Monad m => m a -> (a -> m b) -> m b
(=<<)     :: Monad m => (a -> m b) -> m a -> m b
concatMap ::            (a -> [b]) -> [a] -> [b]
-}

listEx = U.t "listEx"
         ([1,2,3] >>= \x -> [x + 1])
          [2,3,4]

{-
2.5 Summary

Maybe monad
- combining computations that may not return values
[] monad
- combining computations that can return 0, 1, or more values

------------------------------------------------------------------------------
3.2 The Monad class

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a

3.3 Example continued

instance Monad Maybe where
    Nothing  >>= f = Nothing
    (Just x) >>= f = f x
    return         = Just

-}

maternalGF2 :: Sheep -> Maybe Sheep
maternalGF2 s = mother s >>= father

dadsMaternalGF2 :: Sheep -> Maybe Sheep
dadsMaternalGF2 s = father s >>= mother >>= mother

maternalGF3 :: Sheep -> [Sheep]
maternalGF3 s = mfromMaybe (mother s) >>= mfromMaybe . father

dadsMaternalGF3 :: Sheep -> [Sheep]
dadsMaternalGF3 s = mfromMaybe (father s) >>= mfromMaybe . mother >>= mfromMaybe . mother

mgf2  = U.t "mgf2"  (show (maternalGF2 breedSheep))           (show $ Just "Roger")
dmgf2 = U.t "dmgf2" (show (dadsMaternalGF2 breedSheep))       (show (Nothing::Maybe Sheep))
mgf3  = U.t "mgf3"  (show (maternalGF3 breedSheep))           (show ["Roger"])
dmgf3 = U.t "dmgf3" (show (dadsMaternalGF3 breedSheep))       (show ([]::[String]))

{-
3.4 Do notation

'do' notation resembles imperative language
- computation built from sequence of computations

------------------------------------------------------------------------------
4 The monad laws

Not enforced by Haskell compiler: programmer must ensure.
Ensure semantics of do-notation consistent.
- (return x) >>= f == f x
  - return is left-identity for >>=
- m >>= return     == m
  - return is right-identity for >>=
- (m >>= f) >>= g  == m >>= (\x -> f x >>= g)
- >>= is associative

4.3 No way out

No way to get values out of monad as defined in Monad class (on purpose).
Specific monads might provide such functions (e.g., 'fromJust' or pattern-matching '(Just x)')

One-way monads
- values enter monad via 'return'
- computations performed within monad via '>>='
- but can't get values out of monad.
  - e.g., IO monad
- enables "side-effects" in monadic operations but prevent them escaping to rest of program

Common pattern
- represent monadic values as functions
- when value of monadic computation required, "run" monad to provide the answer.

4.4 Zero and Plus

MonadPlus

Some monads obey additional laws
- mzero >>= f         == mzero
- m >>= (\x -> mzero) == mzero
- mzero `mplus` m     == m
- m `mplus` mzero     == m
(i.e., mzero/0, mplus/+, >>=/×)

class (Monad m) => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus Maybe where
    mzero             = Nothing
    Nothing `mplus` x = x
    x `mplus` _       = x

Identifies Nothing as the zero value.
Adding two Maybe values gives first value that is not Nothing

[] monad : mzero/empty-list, mplus/++

'mplus' combines two monadic values into single monadic value
-}

parent :: Sheep -> [Sheep]
parent s = mfromMaybe (mother s) `mplus` mfromMaybe (father s)

prnt1 = U.t "prnt1" (show (parent breedSheep))                     (show ["Molly"])
prnt2 = U.t "prnt1" (show (parent (head (parent breedSheep))))     (show ["Holly","Roger"])

{-
------------------------------------------------------------------------------
5 Exercises

./X_02_example.hs

------------------------------------------------------------------------------
6 Monad support in Haskell

6.1.2 The sequencing functions

-- given list of monadic computations
-- executes each one in turn
-- returns list of results
-- If any computation fails, then the whole function fails:
sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])
  where mcons p q = p >>= \x -> q >>= \y -> return (x:y)
-}

seqExM = U.t "seqExM" (sequence [Just 1,  Just 2])
                      (Just [1,2])
seqExL = U.t "seqExL" (sequence [[    1], [    2]])
                           [[1,2]]

{-          mcons
           /     \
          1       mcons
                 /     \
                2       return []

-- same behavior but does not return list of results
-- useful for side-effects
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())
-}

seq_ExM :: IO ()
seq_ExM = sequence_ [print 1, print 2]

{-
            >>
           /  \
    print 1    >>
              /  \
       print 2    return ()
-}

{-
6.1.3 The mapping functions

-- maps monadic computation over list of values
-- returns list of results
mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
mapM  f as = sequence  (map f as)

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence_ (map f as)

Example:

putString :: [Char] -> IO ()
putString s = mapM_ putChar s

Common pattern: mapM used in a do block, similar to map on lists.

-- compare non-monadic and monadic signatures
map  ::            (a ->   b) -> [a] ->   [b]
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-}

mapMExM = U.t "mapMExM" (mapM Just [1,2,3])
                        (Just [1,2,3])

mapM_ExM :: IO ()
mapM_ExM = mapM_ print [1,2,3]

{-
6.2.2 Monadic versions of list functions

foldM : monadic foldl : monadic computations left-to-right

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []     = return a
foldM f a (x:xs) = f a x >>= \y -> foldM f y xs

easier to understand pseudo-Haskell:

foldM f a1 [x1,x2,...,xn] = do a2 <- f a1 x1
                               a3 <- f a2 x2
                               ...
                               f an xn

If right-to-left needed: reverse input before calling foldM.

Example 3:
-}

-- TODO : use this

-- traceFamily :: Sheep -> [ Sheep -> Maybe Sheep ] -> Maybe Sheep
traceFamily :: Monad m => Sheep -> [ Sheep -> m Sheep ] -> m Sheep
traceFamily = foldM getParent
  where getParent s f = f s

fm  = U.t "fm"  (show (traceFamily breedSheep [father, mother]))         (show (Nothing::Maybe Sheep))
mff = U.t "mff" (show (traceFamily breedSheep [mother, father, father])) (show (Just "Kronos"))
mmm = U.t "mmm" (show (traceFamily breedSheep [mother,mother,mother]))   (show (Just "Eve"))

{-
Typical use of foldM is within a do block.
See example4.hs
    program builds dictionary from entries in all files named on the command line

-- like list filter, but inside of a monad.
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []     = return []
filterM p (x:xs) = do b  <- p x
                      ys <- filterM p xs
                      return (if b then (x:ys) else ys)

See example5.hs

-- zipWithM : monadic zipWith function on lists
zipWithM  :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM  f xs ys = sequence  (zipWith f xs ys)

-- discards output
zipWithM_ :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

-}

zipWithMHC = U.t "zipWithMHC"
             (zipWithM  (curry Just)  [1,2,3] "abc")
             (Just [(1,'a'),(2,'b'),(3,'c')])

zipWithM_HC :: IO ()
zipWithM_HC = zipWithM_ (curry print) [1,2,3] ("abc"::String)

{-
6.2.3 Conditional monadic computations

when   :: (Monad m) => Bool -> m () -> m ()
when   p s = if p then s else return ()

unless :: (Monad m) => Bool -> m () -> m ()
unless p s = when (not p) s

6.2.4 ap and the lifting functions

Lifting : converts a non-monadic function to work monadic values.

Use: operating on monad values outside of a do block.
Use: cleaner code in a do block.

liftM  :: (Monad m) => (a -> b)      -> (m a -> m b)
liftM  f = \a -> do
    a' <- a
    return (f a')

liftM2 :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
liftM2 f = \a b ->
    a' <- a
    b' <- b
    return (f a' b')

up to liftM5 defined in Monad module.

example 6: more concise code:
-}

-- converts "Smith, John" into "John Smith"
swapNames :: String -> String
swapNames s = let (ln,fn) = break (==',') s
              in dropWhile isSpace (tail fn) ++ " " ++ ln

getName :: String -> Maybe String
getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  liftM swapNames (lookup name db)

{- Without using the liftM operation, we would have had to do something
   that is less succinct, like this:

getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  tempName <- lookup name db
	          return (swapNames tempName)

Difference even greater when lifting functions with more args.
-}

gn = U.t "gn" [   getName "John",      getName "Mike", getName "Harold"]
              [Just "John Smith",Just "Michael Caine", Nothing         ]

{-
Lifting enables concise higher-order functions.
-}

-- returns list containing result of folding the given binary operator
-- through all combinations of elements of the given lists.
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations fn []     = []
allCombinations fn (l:ls) = foldl (liftM2 fn) l ls

-- e.g., allCombinations (+) [[0,1],[1,2,3]]
--   => [0+1,0+2,0+3,1+1,1+2,1+3], or [1,2,3,2,3,4]
--       allCombinations (*) [[0,1],[1,2],[3,5]] would be
--   => [0*1*3,0*1*5,0*2*3,0*2*5,1*1*3,1*1*5,1*2*3,1*2*5], or [0,0,0,0,3,5,6,10]

ac1 = U.t "ac1" (allCombinations (+) [[0,1],[1,2,3]])
                [1,2,3,2,3,4]
ac2 = U.t "ac2" (allCombinations (*) [[0,1],[1,2],[3,5]])
                [0,0,0,0,3,5,6,10]
ac3 = U.t "ac3" (allCombinations div [[100, 45, 365], [3, 5], [2, 4], [2]])
                [8,4,5,2,3,1,2,1,30,15,18,9]

{-
related function : 'ap' : sometimes more lift.

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = liftM2 ($)

Note:      liftM2 f      x      y
           return f `ap` x `ap` y

and so on for functions of more arguments.

Useful when working with higher-order functions and monads.

Effect of ap depends on specific monad.
-}

apEx1 = U.t "apEx1" ([(*2),(+3)] `ap` [0,1,2])        [0,2,4,3,4,5]
apEx2 = U.t "apEx2" (Just (*2)   `ap` Just 3)         (Just 6)

-- lookup commands
-- fold ap into resulting command list
-- to compute a result
apEx val cmds0 =
    let fns  = [("double" ,    (2*))
               ,("halve"  ,(`div`2))
               ,("square" , \x->x*x)
               ,("negate" ,  negate)
               ,("incr"   ,    (+1))
               ,("decr"   , (+(-1)))
               ]
        cmds = map (`lookup` fns) (words cmds0)
     in foldl (flip ap) (Just val) cmds

apEx3 = U.t "apEx3" (apEx 2 "double square decr negate")
                    (Just (-15))

{-
6.2.5 Functions for use with MonadPlus

Used with monads that have a zero and a plus

- like sum function on lists of integers
msum :: MonadPlus m => [m a] -> m a
msum xs = foldr mplus mzero xs

List monad: msum==concat
Maybe monad: msum==returns the first non-Nothing value from a list
-}

type Variable = String
type Value = String
type EnvironmentStack = [[(Variable,Value)]]

-- leverages lazyness : the map only does first element, then feeds results to msum
--                      next element only looked at if first results in Nothing
lookupVar :: Variable -> EnvironmentStack -> Maybe Value
lookupVar var stack = msum $ map (lookup var) stack

{-
instead of:

lookupVar :: Variable -> EnvironmentStack -> Maybe Value
lookupVar var []     = Nothing
lookupVar var (e:es) = let val = lookup var e
                       in maybe (lookupVar var es) Just val
-}

ms1 = U.t "ms1" (lookupVar "depth" [[("name","test"),("depth","2")]
                                   ,[("depth","1")]])
                (Just "2")
ms2 = U.t "ms2" (lookupVar "width" [[("name","test"),("depth","2")]
                                   ,[("depth","1")]])
                Nothing
ms3 = U.t "ms3" (lookupVar "var2"  [[("var1","value1"),("var2","value2*")]
                                   ,[("var2","value2"),("var3","value3")]])
                (Just "value2*")

{-
guard :: MonadPlus m => Bool -> m ()
guard p = if p then return () else mzero

Recall MonadPlus law : mzero >>= f == mzero.
Placing guard in monad sequence will force any execution in which guard is False to be mzero.
Like guard predicates in list comprehensions cause values that fail to become [].
-}

data Record = Rec {nameR::String, age::Int} deriving (Eq, Show)
type DB = [Record]

-- return records less than specified age.
-- Uses guard to eliminate records at or over limit.
-- Real code would be clearer using a filter except guard more useful when filter is complex.
-- mapMaybe : eliminates Nothing/mzero from results
-- guard returning mzero in causes do to skip 'return r'
getYoungerThan :: Int -> DB -> [Record]
getYoungerThan limit = mapMaybe (\r -> do { guard (age r < limit); return r })

gytDB = [Rec "Marge" 37, Rec "Homer" 38, Rec "Bart" 11, Rec "Lisa" 8, Rec "Maggie" 2]

gyt1 = U.t "gyt1" (getYoungerThan  3 gytDB)
                  [Rec {nameR = "Maggie", age = 2}]
gyt2 = U.t "gyt2" (getYoungerThan 38 gytDB)
                  [Rec {nameR = "Marge", age = 37}
                  ,Rec {nameR = "Bart", age = 11}
                  ,Rec {nameR = "Lisa", age = 8}
                  ,Rec {nameR = "Maggie", age = 2}
                  ]

{-
------------------------------------------------------------------------------
7 Introduction

Monad
- Computation
- Combination strategy (>>= behavoir)

Identity
- N/A — Used with monad transformers
- bound function applied to input value

Maybe
- computations with 0 or 1 result
- Nothing input gives Nothing output
- Just x input uses x as input to bound function

Error
- computations that can fail (e.g., "throw" exceptions)
- binding passes failure info on without executing bound function
  or uses successful values as input to bound function

[] (List)
- computations that can return multiple possible results
- Maps bound function across input list, concatenates resulting lists

IO
- Computations which perform I/O
- Sequential execution of I/O actions in the order of binding.

State
- Computations which maintain state
- bound function applied to input value
  produces state transition function that is applied to input state

Reader
- Computations that read from shared environment
- bound function applied to input using the same environment

Writer
- Computations that write data in addition to computing values
- Written data maintained separately from values.
  bound function applied to input
  anything it writes is appended to write data stream

Cont
- Computations that can be interrupted and restarted
- bound function inserted into continuation chain

------------------------------------------------------------------------------
8 The Identity monad

Computation: Simple function application
Binding: bound function applied to input.: Identity x >>= f == Identity (f x)
Use:  Monads derived from monad transformers applied to Identity monad.
Zero/plus: None.
Example: Identity a

8.2 Motivation

Does not embody a computation.
Purpose is its role in monad transformers:
- a monad transformer applied to Identity yields a non-transformer version of that monad.

8.3 Definition

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    return a           = Identity a
    (Identity x) >>= f = f x

'runIdentity' follows style of monad definition that represents monad values as computations:
- a monadic computation built up using monadic operators
- value of computation extracted using run*

8.4 Example

-- derive the State monad using the StateT monad transformer
type State s a = StateT s Identity a

------------------------------------------------------------------------------
9 The Maybe monad

Computation: may return Nothing
Binding: Nothing bypasses bound function, Just given as input to bound function.
Use: sequences of computations that may return Nothing (e.g., database queries, dictionary lookups)
Zero/plus: Nothing/zero. Plus returns first non-Nothing value or Nothing if both Nothing.
Example: Maybe a

9.2 Motivation

combining a chain of Maybe computations: end chain early if any produces Nothing as output.

9.3 Definition

data Maybe a = Nothing | Just a

instance Monad Maybe where
    return         = Just
    fail           = Nothing
    Nothing  >>= f = Nothing
    (Just x) >>= f = f x

instance MonadPlus Maybe where
    mzero             = Nothing
    Nothing `mplus` x = x
    x `mplus` _       = x

9.4 Example

Combining dictionary lookups.

Given dictionaries : full name     -> email address
                     nicknames     -> email address
                     email address -> email preferences
find email prefs given full or nick name.
-}

type EmailAddr = String
data MailPref = HTML | Plain deriving (Eq, Show)

data MailSystem = MS { fullNameDB::[(String,EmailAddr)],
                       nickNameDB::[(String,EmailAddr)],
		       prefsDB   ::[(EmailAddr,MailPref)] }

data UserInfo = User { msName::String,
                       nick::String,
		       email::EmailAddr,
		       prefs::MailPref }

makeMailSystem :: [UserInfo] -> MailSystem
makeMailSystem users = let fullLst = map (msName &&& email) users
                           nickLst = map (nick   &&& email) users
			   prefLst = map (email  &&& prefs) users
		       in MS fullLst nickLst prefLst

-- skips next steps if any returns Nothing
getMailPrefs :: MailSystem -> String -> Maybe MailPref
getMailPrefs sys name = do
    addr <- lookup name (fullNameDB sys) `mplus` lookup name (nickNameDB sys)
    lookup addr (prefsDB sys)

mailSystem = makeMailSystem
                 [ User "Bill Gates"      "billy"       "billg@microsoft.com" HTML
                 , User "Bill Clinton"    "slick willy" "bill@hope.ar.us"     Plain
                 , User "Michael Jackson" "jacko"       "mj@wonderland.org"   HTML
                 ]

mail1 = U.t "mail1" (getMailPrefs mailSystem "billy")
                    (Just HTML)
mail2 = U.t "mail2" (getMailPrefs mailSystem "Bill Gates")
                    (Just HTML)
mail3 = U.t "mail3" (getMailPrefs mailSystem "Bill Clinton")
                    (Just Plain)
mail4 = U.t "mail4" (getMailPrefs mailSystem "foo")
                    Nothing

{-
------------------------------------------------------------------------------
10 The Control.Monad.Except monad

10.1 Overview

Computation: computations which may fail or throw exceptions
Binding: Failure values bypass bound function. Success values are inputs to bound function.
Use: Sequences of functions that may fail.
Zero/plus: Zero/empty error. Plus executes 2nd arg if first fails.
Example type: Either String a

10.2 Motivation

Except monad (aka Exception monad) combining computations that may
throw exceptions by bypassing bound functions from point of exception
to point handled.

MonadError parameterized error type of error and monad type constructor.
Common: Either String as monad type constructor. In this case (and others)
the resulting monad is already defined as an instance of the MonadError class.

Can also define custom error type and/or use monad type constructor other
than Either String or Either IOError. These cases need instance definitions of
Error and/or MonadError classes.

10.3 Definition

uses multi-parameter type classes and funDeps

newtype ExceptT e m a :: * -> (* -> *) -> * -> *

type Except e = ExceptT e Identity

class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

throwError used in monadic computation to begin exception processing
catchError provides a handler function to handle previous errors and return to normal execution.

Common idiom:

do { action1; action2; action3 } `catchError` handler

Handler and do-block must have same return type.

10.4 Example

Custom Error with ErrorMonad's throwError and catchError.

Parse hexadecimal numbers.
Throws exception on invalid character.
Error records location of error.
-}

data ParseError = Err {location::Int, reason::String}

-- Monad type constructor
-- - failure : Left ParseError
-- - success : Right a
type ParseMonad = Either ParseError

-- idx is current location in parse
parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c idx = if isHexDigit c then
                        return (toInteger (digitToInt c))
		      else
		        throwError (Err idx ("Invalid character '" ++ [c] ++ "'"))

-- idx is current location in parse
parseHex :: String -> ParseMonad Integer
parseHex s = parseHex' s 0 1
  where parseHex' []      val _   = return val
        parseHex' (c:cs)  val idx = do d <- parseHexDigit c idx
	                               parseHex' cs ((val * 16) + d) (idx + 1)

toString :: Integer -> ParseMonad String
toString n = return $ show n

-- convert hex String rep to decimal String rep
convert :: String -> String
convert s = let (Right str) = do { n <- parseHex s; toString n } `catchError` printError
            in str
  where printError e = return $ "At index " ++ show (location e) ++ ":" ++ reason e

p1 = U.t "p1" (convert "FF")      "255"
p2 = U.t "p2" (convert "FFFF")    "65535"
p3 = U.t "p3" (convert "FFFFxF")  "At index 5:Invalid character 'x'"

{-
------------------------------------------------------------------------------
11 The List monad

Computation: return 0, 1, or more results.
Binding: bound function applied to all inputs, resulting lists concatenated
Use: Sequences of non-deterministic operations. Parsing ambiguous grammars is a common example.
Zero/plus: []/zero ++/plus
Example: [a]

11.2 Motivation

Useful when computations must deal with ambiguity.
Enables all possibilities to be explored until ambiguity resolved.

11.3 Definition

instance Monad [] where
    m >>= f  = concatMap f m
    return x = [x]
    fail s   = []

instance MonadPlus [] where
    mzero = []
    mplus = (++)

11.4 Example

Parsing ambiguous grammars.

Parse data into hex, decimal or alphanumeric words.
Hex overlaps decimal and alphanumeric: ambiguous grammar.
- "dead" is both a valid hex value and a word
- "10" is both a decimal value of 10 and a hex value of 16
-}

data Parsed = Digit Integer | Hex Integer | Word String deriving (Eq, Show)

parseCommon :: (Char -> Bool) -> Char -> Parsed -> [Parsed]
parseCommon test c ret = if test c then return ret else mzero

-- try to add char to parsed rep of hex digit
parseHexDigt :: Parsed -> Char -> [Parsed]
parseHexDigt (Hex   n) c = parseCommon isHexDigit c (Hex ((n*16) + toInteger (digitToInt c)))
parseHexDigt _         _ = mzero

-- try to add char to parsed rep of decimal digit
parseDigit   :: Parsed -> Char -> [Parsed]
parseDigit   (Digit n) c = parseCommon isDigit    c (Digit ((n*10) + toInteger (digitToInt c)))
parseDigit   _         _ = mzero

-- try to add a char to parsed rep of word
parseWord    :: Parsed -> Char -> [Parsed]
parseWord    (Word  s) c = parseCommon isAlpha    c (Word (s ++ [c]))
parseWord _            _ = mzero

-- tries to parse input as hex, decimal and word
-- result is list of possible parses
parse :: Parsed -> Char -> [Parsed]
parse p c = parseHexDigt p c `mplus` parseDigit p c `mplus` parseWord p c

-- parse an entire String and return list of possible parsed values
parseArg :: String -> [Parsed]
parseArg s = do init <- return (Hex 0) `mplus` return (Digit 0) `mplus` return (Word "")
                foldM parse init s

sr1 = U.t "sr1" (parseArg "dead") [Hex 57005,Word "dead"]
sr2 = U.t "sr2" (parseArg   "10") [Hex 16,Digit 10]
sr3 = U.t "sr3" (parseArg  "foo") [Word "foo"]

{-
------------------------------------------------------------------------------
12 The IO monad

Computation: perform I/O
Binding: I/O actions executed in order in which they are bound.
         Failures throw I/O errors which can be caught and handled.
Use: I/O
Zero/plus: None.
Example: IO a

12.2 Motivation

I/O not pure.  IO monad confines I/O computations

12.3 Definition

Definition platform-specific.
No data constructors are exported and no functions to remove data from IO monad.
IO monad is a one-way monad: essential to ensuring safety.
Isolates side-effects and non-referentially transparent actions within
 imperative-style computations of the IO monad.

Monadic values usually known as computations.
Balues in IO monad are called I/O actions.

Functions exported from IO module do not perform I/O.
They return I/O actions that describe an I/O operation to be performed.

I/O actions combined within IO monad (in a purely functional manner)
to create more complex I/O actions, resulting in final I/O action that is main value of program.

IO type constructor is a Monad class and MonadError class.

instance Monad IO where
    return a = ...   -- function from a -> IO a
    m >>= k  = ...   -- executes the I/O action m and binds the value to k's input
    fail s   = ioError (userError s)

data IOError = ...

ioError :: IOError -> IO a
ioError = ...

userError :: String -> IOError
userError = ...

catch :: IO a -> (IOError -> IO a) -> IO a
catch = ...

try :: IO a -> IO (Either IOError a)
try f = catch (do r <- f
                  return (Right r))
              (return . Left)

instance Error IOError where
  ...

instance MonadError IO where
    throwError = ioError
    catchError = catch

IO exports 'try' that executes I/O action
- returns Right on success
- Left IOError if I/O error caught

12.4 Example

Partial impl of "tr"
-}

-- translate char in set1 to corresponding char in set2
translate :: String -> String -> Char -> Char
translate []     _      c = c
translate (x:xs) []     c = if x == c then ' ' else translate xs []  c
translate (x:xs) [y]    c = if x == c then  y  else translate xs [y] c
translate (x:xs) (y:ys) c = if x == c then  y  else translate xs ys  c

-- translate an entire string           this
translateString :: String -> String -> String -> String
translateString set1 set2 = map (translate set1 set2)

usage :: IOError -> IO ()
usage e = do putStrLn "Usage: ex14 set1 set2"
             putStrLn "Translates characters in set1 on stdin to the corresponding"
             putStrLn "characters from set2 and writes the translation to stdout."

-- translates stdin to stdout based on commandline arguments
-- main2
-- abcdefghijklmnopqrstuvwxyz
-- ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- thegeekstuff
-- => THEGEEKSTUFF
main2 :: IO ()
main2 = (do putStr "Enter set1: "
            set1 <- getLine
            putStr "Enter set2: "
            set2 <- getLine
            putStr "Enter contents: "
            contents <- getLine
            putStrLn $ translateString set1 set2 contents)
        `catchError` usage

{-
------------------------------------------------------------------------------
13 The State monad

Computation: maintain state.
Binding: threads state parameter through sequence of bound functions
         so that same state value is never used twice, giving the illusion of in-place update.
Use: sequences of operations that require a shared state.
Zero/plus: None.
Example:  State st a

13.2 Motivation

Pure language cannot update in place: violates referential transparency.
Instead, simulate state.
-}

data RandomResults = RR Int Char Int deriving Show

-- Without state, thread by hand:
makeRandomValue :: StdGen -> (RandomResults, StdGen)
makeRandomValue g = let (n,g1) = randomR (1  ,1000) g
                        (c,g2) = randomR ('a', 'z') g1
                        (m,g3) = randomR (-n ,   n) g2
                    in (RR n c m, g3)

{-
State monad puts threading of state inside (>>=).

13.3 Definition

Uses multi-parameter type classes and funDeps.

- State monad values are transition funs from initial state to (value,newState) pair.
- State s a
  - value of type a
  - inside the State monad with state of type s.
newtype State s a = State { runState :: (s -> (a,s)) }

- return : creates state transition fun that sets value but leaves state unchanged.
- bind   : creates state transition fun that applies right arg to val
           and new state from its left-hand argument.
instance Monad (State s) where
    return a        = State $ \s -> (a,s)
    (State x) >>= f = State $ \s -> let (v,s') = x s in runState (f v) s'

instance MonadState (State s) s where
--  get  :: m s
    get   = State $ \s -> (s,s)    -- retrieves state by copying it as the value
--  put  :: s -> m ()
    put s = State $ \_ -> ((),s)   -- sets state of monad and does not yield a value
--  gets                           -- retrieves function of the state

13.4 Example

thread random generator state through multiple calls to generation function.
-}

-- bounds random value
getRan :: (Random a) => (a,a) -> State StdGen a
getRan bounds = do g      <- get
                   (x,g') <- return $ randomR bounds g
                   put g'
                   return x

-- State monad with StdGen as state, no manually threading of random generator states
makeRandomValueST :: StdGen -> (RandomResults, StdGen)
makeRandomValueST = runState (do n <- getRan (1  ,1000)
                                 c <- getRan ('a', 'z')
                                 m <- getRan (-n ,   n)
                                 return (RR n c m))

-- print a random value of RandomResults, showing the two implementations
-- are equivalent
rg = do
    g <- getStdGen
    print $ fst $ makeRandomValue   g
    print $ fst $ makeRandomValueST g

{-
getRan :: (Random a) => (a, a) -> State StdGen a
getRan bounds = get                       >>= \g       ->
                return $ randomR bounds g >>= \(x, g') ->
                put g'                    >>
                return x

makeRandomValueST :: StdGen -> (RandomResults, StdGen)
makeRandomValueST = runState
                      (getRan (1, 1000)   >>= \n ->
                       getRan ('a', 'z')  >>= \c ->
                       getRan (- n, n)    >>= \m ->
                       return (RR n c m))

rg = getStdGen                            >>= \g ->
     print $ fst $ makeRandomValue   g    >>
     print $ fst $ makeRandomValueST g
-}
{-
------------------------------------------------------------------------------
14 The Reader monad

-}

------------------------------------------------------------------------------

testing =
    T.runTestTT $ T.TestList $
        mom1 ++ dad1 ++ mgf1 ++ mpgf1 ++ listEx ++ mgf2 ++ dmgf2 ++ mgf3 ++ dmgf3 ++ prnt1 ++ prnt2 ++
        seqExM ++ seqExL ++ mapMExM ++ fm ++ mff ++ mff ++ zipWithMHC ++ gn ++ ac1 ++ ac2 ++ ac3 ++
        apEx1 ++ apEx2 ++ apEx3 ++ ms1 ++ ms2 ++ ms3 ++ gyt1 ++ gyt2 ++
        mail1 ++ mail2 ++ mail3 ++ mail4 ++ p1 ++ p2 ++ p3 ++ sr1 ++ sr2 ++ sr3

test :: IO ()
test = do
    testing
    let dolly = breedSheep
    seq_ExM
    mapM_ExM
    _ <- zipWithM_HC
    rg
    return ()

-- End of file.

