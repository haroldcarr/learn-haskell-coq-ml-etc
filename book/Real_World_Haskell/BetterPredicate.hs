import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)

-- import System.Time (ClockTime(..)) -- deprecated
import Data.Time.Clock

import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- the function we wrote earlier
import RecursiveContents (getRecursiveContents)

-- synonym for a pure function of four arguments
type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime       -- last modified
               -> Bool

-- Sequencing 186/225
-- do blocks are shortcut notations for joining actions together.
-- (>>) the first action is performed (and result thrown away), and then the second action is performed.
--      Result is second's result.:  putStrLn "line 1" >> putStrLn "line 2"
-- (>>=) Runs first action, passes its result to second action.
--       Result is second's.  : getLine >>= putStrLn

-- filterM enables us to use pure "p" (predicate) in I/O monad

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    -- check pred is an IO-capable wrapper of pure pred p
    -- it does the IO and then lets p make the decision
    where check name = do
              perms    <- getPermissions name
              size     <- getFileSize name
              modified <- getModificationTime name
              return (p name perms size modified)

-- returns (Maybe <filesize) or Nothing (if not file or on exception)
-- handle is "catch" : given a function to call on exception; and function to run "in catch"
-- bracket is "finally"
--   arg1: first action - acquires resource
--   arg2: last "finally" action
--   arg3: action to perform in-between first and last
-- http://stackoverflow.com/questions/7878065/get-size-of-file-in-haskell
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler $
    bracket (openFile path ReadMode) (hClose) (\h -> do
        size <- hFileSize h
        return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing

{- book version - doesn't work with latest Haskell
getFileSize path = handle (\_ -> return Nothing) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)
-}

-- 221/261

-- .hs files over 1K
myTest path _ (Just size) _ =
    takeExtension path == ".hs" && size > 1024
myTest _ _ _ _ = False

-- DSL to replace myTest

-- InfoP is similar to "Predicate" type above,
-- but takes a type parameter for the result.
-- The "Predicate" type above can be replaced wth "InfoP Bool" (but not done).
type InfoP a =  FilePath      -- path to directory entry
             -> Permissions   -- permissions
             -> Maybe Integer -- file size (Nothing if not file)
             -> UTCTime       -- last modified
             -> a

-- access slots in InfoP
pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

-- This constructs a predicate (the anon function it returns)
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

-- curried version
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

-- avoiding boilerplate with lifting p. 223/263
-- rather than write more functions like "equalP"
-- write code that will lift them.

-- lift: take a function and transform into another that operations in different context
-- Putting "q" as first arg allows concise definitions of lifted functions (e.g., "greaterP" below)
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)

-- gluing predicates together p. 224/264

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP  = liftP2 (||)

-- write liftP in terms of liftP2 (here liftP2's pattern is used, not the definition)
constP :: a -> InfoP a
constP k _ _ _ _ = k
liftP' q f k w x y z = f w x y z `q` constP k w x y z

-- myTest (see above) written with combinators (above) p. 225/265

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".hs") `andP`
          (sizeP `greaterP` 1024)

-- infix aliases for use in predicates

(==?) = equalP
(&&?) = andP
(>?)  = greaterP

-- parenthesis required since no precedence
myTest3 = (liftPath takeExtension ==? ".hs") &&? (sizeP >? 1024)

-- define precedence and remove parenthesis
infix  4 ==?
infixr 3 &&?
infix  4 >?

myTest4 = liftPath takeExtension ==? ".hs" &&? sizeP >? 1024

