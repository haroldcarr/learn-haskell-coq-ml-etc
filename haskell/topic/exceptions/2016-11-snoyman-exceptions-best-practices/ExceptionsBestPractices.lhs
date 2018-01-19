> {-# LANGUAGE MultiWayIf #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
>
> module ExceptionsBestPractices where
>
> import qualified Control.Concurrent.MVar as MV
> import qualified Control.Exception       as E
> import qualified Control.Exception.Safe  as S
> import qualified Control.Monad.Except    as Ex
> import qualified Control.Monad.IO.Class  as MIO
> import qualified Data.Text               as T
> import qualified Data.Typeable           as Ty
> import qualified Prelude
> import           Protolude
> import qualified Text.Read               as TR
>
> {-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}

https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell

EXCEPTIONS BEST PRACTICES IN HASKELL.
Michael Snoyman - 07 November, 2016

safe-exceptions library : https://hackage.haskell.org/package/safe-exceptions
- recommend use
- contains a tutorial : https://haskell-lang.org/library/safe-exceptions
  - see file SafeExceptions
- also : https://haskell-lang.org/tutorial/exception-safety
  - see file ExceptionSafetyTutorial

FP Complete Haskell Syllabus : https://www.fpcomplete.com/haskell-syllabus
Haskell training             : https://www.fpcomplete.com/training

THE IO CONTRACT

GHC supports async exceptions
all code that runs in IO can have exceptions of any type which is an instance of Exception
any thread can be killed at any time by an asynchronous exception

------------------------------------------------------------------------------
ExceptT IO ANTI-PATTERN

> newtype E1 = E1 T.Text deriving (Show)
> newtype E2 = E2 Int  deriving (Show)
> instance E.Exception E1
> instance E.Exception E2

> f1 :: Int -> Ex.ExceptT E1 IO Int
> f1 x =
>        -- sync exceptions
>   if | x == 1    -> E.throw (E1 "1")
>      | x == 2    -> E.throw (E2  2)
>      | x == 3    -> Ex.liftIO (E.throwIO (E1 "3"))
>      | x == 4    -> Ex.liftIO (E.throwIO (E2  4 ))
>      | x == 5    -> Ex.liftIO (E.throwIO (E.SomeException (E1 "5")))
>      | x == 6    -> Ex.liftIO (E.throwIO (E.SomeException (E2  6 )))
>        -- async exception (from runtime)
>      | otherwise -> Ex.liftIO (do v <- MV.newEmptyMVar; MV.takeMVar v)
>
> tf1,tf2,tf3,tf4,tf5,tf6,tf7 :: IO (Either E1 Int)
> tf1 = Ex.runExceptT (f1 1)
> tf2 = Ex.runExceptT (f1 2)
> tf3 = Ex.runExceptT (f1 3)
> tf4 = Ex.runExceptT (f1 4)
> tf5 = Ex.runExceptT (f1 5)
> tf6 = Ex.runExceptT (f1 6)
> tf7 = Ex.runExceptT (f1 7)

problems
- non-composable (because of different exception types)
- implies only 'MyException' can be thrown
  - false, since in IO
- have not limited the possibility of exceptions
  - only added an extra avenue by which an exception can be thrown
  - myFunction can now either throwE or liftIO . throwIO.

Almost always wrong to wrap an ExceptT, EitherT, or ErrorT around an IO-based transformer stack.

Separate : bad idea expose concrete transformer stack used in a public-facing API
- better to express in terms of typeclass requirements, using mtl typeclasses

similar ANTI-PATTERN:

myFunction :: String -> ExceptT Text IO Int

NON-solution
- return an Either, throw exception for uncommon errors
- returning Either from ExceptT IO means
  - 3 distinct sources of errors in just one function

OK
- using ExceptT, etc with non-IO base monad (e.g., pure code) is fine

------------------------------------------------------------------------------
MASK-THEM-ALL ANTI-PATTERN

BAD: dealing with async exceptions everywhere is hard : so just mask them all

Best practices
- use bracket pattern when possible
- sse the safe-exceptions package
- for complex flow of control and non-linear scoping of resources, use resourcet package

------------------------------------------------------------------------------
THE GOOD

MONADTHROW

> lu :: Maybe Text
> lu = do
>   foo <- lookup "foo"
>   bar <- lookup "bar"
>   baz <- lookup "baz"
>   f foo bar baz
>  where
>   lookup :: T.Text -> Maybe T.Text
>   lookup "foo" = Just "FOO"
>   lookup "bar" = Just "BAR"
>   lookup "baz" = Just "BAZ"
>   lookup _     = Nothing
>   f a b c = Just (a <> b <> c)

if lu returns Nothing : no idea why : thrown away info (sometimes that is OK)

possible solution

    lookup :: Eq k => k -> [(k, v)] -> Either (KeyNotFound k) v
    f :: SomeVal -> SomeVal -> SomeVal -> Either F'sExceptionType F'sResult

but types do not line up

hacky workaround

> newtype LUandFException = LUandFException T.Text deriving (Show)
> instance E.Exception LUandFException


> lu2 :: Either LUandFException T.Text
> lu2 = do
>   foo <- lookup "foo"
>   bar <- lookup "bar"
>   baz <- lookup "baz"
>   f foo bar baz
>  where
>   lookup :: T.Text -> Either LUandFException T.Text
>   lookup "foo" = Right "FOO"
>   lookup "bar" = Right "BAR"
>   lookup "baz" = Right "BAZ"
>   lookup k     = Left (LUandFException ("lookup failed for: " <> k))
>   f a b c      = if a /= "FOO" then Left (LUandFException "f failure")
>                  else Right (a <> b <> c)

SOLUTION : use MonadThrow

> lu3 :: S.MonadThrow m => m T.Text
> lu3 = do
>   foo <- lookup "foo"
>   bar <- lookup "bar"
>   baz <- lookup "baz"
>   f foo bar baz
>  where
>   lookup :: (S.MonadThrow m) => T.Text -> m T.Text
>   lookup "foo" = return "FOO"
>   lookup "bar" = return "BAR"
>   lookup "baz" = return "BAZ"
>   lookup k     = S.throw (E1 ("lookup failed for: " <> k))
>   f :: (S.MonadThrow m) => T.Text -> T.Text -> T.Text -> m T.Text
>   f a b c      = if a /= "FOO" then S.throw (E2 2)
>                  else return (a <> b <> c)

> lkup :: (S.MonadThrow m) => T.Text -> m T.Text
> lkup "foo" = return "FOO"
> lkup "bar" = return "BAR"
> lkup "baz" = return "BAZ"
> lkup k     = S.throw (E1 ("lookup failed for: " <> k))
>
> ff :: (S.MonadThrow m) => T.Text -> T.Text -> T.Text -> m T.Text
> ff a b c = if a /= "FOO" then S.throw (E2 2)
>            else return (a <> b <> c)
>
> lu4 :: Either SomeException T.Text
> lu4 = do
>   foo <- lkup "foo"
>   bar <- lkup "bar"
>   baz <- lkup "baz"
>   ff foo bar baz

Versus Either signature
- some information lost : type of exception that can be thrown
- gain
  - composability
  - unification with Either (as well as other instances of MonadThrow, like IO)

MonadThrow typeclass is a tradeoff
- but well thought out
- usually the right one
- in line with Haskell's runtime exception system
  - which does not capture the types of exceptions that can be thrown

------------------------------------------------------------------------------
TRANSFORMERS

overly restrictive signature

    foo :: Int -> IO String

generalize (with a usage of liftIO)

    foo :: MonadIO m => Int -> m String

enables function to work with any transformer on top of IO

but

    bar :: FilePath -> (Handle -> IO a) -> IO a

if inner fun needs to live in  transformer on top of IO
- difficult to make it work
- can be done with lifted-base, but hard

better : express in terms of functions from safe-exceptions

    bar :: (MonadIO m, MonadMask m) => FilePath -> (Handle -> m a) -> m a

applies to exception handling and to things like forking threads

also consider using  Acquire type from resourcet

------------------------------------------------------------------------------
CUSTOM EXCEPTION TYPES

BAD

    foo = do
        if x then return y else error "something bad happened"

problem
- string-based error messages (hard to handle higher up in stack)
- using error gives no ordering guarantees
  - creates an exception in a pure value that needs to be evaluated before it's thrown

BEST : define custom exception type

> data SomethingBad = SomethingBad  deriving Typeable
> instance Prelude.Show SomethingBad where show SomethingBad = "something bad happened"
> instance Exception SomethingBad
>
> sb :: S.MonadThrow m => Bool -> a -> m a
> sb x y = if x then return y else S.throwM SomethingBad
> tsbt,tsbf :: S.MonadThrow m => m Char
> tsbt = sb True 'c'
> tsbf = sb False 'c'

now
- easy to catch the SomethingBad at higher level
- better exception ordering

side point : use use displayException method in Exception typeclass instead of Show instance

WHY GHC'S POINT IN THE DESIGN SPACE IS GREAT

IO actions can fail

If every IO action returned a IO (Either UniqueExceptionType a)
- programming model would be tedious
- when a is (), easy to forget to check return type to see if exception occurred

If every IO action returned IO (Either SomeException a)
- not have to deal with different exception types
- could use ErrorT to make code simpler
- but : just reinvented what IO does

Runtime exceptions are more efficient than using ErrorT everywhere.

------------------------------------------------------------------------------
CONCRETE EXAMPLE: READLINE

parsing an input String : meaningful exception if no parse

> data ReadException = ReadException T.Text Ty.TypeRep deriving (Ty.Typeable)
>
> instance Prelude.Show ReadException where
>   show (ReadException s typ) = concat [ "Unable to parse as ", show typ, ": ", show s ]
>
> instance E.Exception ReadException
>
> readM :: (S.MonadThrow m, TR.Read a, Ty.Typeable a) => T.Text -> m a
> readM s = res where
>   res = case readMaybe (T.unpack s) of
>           Just  x -> return x
>           Nothing -> S.throwM $ ReadException s (typeRep res)
>
> treadm :: IO ()
> treadm = do
>   print (readM "hello" :: Either SomeException Int)
>   print (readM "5"     :: Either SomeException Int)
>   print (readM "5"     :: Either SomeException Bool)
>   -- Also works in plain IO
>   res1 <- readM "6"
>   print (res1 :: Int)
>   res2 <- readM "not an int"
>   print (res2 :: Int) -- never gets called

meets criteria
- function generalized to multiple monads and exceptions

now make a readLine function that reads from stdin
- two signature choices

    readLine1 :: (MonadIO m, MonadThrow n, Read a, Typeable a) => m (n a)

    - failure case is common, therefore do not mix it in with same monad handling IO side-effects
    - more explicit about common failure case
    - but gives false that all failures captured by inner value

    readLine2 :: (MonadIO m, MonadThrow m, Read a, Typeable a) => m a

    - combine two different monads (IO side-effects and failure) into one layer
    - failure is uncommon :  user should use tryAny (etc) to extract failures
    - in practice, no point in having MonadIO and MonadThrow
      - can use liftIO to combine them (see below)
    - So signature is

    readLine2 :: (MonadIO m, Read a, Typeable a) => m a.

> readLine1 :: (MIO.MonadIO m, S.MonadThrow n, TR.Read a, Ty.Typeable a) => m (n a)
> readLine1 = fmap readM (Ex.liftIO getLine)
>
> readLine2 :: (MIO.MonadIO m, TR.Read a, Ty.Typeable a) => m a
> readLine2 = liftIO (join readLine1)
>
> trl12 :: IO ()
> trl12 = do
>   putStrLn ("Enter an Int (non-runtime exception)"::T.Text)
>   res1 <- readLine1
>   print (res1 :: Either SomeException Int)
>   putStrLn ("Enter an Int (runtime exception)"::T.Text)
>   res2 <- readLine2
>   print (res2 :: Int)

SEE ALSO

Exceptions in continuation-based monads
http://www.yesodweb.com/blog/2014/05/exceptions-cont-monads
