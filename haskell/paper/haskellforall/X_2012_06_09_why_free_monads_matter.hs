{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module X_2012_06_09_why_free_monads_matter where
{-
Created       : 2014 Apr 28 (Mon) 16:13:48 by Harold Carr.
Last Modified : 2016 Mar 05 (Sat) 09:11:12 by Harold Carr.
-}

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

import           Control.Exception.Base (throwIO)
import           Control.Monad          (forever, liftM, when)
import           Control.Monad.Free
import           Data.Fix

import           System.IO.Unsafe       (unsafePerformIO)
import           Test.HUnit
import           Test.HUnit.Util        as T (t, tt)

------------------------------------------------------------------------------
{-
decouple a data representation from processing of that data

example ways to process
- compile to executable
- run/interpret
- pretty print
- compress/archive
- error check
-}

-- Example:

-- syntax tree of commands with link to next command
data Toy b next = Output b next -- prints something of type b to console
                | Bell     next -- rings computer's bell
                | Done          -- end of execution
                deriving (Eq, Show)

-- Problem: different type for every command combination

exampleCommand  ::        Toy Char (Toy a next)
exampleCommand   =       Output 'A' Done
exampleCommand2 :: Toy a (Toy Char (Toy b next))
exampleCommand2  = Bell (Output 'A' Done)

{-
Remedy using:

-- named Fix because it is "fixed point of a functor"
:i Fix
newtype   Fix f = Fix {unFix :: f (Fix f)}
instance   Eq (f (Fix f)) =>   Eq (Fix f)
instance  Ord (f (Fix f)) =>  Ord (Fix f)
instance Show (f (Fix f)) => Show (Fix f)

Wraps many Toys into the same data type.
-}

exampleCommandFp1 :: Fix (Toy Char)
exampleCommandFp1  =            Fix (Output 'A' (Fix Done))
exampleCommandFp2 :: Fix (Toy Char)
exampleCommandFp2  = Fix (Bell (Fix (Output 'A' (Fix Done))))

{-
- Problem: requires Done constructor to terminate chain
- 'Fix' does not support subroutines that operate on part of chain

Hack : when subroutine finished but not calling Done,
       throw exception with continuation where catcher resumes
-}

data FixE f a = Fix' (f (FixE f a))
              | Throw a

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix'  x) f = Fix' (fmap (`catch` f) x)
catch (Throw e) f = f e

-- Toy b must be a functor:
instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap _  Done           = Done

-- Now code can be caught and resumed:
data IncompleteException = IncompleteException

-- output 'A'
-- throw IncompleteException
subroutine :: FixE (Toy Char) IncompleteException
subroutine  = Fix' (Output 'A' (Throw IncompleteException))

-- try {subroutine}
-- catch (IncompleteException) {
--     bell
--     done
-- }
program :: FixE (Toy Char) e
program  = subroutine `catch` (\_ -> Fix' (Bell (Fix' Done)))

------------------------------------------------------------------------------
-- Free Monads - Part 1

{-
FixE (above) already exists:

data Free f a = Free (f (Free f a))
              | Pure  a

instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r

e.g., return is Throw, >>= is catch

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure
-}

output x = liftF (Output x ())
bell     = liftF (Bell     ())
done     = liftF  Done

subroutine' :: Free (Toy Char) ()
subroutine' = output 'A'

-- can use `do` notation since using Free MONAD
-- just builds a data type instance (no effects)
program' :: Free (Toy Char) r
program' = do
    subroutine'
    bell
    subroutine'
    bell
    bell
    done

-- program'
-- => Free (Output 'A' (Free (Bell (Free (Output 'A' (Free (Bell (Free (Bell (Free Done))))))))))

-- PRETTY PRINT

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) = "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell     x)) = "bell\n"  ++ showProgram x
showProgram (Free Done)         = "done\n"
showProgram (Pure r)            = "return " ++ show r ++ "\n"

-- this signature/function just to give unit type to `r` for tests
pretty :: (Show a) => Free (Toy a) () -> String
pretty = showProgram

t1 = T.t "t1"
     (pretty program')
     "output 'A'\nbell\noutput 'A'\nbell\nbell\ndone\n"

-- check monad laws

t2 = T.tt "t2"
     [ pretty (output 'A')
     , pretty (return 'A' >>= output)
     , pretty (output 'A' >>= return)
     ]
     "output 'A'\nreturn ()\n"

t3 = T.tt "t3"
     [ pretty ((output 'A' >>  done) >> output 'C')
     , pretty  (output 'A' >> (done  >> output 'C'))
     ]
     "output 'A'\ndone\n"

{-
Note: `Done` swallows all commands after it, whereas `Pure` resumes.
Many cases do not need Done-like constructor in functor.
Some cases may need Done's "abort" semantics.
-}

-- INTERPRETER

ringBell :: IO () -- some obnoxious library would provide this
ringBell = print "BELL"

interpret :: (Show b) => Free (Toy b) r -> IO ()
interpret (Free (Output b x)) = print b  >> interpret x
interpret (Free (Bell     x)) = ringBell >> interpret x
interpret (Free  Done       ) = return ()
interpret (Pure r)            = throwIO (userError "Improper termination")

t4 = T.t "t4"
     (unsafePerformIO (interpret program'))
     ()

------------------------------------------------------------------------------

-- "Concurrency" : interleave two monadic "threads"

-- call in listed order
-- - nesting forces first action to be evaluated before next one
-- - when no actions left, use Return ("done") constructor
-- pass return values between successive monad actions

data Thread m r = Atomic (m (Thread m r)) -- Atomic wraps one indivisible step
                | Return r

-- turn any single monad invocation into an atomic Thread step
atomic :: (Monad m) => m a -> Thread m a
atomic m = Atomic $ liftM Return m

-- make Thread a monad
-- FAKE sequencing
-- keeping atomic steps separate
-- later interleave them with other threads

instance (Monad m) => Monad (Thread m) where
    return = Return
    (Atomic m) >>= f = Atomic (liftM (>>= f) m)
    (Return r) >>= f = f r

thread1 :: Thread IO ()
thread1 = do
    atomic $ do putStrLn ""; putStrLn "Enter text, hit return"
    atomic $ putStrLn "Here is what you entered:"

thread2 :: Thread IO ()
thread2 = do
    str <- atomic $ getLine
    atomic $ putStrLn str

interleave :: (Monad m) => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
    next1 <- atomic m1
    next2 <- atomic m2
    interleave next1 next2

interleave t1 (Return _) = t1
interleave (Return _) t2 = t2

-- run threads after interleaving them
runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r

t5 = T.t "t5"
     (unsafePerformIO (runThread (interleave thread1 thread2)))
     ()

------------------------------------------------------------------------------

-- Free Monads - Part 2

-- NOTE:
-- - Thread is Free
-- - atomic is liftF

-- free monad resembles a list
{-
data Free f r = Free (f (Free f r)) | Pure r
data List a   = Cons  a (List a  )  | Nil
-}

-- think of a free monad as a list of functors
-- Free like Cons, prepending a functor to list
-- Pure like Nil,  representing an empty list (i.e. no functors)

-- what happens if the free monad's functor is itself a value:
type List' a = Free ((,) a) ()

{-
List' a
= Free ((,) a) ()
= Free (a, List' a) | Pure ()
= Free  a (List' a) | Pure ()
-}

-- It becomes an ordinary list
-- A list is just a special case of a free monad.
-- but, Monad instance for [] is not same thing as the Monad instance for List' : Free ((,) a))
-- In List' monad, join behaves like (++), return behaves like []
-- List' monad just a fancy way to concatenate values using do notation

-- liftF like creating singleton list: creates free monad with one functor in it
-- singleton x = Cons x Nil -- i.e. x:[], or [x]
-- liftF x = Free (fmap Pure x)

-- interleave (above) is list merge:

merge (x1:xs1) (x2:xs2) = x1:x2:merge xs1 xs2
merge xs1 [] = xs1
merge [] xs2 = xs2

-- this is actually more similar to:
-- [x1] ++ [x2] ++ interleave xs1 xs2
interleave' :: (Monad m, Functor m, MonadFree m (Thread m)) => Thread m b -> Thread m b -> Thread m b
interleave' (Atomic m1) (Atomic m2) = do
    next1 <- liftF m1
    next2 <- liftF m2
    interleave' next1 next2
interleave' a1 (Return _) = a1
interleave' (Return _) a2 = a2

-- concurrency is merging a lists of actions

-- category theory : free monads and lists: both free objects, lists are free monoids, and free monads are free monads.

------------------------------------------------------------------------------

-- Interpreters - Revisited

-- player-programmable game
-- accept programs from players to run in game
-- do not give complete access to IO monad

data Direction = Left | Right | Forward | Backward | Up | Down
data Image = Image

data Interaction next = Look Direction (Image -> next)
                      | Fire Direction next
                      | ReadLine (String -> next)
                      | WriteLine String (Bool -> next)
                      -- deriving (Functor) -- explict below instead

-- Constructor have fields player fills in
-- Provide functions which the interpreter will supply input to
-- Interaction is contract between programmer and interpreter for a single step

instance Functor Interaction where
    fmap f (Look dir g)    = Look dir (f . g)
    fmap f (Fire dir x)    = Fire dir (f x)
    fmap f (ReadLine g)    = ReadLine (f . g)
    fmap f (WriteLine s g) = WriteLine s (f . g)

-- create a list of actions by using the Free monad
type Program = Free Interaction

-- player can a program, e.g.,:
easyToAnger = Free $ ReadLine $ \s -> case s of
    "No" -> Free $ Fire Forward $ Free $ WriteLine "Take that!" (\_ -> easyToAnger)
    _    -> easyToAnger

data Game r = Game r
{-
interpret' :: Program r -> Game r
interpret' prog = case prog of
    Free (Look dir g) -> do
        img <- collectImage dir
        interpret' (g img)
    Free (Fire dir next) -> do
        sendBullet dir
        interpret' next
    Free (ReadLine g) -> do
        str <- getChatLine
        interpret' (g str)
    Free (WriteLine s g) -> do
        putChatLine s
        interpret' (g True)
    Pure r -> return r

collectImage dir = Game Image
sendBullet dir = Game dir
getChatLine = Game "getChatLine"
putChatLine s = Game s

look :: Direction -> Program Image
look dir = liftF (Look dir id)

fire :: Direction -> Program ()
fire dir = liftF (Fire dir ())

readLine :: Program String
readLine = liftF (ReadLine id)

writeLine :: String -> Program Bool
writeLine s = liftF (WriteLine s id)

-- player can more easily write their program using do:
easyToAnger' :: Program a
easyToAnger' = forever $ do
    str <- readLine
    when (str == "No") $ do
        fire Forward
        _ <- writeLine "Take that!"
        return ()
-}
------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList $ t1 ++ t2 ++ t3 ++ t4 ++ t5

-- End of file.

