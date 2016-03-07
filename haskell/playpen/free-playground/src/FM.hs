{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module FM where

import           Control.Exception     (catch)
import           Control.Monad.Free    (Free (Free, Pure), MonadFree, iterM,
                                        liftF)
import           Control.Monad.Free.TH (makeFree)
import           Control.Monad.Loops   (unfoldM)
import           Control.Monad.State
import qualified Control.Monad.Trans.Free as TF hiding (Free, Pure, iterM)
import qualified System.Exit           as SE hiding (ExitSuccess)
import           System.IO             (isEOF)
import           System.IO.Error       (ioeGetErrorString)
import           Test.HUnit            (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util       as U (t)

-- http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html

data TeletypeF x
    = PutStrLn String    x    -- putStrLn    :: String -> IO ()
    | GetLine (String -> x)   -- getLine     ::           IO String
    | ExitSuccess             -- exitSuccess ::           IO a
    deriving Functor
{-
instance Functor TeletypeF where
    fmap f (PutStrLn str x) = PutStrLn str (f x)
    fmap f (GetLine      k) = GetLine      (f . k)
    fmap f  ExitSuccess     = ExitSuccess
-}
type Teletype = Free TeletypeF

-- putStrLn' ::                          String -> Teletype ()
putStrLn'    :: MonadFree TeletypeF m => String -> m        ()
putStrLn' str = liftF $ PutStrLn str ()

-- getLine'  ::      Teletype  String
getLine'     :: Free TeletypeF String
getLine'      = liftF $ GetLine id

--exitSuccess'::     Teletype  r
exitSuccess' :: Free TeletypeF a
exitSuccess'  = liftF ExitSuccess

runImpure :: Teletype r -> IO r
runImpure (Pure               r ) = return r
runImpure (Free (PutStrLn str t)) = putStrLn str >>  runImpure t
runImpure (Free (GetLine  f    )) = getLine      >>= runImpure . f
runImpure (Free  ExitSuccess    ) = SE.exitSuccess

-- runImpure echo

runTrace :: Teletype r -> ([String],[String]) -> ([String],[String])
runTrace (Pure               r )   (is, os) =                (is, os++["Pure"])
runTrace (Free (PutStrLn str t))   (is, os) = runTrace t     (is, os++["PurStrLn/"++str])
runTrace (Free (GetLine  f    ))   ([], os) =                ([], os++["GetLine/[]"])
runTrace (Free (GetLine  f    )) (i:is, os) = runTrace (f i) (is, os++["GetLine/"++i])
runTrace (Free  ExitSuccess    )   (is, os) =                (is, os++["ExitSuccess"])

echo :: Teletype ()
echo = do
    _ <- getLine'
    s1 <- getLine'
    putStrLn' s1
    s2 <- getLine'
    putStrLn' s2
    _ <- getLine'
    putStrLn' s2
    putStrLn' "before exit"
    exitSuccess'
    putStrLn' "after exit"

trte :: [Test]
trte  = U.t "trte"
    [ runTrace echo (["1","2","3"]        ,[])
    , runTrace echo (["1","2","3","4"]    ,[])
    , runTrace echo (["1","2","3","4","5"],[])
    ]
    [ ([],  ["GetLine/1","GetLine/2","PurStrLn/2","GetLine/3","PurStrLn/3","GetLine/[]"])
    , ([],  ["GetLine/1","GetLine/2","PurStrLn/2","GetLine/3","PurStrLn/3","GetLine/4","PurStrLn/3","PurStrLn/before exit","ExitSuccess"])
    ,(["5"],["GetLine/1","GetLine/2","PurStrLn/2","GetLine/3","PurStrLn/3","GetLine/4","PurStrLn/3","PurStrLn/before exit","ExitSuccess"])
    ]

------------------------------------------------------------------------------
-- http://hackage.haskell.org/package/free-4.12.4/docs/examples/Teletype.lhs

-- 'k' stands for next action to execute

type Error = String

data TeletypeEK k
    = Halt                              -- Abort (ignore all following instructions)
    | NL                            k   -- Newline
    | Read        (Char  ->         k)  -- Get a character from the terminal
    | ReadOrEOF { onEOF  ::         k,
                  onChar :: Char -> k } -- GetChar if not end of file
    | ReadOrError (Error ->         k)
                  (Char  ->         k)  -- GetChar with error code
    | PutStrEK String               k   -- Write a message to the terminal
    deriving (Functor)

makeFree ''TeletypeEK

{-
generates

@
 halt        :: (MonadFree TeletypeEK m) => m a
 nL          :: (MonadFree TeletypeEK m) => m ()
 read        :: (MonadFree TeletypeEK m) => m Char
 readOrEOF   :: (MonadFree TeletypeEK m) => m (Maybe Char)
 readOrError :: (MonadFree TeletypeEK m) => m (Either Error Char)
 putStrEK    :: (MonadFree TeletypeEK m) => String -> m ()
@
-}

-- need instance of 'MonadFree TeletypeEK'
-- 'TeletypeEK' is a 'Functor', therefore:

type TeletypeM = Free TeletypeEK

runTeletypeIO :: TeletypeM a -> IO a
runTeletypeIO = iterM run
  where
    run :: TeletypeEK (IO a) -> IO a
    run Halt                   =          SE.exitSuccess
    run (Read               k) =          getChar      >>= k
    run (ReadOrEOF   eof    k) = isEOF >>= \b ->
                                     if b
                                     then eof
                                     else getChar      >>= k
    run (ReadOrError ferror k) =   catch (getChar      >>= k) (ferror . ioeGetErrorString)
    run (NL                 k) =          putChar '\n' >>  k
    run (PutStrEK    str    k) =          putStr  str  >>  k

readLine :: TeletypeM String
readLine = unfoldM $ mfilter (/= '\n') <$> readOrEOF

hello :: TeletypeM ()
hello = do
    putStrEK "Hello! What's your name?"; nL
    name <- readLine
    putStrEK "Nice to meet you, "; putStrEK name; putStrEK "."; nL
    halt

testEK :: IO ()
testEK = runTeletypeIO hello

------------------------------------------------------------------------------
-- http://dlaing.org/cofun/posts/free_and_cofree.html

data AdderF k = Add Int (Bool -> k)
              | Clear            k
              | Total   (Int  -> k)
              deriving (Functor)

-- rather than makeFree ''AdderF
-- generalize in order to work with other effects (the 'm' below)

type Adder    a = Free  AdderF   a
type AdderT m a = TF.FreeT AdderF m a

add :: Monad m => Int -> AdderT m Bool
add x = liftF $ Add x id

clear :: Monad m => AdderT m ()
clear = liftF $ Clear ()

total :: Monad m => AdderT m Int
total = liftF $ Total id

-- findLimit :: Adder Int
-- more general:
findLimit :: Monad m => AdderT m Int
findLimit = do
    -- capture the old count
    t <- total
    -- clear the count
    clear
    -- seek out the limit
    r <- execStateT findLimit' 0
    -- restore the old count
    clear
    _ <- add t
    -- return the result
    return r

findLimit' :: Monad m => StateT Int (TF.FreeT AdderF m) ()
findLimit' = do
    -- add 1 to the total
    r <- lift $ add 1
    -- check for overflow
    when r $ do
        -- if no overflow, add to our state counter ...
        modify (+ 1)
        -- and continue
        findLimit'

------------------------------------------------------------------------------

test :: IO Counts
test =
    runTestTT $ TestList {- $ -} trte
