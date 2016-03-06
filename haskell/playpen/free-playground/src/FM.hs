{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}

module FM where

import           Control.Monad.Free
import qualified System.Exit        as SE hiding (ExitSuccess)
import           Test.HUnit         (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util    as U (t)

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

putStrLn'    :: String -> Teletype ()
putStrLn' str = liftF $ PutStrLn str ()

getLine'     :: Teletype String
getLine'      = liftF $ GetLine id

exitSuccess' :: Teletype r
exitSuccess'  = liftF ExitSuccess

-- EXAMPLE INSTANCE

echo1 :: Teletype ()
echo1 = do
    str <- getLine'
    putStrLn' str
    exitSuccess'
    putStrLn' "Finished"

runImpure :: Teletype r -> IO r
runImpure (Pure               r ) = return r
runImpure (Free (PutStrLn str t)) = putStrLn str >>  runImpure t
runImpure (Free (GetLine  f    )) = getLine      >>= runImpure . f
runImpure (Free  ExitSuccess    ) = SE.exitSuccess

-- runImpure echo

runPure :: Teletype r -> [String] -> [String]
runPure (Pure               r )    xs  = []
runPure (Free (PutStrLn str t))    xs  = str:runPure t     xs
runPure (Free (GetLine  f    ))    []  = []
runPure (Free (GetLine  f    )) (x:xs) =     runPure (f x) xs
runPure (Free  ExitSuccess    )    xs  = []

trpe1 :: [Test]
trpe1  = U.t "trpe1"
    (runPure echo1 ["hello"])
    ["hello"]

echo2 :: Teletype ()
echo2 = do
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

runTrace :: Teletype r -> ([String],[String]) -> ([String],[String])
runTrace (Pure               r )   (is, os) =                (is, os++["Pure"])
runTrace (Free (PutStrLn str t))   (is, os) = runTrace t     (is, os++["PurStrLn/"++str])
runTrace (Free (GetLine  f    ))   ([], os) =                ([], os++["GetLine/[]"])
runTrace (Free (GetLine  f    )) (i:is, os) = runTrace (f i) (is, os++["GetLine/"++i])
runTrace (Free  ExitSuccess    )   (is, os) =                (is, os++["ExitSuccess"])

trte2 :: [Test]
trte2  = U.t "trte2"
    [ runTrace echo2 (["1","2","3"]        ,[])
    , runTrace echo2 (["1","2","3","4"]    ,[])
    , runTrace echo2 (["1","2","3","4","5"],[])
    ]
    [ ([],  ["GetLine/1","GetLine/2","PurStrLn/2","GetLine/3","PurStrLn/3","GetLine/[]"])
    , ([],  ["GetLine/1","GetLine/2","PurStrLn/2","GetLine/3","PurStrLn/3","GetLine/4","PurStrLn/3","PurStrLn/before exit","ExitSuccess"])
    ,(["5"],["GetLine/1","GetLine/2","PurStrLn/2","GetLine/3","PurStrLn/3","GetLine/4","PurStrLn/3","PurStrLn/before exit","ExitSuccess"])
    ]

test :: IO Counts
test =
    runTestTT $ TestList $ trpe1 ++ trte2
