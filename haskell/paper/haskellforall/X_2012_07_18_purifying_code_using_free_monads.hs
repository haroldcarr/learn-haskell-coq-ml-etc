{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}

module X_2012_07_18_purifying_code_using_free_monads where

import           Control.Monad.Free
import qualified System.Exit        as SE hiding (ExitSuccess)
import           Test.QuickCheck    as QC

{-
Created       : 2015 Sep 02 (Wed) 11:56:37 by Harold Carr.
Last Modified : 2016 Mar 06 (Sun) 08:57:31 by Harold Carr.

http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html

FREE MONADS ENABLE DECOMPOSING AN IMPURE PROGRAM INTO A PURE
REPRESENTATION OF ITS BEHAVIOR (and an impure interpreter).

Purify code using Free monads.  Factor out side-effects via Free monads.

Purity enables:
- equational reasoning
- QuickCheck

Impure program:

main = do
    x <- getLine
    putStrLn x
    exitSuccess
    putStrLn "Finished"
-}

-- REPRESENTATION

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

-- IMPURE INTERPRETER OF REPRESENTATION

run :: Teletype r -> IO r
run (Pure               r ) = return r
run (Free (PutStrLn str t)) = putStrLn str >>  run t
run (Free (GetLine  f    )) = getLine      >>= run . f
run (Free  ExitSuccess    ) = SE.exitSuccess

-- EXAMPLE INSTANCE

echo :: Teletype ()
echo = do
    str <- getLine'
    putStrLn' str
    exitSuccess'
    putStrLn' "Finished"

-- run echo

{-
"PURIFYING THE CODE" : distill logic into pure code, impure part in interpreter.

ADVANTAGE: Proofs

Prove last line never executes:

main = do
    x <- getLine
    putStrLn x
    exitSuccess
    putStrLn "Finished" <-- NEVER EXECUTES

Depends on impure implementation of 'exitSuccess'.  Types don't help.

Can prove that any command after exitSuccess' never executes in purified version.

Prove:  exitSuccess' >> m = exitSuccess'

                  exitSuccess' >>        m -- exitSuccess' = liftF ExitSuccess
=           liftF ExitSuccess  >>        m -- m >> m' = m >>= \_ -> m'
=           liftF ExitSuccess  >>= \_ -> m -- liftF f = Free (fmap Pure f)
= Free (fmap Pure ExitSuccess) >>= \_ -> m -- fmap f ExitSuccess = ExitSuccess
= Free            ExitSuccess  >>= \_ -> m -- Free m >>= f = Free (fmap (>>= f) m)
= Free (fmap (>>= \_ -> m) ExitSuccess)    -- fmap f ExitSuccess = ExitSuccess
= Free            ExitSuccess              -- fmap f ExitSuccess = ExitSuccess
= Free (fmap Pure ExitSuccess)             -- liftF f = Free (fmap Pure f)
=           liftF ExitSuccess              -- exitSuccess' = liftF ExitSuccess
=                 exitSuccess'

Last steps equality reversed : worked backwards from fun def to defined expr.

Proof true for any interpreter of this free monad.

Therefore can add  GHC rewrite rule:

{-# RULES "exit" forall m. exitSuccess' >> m = exitSuccess' #-}

Reasoning

Can't prove program outputs string received as input because putStrLn is impure.
Can't prove free monad with above interpreter either since interpreter uses putStrLn.

Can prove using pure interpreter: runPure echo = take 1
-}

-- PURE INTERPRETER OF REPRESENTATION

runPure :: Teletype r -> [String] -> [String]
runPure (Pure               r )    xs  = []
runPure (Free (PutStrLn str t))    xs  = str:runPure t     xs
runPure (Free (GetLine  f    ))    []  = []
runPure (Free (GetLine  f    )) (x:xs) =     runPure (f x) xs
runPure (Free  ExitSuccess    )    xs  = []

{-
TODO : PROOF


Corner case: the user might not enter any input.

Interpreter then returns empty list, just like take.

Equational reasoning caught corner case.

Once impure code factored into Free monad and proved, its acts like a
trusted kernel.  Only need to maintain interpreter from that point
forward.

Can't prove entire program correct. But can prove everything except interpreter correct.

Reduced interpreter to absolute minimal attack surface.

Testing

Can't prove anything about code in IO monad.

Tests for impure code don't scale.

Can exercise pure code via QuickCheck.

Test 'runPure echo = take 1'
-}

test :: IO ()
test = QC.quickCheck (\xs -> runPure echo xs == take 1 xs)

{-}
+++ OK, passed 100 tests.

conclusion: equational reasoning on pure code
- prove soundness,
- reason about behavior
- test

Use Free monad to achieve more purity / less impurity.

-------------------------
April 23, 2014:

`free` library has Template Haskell function to autogenerate wrapped functions (e.g., putStrLn')

http://hackage.haskell.org/package/free-4.7.1/docs/Control-Monad-Free-TH.html



-------------------------
Unknown December 6, 2015:

How to represent a function of the type (e.g., System.TimeIt.timeItT):

    `IO a -> IO (Double, a)`

as a TeletypeF constructor

Gabriel Gonzalez December 6, 2015:

use

https://hackage.haskell.org/package/operational

encoding of free monads

-------------------------
Michael Roger January 13, 2014:

Is the "free monad" the key to adding purity/semantics to IO?

You are splitting IO into to two monads:
- pure
- impure monad.
Enables swapping out impure impl with pure, maintaining some semantic guarantees.

It would be more useful to to use a non-free monad
- that enables type-system encode *even more* semantics about behaviors of the IO functions

Reason `runPure echo === take ` shows
- `run echo` (impure) *might* be ismorphic/adjoint/equivalent to `take 1`
- but no guarantee
- because list-management code is inside `runPure`, not in the common (free) monad.
- the free monad and `runPure` provides a test case that `run` "isn't definitely wrong"

If list-management code moved from `runPure` to the pure monad
- no longer a free monad, carries additional semantics
  - says `putStrLn` consumes content from `getLine` (and any intermediate pure transformations),
  - then you have IO "laws"
- still can't statically *guarantee* that impure IO follows those laws
- but can encode them and can write specific test cases to check

Gabriel Gonzalez January 18, 2014:

The free monad is not the only way to add a useful semantics to `IO`.
The approach of moving some shared logic up into the free monad and making it non-free is better.

I use the free monad in example because it is extreme
- once free monad is understood, then typically understand how non-free derivatives work
-}
