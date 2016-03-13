{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module HA_Operational_Monad_Tutorial where

import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

{-# ANN module "HLint: ignore Use list literal" #-}
{-# ANN module "HLint: ignore Use >=>"          #-}

-- http://apfelmus.nfshost.com/articles/operational-monad.html

-- emptyL and Return are "end of list", except Return holds a value
type ProgramL instr    = [instr]
emptyL :: ProgramL StackInstructionL
emptyL  = []
-- instr : type of instructions
-- a     : return type
data Program  instr a where
    (:::)  :: instr a -> (a -> Program instr b) -> Program instr b
    Return ::             a -> Program instr a
deriving instance Functor (Program instr)

type StackProgramL     = ProgramL StackInstructionL
type StackProgram a    = Program  StackInstruction a

data StackInstructionL =
    PopL
  | PushL   Int
data StackInstruction a where
    Pop  ::        StackInstruction Int
    Push :: Int -> StackInstruction ()

-- L REP LIMITATION: can't get values from stack.

-- stack: imperative list of lnstructions
--          push  5 ;         push  42 ;         pop  ;         pop  ;
exampleL :: ProgramL StackInstructionL
exampleL  = PushL 5 :         PushL 42 :         PopL :         PopL :         []
example  :: Program  StackInstruction Int
example   = Push  5 ::: \_ -> Push  42 ::: \_ -> Pop  ::: \a -> Pop  ::: \b -> Return (a+b)

-- L Interpreter:

type Stack a = [a]

-- operational semantics: inspect first instruction; change stack; go to next instruction
interpretL :: StackProgramL  -> Stack Int -> Stack Int
interpretL (PushL a :   is)    stack  = interpretL is     (a : stack)
interpretL (PopL    :   is)    stack  = interpretL is    (tail stack)
interpretL              []     stack  = stack

-- like `interpretL` except
-- - need to pass the return values like () and b remaining instructions `is`
-- - not interested in final stack, only in the value returned
interpret  :: StackProgram a -> Stack Int -> a
interpret  (Push  a ::: is)    stack  = interpret (is ()) (a : stack)
interpret  (Pop     ::: is) (a:stack) = interpret (is a )      stack
interpret  (Return a)          _         = a
interpret  _                   _         = error "interpret: fall through pattern match"

til :: [Test]
til = U.t "til"
    (interpretL exampleL [])
    []

ti1 :: [Test]
ti1 = U.t "ti1"
    (interpret  example  [])
    47

-- "lift"

singletonL  :: instr -> [instr]
singletonL i = [i]

singleton  :: instr a -> Program instr a
singleton i = i ::: Return

pop  :: StackProgram Int
pop   = singleton Pop

push :: Int -> StackProgram ()
push  = singleton . Push

-- concatenat program sequences (respects monoid laws)

bindL :: StackProgramL -> StackProgramL -> StackProgramL
bindL  = (++)

instance Applicative (Program instr) where
    pure  = undefined
    (<*>) = undefined

instance Monad (Program instr) where
           -- (>>=)  :: Program i a -> (a -> Program i b) -> Program i b
    (Return a) >>= js = js a
    (i ::: is) >>= js = i ::: (\a -> is a >>= js)
    return = Return

tetL :: [Test]
tetL = U.t "tetL"
    (interpretL (exampleL `bindL` exampleL) [])
    []
tet :: [Test]
tet = U.t "tet"
    (interpret (example >> example) [])
    47
tet2 :: [Test]
tet2 = U.t "tet2"
    (interpret (example >>= \ab -> pop >>= \c -> return (ab*c)) [10])
    470

-- more stack combinators

replace :: Int -> Program StackInstruction ()
replace a     = pop >> push a

popN :: (Eq a, Num a) => a -> StackProgram Int
popN 1 = pop
popN n = pop >> popN (n-1)

------------------------------------------------------------------------------

test :: IO Counts
test =
    runTestTT $ TestList $ til ++ ti1 ++ tetL ++ tet ++ tet2
