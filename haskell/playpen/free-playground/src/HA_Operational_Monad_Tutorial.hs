{-# LANGUAGE GADTs #-}

module HA_Operational_Monad_Tutorial where

import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

{-# ANN module "HLint: ignore Use list literal" #-}

-- http://apfelmus.nfshost.com/articles/operational-monad.html

-- emptyL and Return are end of "list", except Return holds a value
type ProgramL instr    = [instr]
emptyL :: ProgramL StackInstructionL
emptyL  = []
-- instr is type of instructions
-- a is return type
data Program  instr a where
    Then   :: instr a -> (a -> Program instr b) -> Program instr b
    Return ::             a -> Program instr a

type StackProgramL     = ProgramL StackInstructionL
type StackProgram a    = Program  StackInstruction a

data StackInstructionL =
    PopL
  | PushL   Int
data StackInstruction a where
    Pop  ::        StackInstruction Int
    Push :: Int -> StackInstruction ()

-- stack: imperative list of lnstructions
--          push  5  ;           push  42  ;           pop  ;           pop  ;
exampleL :: ProgramL StackInstructionL
exampleL  = PushL 5  :           PushL 42  :           PopL :           PopL :           []
example  :: Program  StackInstruction Int
example   = Push  5 `Then` \_ -> Push  42 `Then` \_ -> Pop `Then` \a -> Pop `Then` \b -> Return (a+b)

type Stack a = [a]

-- L Interpreter:

-- operational semantics: inspect the first instruction; change stack; recursively proceed to next instruction
interpretL :: StackProgramL  -> Stack Int -> Stack Int
interpretL (PushL a  :     is)    stack  = interpretL is     (a : stack)
interpretL (PopL     :     is)    stack  = interpretL is    (tail stack)
interpretL                 []     stack  = stack

-- like `interpretL` except
-- - need to pass the return values like () and b remaining instructions `is`
-- - not interested in final stack, only in the value returned
interpret  :: StackProgram a -> Stack Int -> a
interpret  (Push  a `Then` is)    stack  = interpret (is ()) (a : stack)
interpret  (Pop     `Then` is) (a:stack) = interpret (is a )      stack
interpret  (Return a)          _         = a
interpret  _                   _         = error "fall through pattern match"

til :: [Test]
til = U.t "til"
    (interpretL exampleL [])
    []

ti1 :: [Test]
ti1 = U.t "ti1"
    (interpret  example  [])
    47

{-
L REP LIMITATION: can't get values from stack.

    a <- pop;
    b <- pop;
    push (a+b);

derivation of variable binding

take a binding	                          : a <- pop;            rest
turn the arrow to the right	          : pop -> a;            rest
use lambda expr to move it past semicolon : pop     ;      \a -> rest
replace semicolon with constructor `Then` : Pop     `Then` \a -> rest
`Then` binds value returned by pop to a.

`Return` represents empty program.

example now:

analogous to the "cons"" operation (:) for lists, with addition of
- return type a in instr a
- lambda abstraction
-}

------------------------------------------------------------------------------

test :: IO Counts
test =
    runTestTT $ TestList $ til ++ ti1
