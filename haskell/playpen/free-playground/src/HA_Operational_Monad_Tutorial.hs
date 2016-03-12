{-# LANGUAGE GADTs #-}

module HA_Operational_Monad_Tutorial where

{-# ANN module "HLint: ignore Use list literal" #-}

-- stack: imperative list of lnstructions : push 5; push 42; pop; pop;

type ProgramL instr    = [instr]
type StackProgramL     = ProgramL StackInstructionL
data StackInstructionL = PushL Int | PopL
emptyL :: ProgramL StackInstructionL
emptyL  = []

exampleL :: ProgramL StackInstructionL
exampleL  = PushL 5 : PushL 42 : PopL : PopL : []

-- representation enables assembling programs from piecse via concatenation (++)

exampleLTwice :: ProgramL StackInstructionL
exampleLTwice  = exampleL ++ exampleL

-- follows monoid laws

-- Interpreter:

type Stack a = [a]

-- operational semantics: inspect the first instruction; change stack; recursively proceed to next instruction
interpretL :: StackProgramL -> Stack Int -> Stack Int
interpretL (PushL a : is) stack = interpretL is (a :  stack)
interpretL (PopL    : is) stack = interpretL is (tail stack)
interpretL            []  stack = stack

{-
LIMITATION: can't get values from stack.

    a <- pop;
    b <- pop;
    push (a+b);

New Representation
-}

data StackInstruction a where
    Pop  :: StackInstruction Int
    Push :: Int -> StackInstruction ()

-- instr is type of instructions
-- a is return type
data Program instr a where
    Then   :: instr a -> (a -> Program instr b) -> Program instr b
    Return ::             a -> Program instr a

type StackProgram a = Program StackInstruction a

{-
derivation of variable binding

take a binding	                          : a <- pop;            rest
turn the arrow to the right	          : pop -> a;            rest
use lambda expr to move it past semicolon : pop     ;      \a -> rest
replace semicolon with constructor `Then` : Pop     `Then` \a -> rest
`Then` binds value returned by pop to a.

`Return` represents empty program.

example now:

example2 = Pop `Then` \a ->
           Pop `Then` \b ->
           Push (a+b) `Then`
           Return

analogous to the "cons"" operation (:) for lists, with addition of
- return type a in instr a
- lambda abstraction
-}
