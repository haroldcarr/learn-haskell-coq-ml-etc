{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal" #-}

{-# LANGUAGE GADTs #-}

module Lib where

import           Data.Char     (isDigit, ord)
import           Prelude       hiding (return, (>>=))
import qualified System.Random as SR

{-

Heinrich Apfelmus
Wed, 03 Feb 2010

The Operational Monad Tutorial

monads from viewpoint of operational semantics
makes designing/implementing new monads easy

------------------------------------------------------------------------------
Introduction

s -> (a,s)
- not the only way to implement state monad

main idea:
- view monads as sequence of instructions to be executed by a machine
- so implementing monads is equivalent to writing an interpreter


basically the principles of Chuan-kai Lin’s Unimo paper
- John Hughes used it to derive the state monad

Ryan Ingram’s MonadPrompt package is another recent formulation.
https://hackage.haskell.org/package/MonadPrompt

apfeλmus operational package : https://hackage.haskell.org/package/operational

------------------------------------------------------------------------------
Stack Machine

push 5; push 42; pop;
-}

type ProgramL instr    = [instr]
type StackProgramL     = ProgramL StackInstructionL
data StackInstructionL = PushL Int | PopL

exampleL :: StackProgramL
exampleL = PushL 5 : PushL 42 : PopL : []

{-
the colon (:) takes the role of the semicolon for sequencing instructions

Concatenation and thoughts on the interface

this representation gives a convenient tool for assembling bigger programs from smaller ones
-}

exampleTwiceL :: StackProgramL
exampleTwiceL = exampleL ++ exampleL

emptyL0 :: [a]
emptyL0  = []
{-
concatenation obeys these laws:

 empty ++ is      =  is                  -- left unit
    is ++ empty   =  is                  -- right unit
(is ++ js) ++ ks  =  is ++ (js ++ ks)    -- associativity

equiped with programs and (++) to combine them,
no need to special case single instructions and (:) for sequencing them
-}

replaceL :: Int -> StackProgramL
replaceL a = PopL : PushL a : []

pushL  :: Int -> StackProgramL
pushL i = PushL i : []
popL   :: StackProgramL
popL    = PopL : []
emptyL :: StackProgramL
emptyL  = []
(.++) :: StackProgramL -> StackProgramL -> StackProgramL
(.++) l r = l ++ r

{-
no need to make distinction between single instruction and compound program

Interpreter

interpret : maps program to intended meaning

impl follows operational semantics style
- inspect 1st instruction
- change stack accordingly
- recursively proceed with remaining instructions
-}

type Stack a = [a]

interpretL :: StackProgramL -> (Stack Int -> Stack Int)
interpretL (PushL a : is) stack = interpretL is (a : stack)
interpretL (PopL    : is) stack = interpretL is (tail stack)
interpretL []            stack  = stack
{-

formulation above is not able to inspect values from the stack

cannot write program that pops two values and pushes sum

a <- pop;
b <- pop;
push (a+b);

binding variables not possible with current representation

------------------------------------------------------------------------------
Stack Machine - Monad

Representation

Return types

to interpret pop as a function that returns something, label it with type of value returned

instead of
Pop :: StackInstruction
will be
Pop :: StackInstruction Int

For consistency, give return type to push even though it return anything

Push 42 :: StackInstruction ()
Push    :: Int -> StackInstruction ()
-}

data StackInstruction a where
  Pop  ::        StackInstruction Int
  Push :: Int -> StackInstruction ()

-- annotate programs with their return type

-- instr : type of instructions
-- a     : return type
--data Program instr a where

type StackProgram a = Program StackInstruction a

{-
Binding variables via lambda abstractions

Return represents empty program

example2 = Pop `Then` \a ->
           Pop `Then` \b ->
           Push (a+b) `Then`
           Return

Then :: instr a -> (a -> Program instr b) -> Program instr b

This is analogous to the “cons” operation (:) for lists,
except for the return type a in instr a and the lambda abstraction.

Empty program represented by a constructor

Return :: a -> Program instr a

not “empty” : it returns the given value a

-}

example3 :: StackProgram Int
example3 = Pop `Then` \a -> Pop `Then` \b -> Return (a * b)

data Program instr a where
  Then   :: instr a -> (a -> Program instr b) -> Program instr b
  Return :: a                                 -> Program instr a

-- Interpreter

-- this impl like previous one, except it passes return values to remaining instructions
interpretS :: StackProgram a -> (Stack Int -> a)
interpretS (Push a `Then` is)    stack  = interpretS (is ()) (a:stack)
interpretS (Pop    `Then` is) (b:stack) = interpretS (is b )    stack
interpretS (Pop    `Then`  _) []        = error "empty stack"
interpretS (Return c)           _stack  = c

ie3 :: Int
ie3  = interpretS example3 [7,11]

{-
Concatenation and interface

build large programs by concatenating smaller ones

no need to make between single instruction and compound program
-}

-- takes role of \x -> [x]
-- helps blur line between program and instructions
singleton :: instr a -> Program instr a
singleton i = i `Then` Return

pop  :: StackProgram Int
pop   = singleton Pop
push :: Int -> StackProgram ()
push  = singleton . Push

-- concatenation operator (aka “bind”) that glues two programs together:
-- impl is analogous to (++)
(>>=) :: Program i a -> (a -> Program i b) -> Program i b
(Return a)    >>= js  = js a
(i `Then` is) >>= js  = i `Then` (\a -> is a >>= js)

return :: a -> Program instr a
return = Return

{-
obeys three evident laws

return a >>= is     = is a                        -- left unit
is >>= return       = is                          -- right unit
(is >>= js) >>= ks  = is >>= (\a -> js a >>= ks)  -- associativity

AKA monad laws

Since return values need to be passed,
the laws are slightly different from the list concatenation laws, but essence is the same

“monad laws”
- any data type supporting those two operations and obeying the laws is called a monad


instance Monad (Program instr) where
  (>>=)  = ...
  return = ...

recap

the above stack machine is the state monad : State (Stack Int)

even though it doesn't use s -> (a,s) for threading state

mplemented equivalent of

evalState :: State s -> (s -> a)

main benefit of operational viewpoint and Program instr a type:
- regardless of choice of interpreter or instruction set
  the monad laws for (>>=) and return will hold
- they are independent of those choices
- makes it easier to define/implement new monads

------------------------------------------------------------------------------
Multiple Interpreters

give the same monad multiple interpreters

multiple interpreters is useful for implementing games
- account for both human and computer opponents
  as well as replaying a game from a script
  (motivation of Ryan Ingram's MonadPrompt package)

example: Random Numbers

Random a : denotes random variables taking values in a.

Random is a monad.

two ways to implement this monad
- interpret random variables as a recipe for creating pseudo-random values from a seed

  type Random a = StdGen -> (a,StdGen)

- view them as a probability distribution

  type Probability = Double
  type Random a    = [(a,Probability)]

rather than chosing one way or the other, the operational approach enables both

example represents Random as a language with just one instruction 'uniform'
that randomly selects an element from a list with uniform probability
-}

type Random a = Program RandomInstruction a

data RandomInstruction a where
  Uniform :: [a] -> RandomInstruction a

uniform :: [a] -> Random a
uniform  = singleton . Uniform

-- roll of a die is modeled as
die :: Random Int
die = uniform [1..6]

-- sum of two dice rolls

sum2Dies :: Random Int
sum2Dies  = die >>= \a -> die >>= \b -> return (a+b)

-- interpretation : sampling a random variable by generating pseudo-random values
sample :: Random a -> SR.StdGen -> (a, SR.StdGen)
sample (Return a)             gen = (a,gen)
sample (Uniform xs `Then` is) gen = sample (is $ xs !! k) gen'
    where (k,gen') = SR.randomR (0,length xs-1) gen

-- interpretation : calculating its probability distribution
distribution :: Fractional b => Random a -> [(a, b)]
distribution (Return a)             = [(a, 1)]
distribution (Uniform xs `Then` is) = [(a, p/n) | x <- xs, (a, p) <- distribution (is x)]
 where n = fromIntegral (length xs)

tdieS :: [Int]
tdieS  = t (5::Int) (SR.mkStdGen 97)
 where
  t 0 _  = []
  t n sg = let (r, sg') = sample die sg
            in r : t (n - 1) sg'

tdieD :: [(Int, Double)]
tdieD  = distribution die

tsum2DiesD :: [(Int, Double)]
tsum2DiesD  = distribution sum2Dies

{-
Distribution interpreter has a flaw : it does not tally probabilities of equal outcomes.
Would require an 'Eq a' constraint on the types of return and (>>=),
which is not possible with the current Monad type class.
Workaround: the 'norm' function from the paper on probabilistic functional programming.

------------------------------------------------------------------------------
Monadic Parser Combinators

goal : derive an implementation of Koen Claessen’s ideas from scratch.

Primitives
-}
-- reads next symbol from input stream
symbol :: Parser Char
symbol  = singleton Symbol
-- parser that never succeeds
mzero  :: Parser a
mzero   = singleton MZero
-- runs two parsers in parallel
mplus  :: Parser a -> Parser a -> Parser a
mplus l r = singleton (MPlus l r)

-- mzero and mplus define MonadPlus

-- derived:

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = symbol >>= \c -> if p c then return c else mzero

many :: Parser a -> Parser [a]
many  p = return [] `mplus` many1 p

many1 :: Parser a -> Parser [a]
--many1 _p = undefined --liftM2 (:) p (many p) -- ** liftM2 requires a Haskell monad
many1 p = p >>= \p' -> many p >>= \mp -> return (p' : mp)

digit :: Parser Int
digit  = satisfies isDigit >>= \c -> return (ord c - ord '0')

number :: Parser Int
number  = many1 digit >>= (return . foldl (\x d -> 10*x + d) 0)

tdigit :: [Int]
tdigit  = interpretPI digit "2"

tnumber :: [Int]
tnumber  = interpretPI number "22345"

{-
first implementation

instruction set for parser language are the primitive operations:
-}

data ParserInstruction a where
  Symbol :: ParserInstruction Char
  MZero  :: ParserInstruction a
  MPlus  :: Parser a -> Parser a -> ParserInstruction a

type Parser a = Program ParserInstruction a

interpretPI :: Parser a -> String -> [a]
-- Return at program end will return result if input parses completely.
interpretPI (Return a)            s = [a | null s]
-- reads single character from input stream if available, otherwise fails.
interpretPI (Symbol    `Then` is) s = case s of
  c:cs -> interpretPI (is c) cs
  []   -> []
-- returns empty result
interpretPI (MZero     `Then`  _) _ = []
-- runs two parsers in parallel, collecting their results.
interpretPI (MPlus p q `Then` is) s =
  interpretPI (p >>= is) s ++ interpretPI (q >>= is) s

{-
note

MZero, MPlus cases would look better as (not valid Haskell, because left not constructors):

interpret mzero       = \s -> []
interpret (mplus p q) = \s -> interpret p s ++ interpret q s

can combine the two equations with MonadPlus laws
that specify how mzero and mplus interact with (>>=)

    mzero >>= m = mzero
mplus p q >>= m = mplus (p >>= m) (q >>= m)

  interpret (Mplus p q `Then` is)
=   { definition of concatenation and mplus }
  interpret (mplus p q >>= is)
=   { MonadPlus law }
  interpret (mplus (p >>= is) (q >>= is))
=   { intended meaning }
  \s -> interpret (p >>= is) s ++ interpret (q >>= is) s

given the first step of above derivation,
forget about constructors
instead regard

interpret (mplus p q >>= is) = ...

as “valid” Haskell code

In other words, it is (again) beneficial to not distinguish between single instructions
and compound programs.

Depth-first backtracking impl

above impl has a potential space leak, the case

interpret (MPlus p q `Then` is) s =
    interpret (p >>= is) s ++ interpret (q >>= is) s

string 's' is shared by the recursive calls : has to be held in memory for indeterminate time.

impl will try to parse s with p >>= is first
then backtrack to beginning of s to parse again with q >>= is

string s has to be held in memory as long the second parser has not started yet

Breadth-first

To fix space leak, want breadth-first impl
- do not try alternative parsers in sequence
- instead keeps collection of all possible alternatives and advances them at once

key idea:

  (symbol >>=        is)  `mplus` (symbol >>= js)
=  symbol >>= (\c -> is c `mplus`             js c)

When parsers on both sides are waiting for next input symbol,
group them together and fetch the next symbol ONCE from the input stream.

equation extends to more than two parsers

  (symbol >>= is) `mplus` (symbol >>= js) `mplus` (symbol >>= ks)
=  symbol >>= (\c -> is c `mplus` js c `mplus` ks c)

...

Use this equation as a function definition, mapping LHS to RHS
Cannot do directly because LHS is not one of the four patterns that can be matched.
Using MonadPlus laws, can rewrite any parser into this form, with a function:
-}

expand :: Parser a -> [Parser a]
expand (MPlus p q `Then` is) = expand (p >>= is) ++
                               expand (q >>= is)
expand (MZero     `Then`  _) = []
expand x                     = [x]

{-
idea is that expand turns a parser into a list of summands which can be pattern matched:

  foldr mplus mzero . expand = id

It "expands" parsers matching
   mzero >>= is
and
   mplus p q >>= is
until only summands of the form
   symbol >>= is
and
   return a
remain.

With the parser expressed as a “sum”,
now apply key idea and group all summands of the form
   symbol >>= is

also have to take care of other summands of the form
   return a
-}

interpret :: Parser a -> String -> [a]
interpret  = interpret' . expand
 where
  interpret' :: [Parser a] -> String -> [a]
  interpret' ps []     = [a | Return a <- ps]
  interpret' ps (c:cs) = interpret' [p | (Symbol `Then` is) <- ps, p <- expand (is c)] cs

{-
how to handle each of the summands depends on the input stream:

If still unconsumed input symbols, then only summands of form
  symbol >>= is
will proceed --- the other parsers have ended prematurely.

If input stream empty, then only parsers of form
  return x
have parsed input correctly --- their results are to be returned.

Above equivalent to Koen Claessen’s implementation.

Note: the rewrite calculations/derivations can be visualized as algebra
(ignoring that (>>=) passes variables)  shown as:

Term    : Mathematical operation
return  : 1
(>>=)   : × multiplication
mzero   : 0
mplus   : + addition
symbol  : x indeterminate

key idea corresponds to distributive law

x × a + x × b = x × (a + b)

monad and MonadPlus laws have counterparts in algebra too

------------------------------------------------------------------------------
Conclusion

Further Examples

monads implementations become clearer when approached operationally
e.g.,
- list monad transformer (where the naive m [a] is known not to work)
- Oleg Kiselyov’s LogicT,
- Koen Claessen’s poor man’s concurrency monad
- coroutines like Peter Thiemann’s WASH (has monad for tracking session state in a web server)

operational package includes a few of these examples

------------------------------------------------------------------------------
Connection with the Continuation Monad

the continuation monad transformer

  data Cont m a = Cont { runCont :: forall b. (a -> m b) -> m b }

has been used to implement these advanced monads.

operational and continuation monad are almost the same thing
the continuation monad is the refunctionalization of instructions as functions

\k -> interpret (Instruction `Then` k)

Apfelmus thinks this combination instruction, interpreter and continuation
does not clarify what is going on.

The GADT 'Program' gives a clear notion of what a monad is and what it means to implement one.

G/ADT should be preferred way of presenting/implementing new monads, at least before optimizations.

Drawbacks

Compared to specialized implementations, like s -> (a,s) for the state monad,
the operational approach has drawbacks.

impl of (>>=) above has same quadratic running time problem as (++)
when used in a left-associative fashion.
This can be fixed with a different (fancy) list data type; the operational library implements one.

Second, and this cannot fixed: lose laziness.
The state monad represented as s -> (a,s) can cope with some infinite programs like

   evalState (sequence . repeat . state $ \s -> (s,s+1)) 0

whereas the list of instructions approach cannot handle that handling that,
since only the last Return instruction can return values.

I also think that this loss of laziness also makes value recursion a la MonadFix very difficult.
-}
