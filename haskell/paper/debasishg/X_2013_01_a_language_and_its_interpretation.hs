{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE PackageImports #-}

module X_2013_01_a_language_and_its_interpretation
where
{-
http://debasishg.blogspot.ca/2013/01/a-language-and-its-interpretation.html

Created       : 2015 Sep 01 (Tue) 10:51:58 by Harold Carr.
Last Modified : 2015 Sep 01 (Tue) 12:12:50 by Harold Carr.
-}

import           Control.Monad.Free

{-
separation of concerns between pure data and its processing

Monads generally don't compose.
Mmonads restricted to a particular form support a sum type that composes.

Wouter Swierstra
Data Types a la carte
http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.101.4131

describes this as

Term f a = Pure a | Impure (f (Term f a))

Monads of pure values OR an impure effect, constructed using f.

When f is a functor, Term f is a monad.

In this case, Term f is free monad (the left adjoint to forgetful functor f).

What makes a monad free: see Edward Kmett's response:
https://plus.google.com/u/0/+DebasishGhosh/posts/g9LASrMjeFS

Free monad is the freeest object possible that's still a monad.

Composition

Free monads compose.  Can build larger abstractions of pure data and but still monad properties.

Free monad is pure. Can put impurities into processing of that monad.

Example: interpreter for Joy-like concatenative language : uses stack for its computation.

Gabriel Gonzalez on use of free monads
http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html
http://www.haskellforall.com/2012/07/free-monad-transformers.html

Similar exercise done for embedding Forth in Haskell
https://github.com/TikhonJelvis/Forth-nonsense/blob/master/Forth.hs
-}

p :: Joy ()
p = do push 5
       push 6
       add
       incr
       add2
       square
       end
{-
> p
Free (Push 5 (Free (Push 6 (Free (Add (Free (Push 1 (Free (Add (Free (Push 1 (Free (Add (Free (Push 1 (Free (Add (Free (Dup (Free (Mult (Free (Dup (Free (Dup (Free (Mult (Free (Mult (Free End))))))))))))))))))))))))))))))

Building the pure data (aka Builder)

All operators take a continuation.
Ignore anything after End.
-}
data JoyOperator cont = Push Int cont
                      | Add      cont
                      | Mult     cont
                      | Dup      cont
                      | End
                      deriving (Show, Functor)
{-

Derives 'Functor'.
Free monads are general way of turning functors into monads.
(Required for implementing forgetful functor adjoint of free monad.)

Knowing something is a free monad helps transform an operation over
the monad (the monad homomorphism) into an operation over the functor
(functor homomorphism).

Define free monad over JoyOperator using Control.Monad.Free:

data Free f a = Pure a | Free (f (Free f a))
-}

-- | The free monad over JoyOperator
type Joy = Free JoyOperator

{-
Joy operators as operations over free monads.

Note: 'liftF' lifts an operator (which is a Functor) into the context of a free monad.

liftF :: Functor f => f a -> Free f a

free moand has forgetful functor as left adjoint.
Unlifting from the monad to the functor via:
retract function ..

retract :: Monad f => Free f a -> f a

retract . liftF = id
-}

-- | Push an integer to the stack
push :: Int -> Joy ()
push n = liftF $ Push n ()

-- |
dup :: Joy ()
dup = liftF $ Dup ()

-- |
end :: Joy ()
end = liftF $ End

-- | Add the top two numbers of the stack and push the sum
add :: Joy ()
add = liftF $ Add ()

-- |
mult :: Joy ()
mult = liftF $ Mult ()

-- can also combine operators
-- Since monads, can use 'do'

-- | adds 1 to a number.
incr :: Joy ()
incr = do {push 1; add}

-- | increments twice
add2 :: Joy ()
add2 = do {incr; incr}

-- | squares a number
square :: Joy ()
square = do {dup; mult}

{-
An Interpreter (aka Visitor)
-}

data JoyError = NotEnoughParamsOnStack | NotEmptyOnEnd | NoEnd deriving Show

-- | interpreter takes free monad as input.
runProgram :: Joy n -> Either JoyError Int
runProgram program = joy [] program
  where joy stack           (Free (Push v cont)) = joy (v : stack) cont
        joy (a : b : stack) (Free (Add cont))    = joy (a + b : stack) cont
        joy (a : b : stack) (Free (Mult cont))   = joy (a * b : stack) cont
        joy (a : stack)     (Free (Dup cont))    = joy (a : a : stack) cont
        joy _               (Free Add {})        = Left NotEnoughParamsOnStack
        joy _               (Free Dup {})        = Left NotEnoughParamsOnStack
        joy []              (Free End)           = Left NotEnoughParamsOnStack
        joy [result]        (Free End)           = Right result
        joy _               (Free End)           = Left NotEmptyOnEnd
        joy _               Pure {}              = Left NoEnd

{-
runProgram p

Improving bind

Janis Voigtlander
Improvement of Computations over Free Monads.
http://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf

Edward Kmett implemented the above.
-}
