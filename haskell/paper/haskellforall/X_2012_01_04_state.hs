{-
Created       : 2014 Oct 21 (Tue) 14:51:28 by Harold Carr.
Last Modified : 2014 Oct 21 (Tue) 15:25:03 by Harold Carr.

http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers_04.html
-}

{-
State

Haskell elegantly manages state
got it right.

Purity : functions *never* have side effects.

Haskell does state replacing normal function results "stateful" outputs:
-}

-- actually a newtype, and output flipped
type XState s a = s -> (a, s)

{-
stateful output
- function that takes old state of type s
- returns output of type a
- and *new* updated state of type s

old state     output   new state
    s     -> (   a   ,     s    )

So if stateless function

f :: a -> b

is turned into a stateful function:

f' :: a -> State s b

which is:

f' :: a -> s -> (b, s)

a stateful function
- input
  - of type a
  - and an old state
- output
  - type b
  - and new updated state

input    old state     output   new state
  a   ->     s     -> (   b   ,     s    )

"stateful" functions are still pure functions

get (current state)
- input : current state
- output : input in output slot (with no change)
-}

xget :: XState s s
xget s = (s, s)


-- put (update state)

xput :: s -> XState s ()
xput s = \_ -> ((), s)

-- example:

xadd0 :: Int -> XState Int ()
xadd0 n = do
    x <- xget
    xput (x + n)

xquadruple0 :: XState Int ()
xquadruple0 = do
    x <- xget
    xadd0 x
    y <- xget
    xadd0 y

-- explicit lambda version

xadd1 :: Int -> XState Int ()
xadd1 n s0 = let (x, s1) = xget s0
             in xput (x + n) s1

xquadruple1 :: XState Int ()
xquadruple1 s0 = let (x, s1) = xget s0
                     (_, s2) = xadd1 x s1
                     (y, s3) = xget s2
                 in xadd1 y s3

-- remove boilerplate : manually passing state around above.

xbind :: XState s a -> (a -> XState s b) -> XState s b
xbind m f s = let (a, s') = m s
              in f a s'

-- shorten original code

xadd2 :: Int -> XState Int ()
xadd2 n = xbind xget          (\x ->
          xput (x + n)        )

xquadruple2 :: XState Int ()
xquadruple2 = xbind xget      (\x ->
              xbind (xadd2 x) (\_ ->
              xbind xget      (\y ->
              xadd2 y         )))

-- then back to xadd0 using do

-- do notation not just syntactic sugar for stateful functions, syntactic sugar for anything that's a monad
-- and State s is just a monads

-- (>>=)  :: (Monad m) =>        m a -> (a ->        m b) ->        m b
   xbind  ::              XState s a -> (a -> XState s b) -> XState s b

{-
I/O

Haskell uses state for input and output too.
type IO a = State RealWorld a
-- i.e. type IO a = RealWorld -> (a, RealWorld)

IO differs from State s in that the state is never exposed.
- cannot get/set
- cannot store the state

Can use understanding of State to reason IO.

- getLine :: RealWorld -> (String, RealWorld)
  getLine :: IO String

starts from some RealWorld state, outputs a String, leaving RealWorld in a new state

- string result may may change with each invocation
- getLine may change the outside world


Advantages of Purity

- statefulness in type system
- can distinguish between pure and "impure" functions via type
  - do not have to read documentation nor code

- compiler can use same optimizations for IO code that it uses for pure State s code

- compiler can aggressively optimize pure code because type guarantees no side effects
  - e.g., cache output of function

- abstracts imperative code
  - high-level imperative combinators implemented directly

- State monad based in category theory
  - can reason about it
-}
-- End of file.
