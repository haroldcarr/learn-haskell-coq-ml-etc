{-
Created       : 2014 Apr 29 (Tue) 15:48:28 by Harold Carr.
Last Modified : 2016 Mar 14 (Mon) 20:30:47 by Harold Carr.
-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FM_2013_09_andres_loeh_monads_for_free where

import           Control.Monad   (ap)
import           Test.HUnit
import           Test.HUnit.Util as T

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Use const"         #-}
{-# ANN module "HLint: ignore Use list literal"  #-}

-- https://skillsmatter.com/skillscasts/4430-monads-for-free
-- http://www.andres-loeh.de/Free.pdf

------------------------------------------------------------------------------

-- most problems benefit from looking at them as a programming language problem: turn them into a (E)DSL

-- deep embedding: represent everything as data
-- example: use data to represent programs

data Expr = Lit Int
          | Add Expr Expr

-- 1 + (3 + 4)
ex = Add (Lit 1) (Add (Lit 3) (Lit 4))

-- then can do different things with data representation

-- interpret

eval :: Expr -> Int
eval (Lit n)    = n
eval (Add e1 e2) = eval e1 + eval e2

t1 = T.t "t1"
     (eval ex)
     8

-- print

text :: Expr -> String
text (Lit n) = show n
text (Add e1 e2) = "(" ++ text e1 ++ " + " ++ text e2 ++ ")"

t2 = T.t "t2"
     (text ex)
     "(1 + (3 + 4))"

------------------------------------------------------------------------------

-- what if want to do something similar for an imperative use-case?

-- example: interaction/conversation

example_0 = do
    say0 "Hello"
    say0 "Who are you?"
    name <- ask0
    say0 ("Nice to meet you, " ++ name ++ ".")

-- direct embedding in IO

say0 = putStrLn
ask0 = getLine

-- but stuck with only IO
-- instead define a data representation and do deep embedding

-- solution using GADT
-- - GADT: enables restricting the result type of constructors

data Interaction1 :: * -> * where
    Say1    :: String -> Interaction1 ()
    Ask1    :: Interaction1 String
    Return1 :: a -> Interaction1 a
    Bind1   :: Interaction1 a -> (a -> Interaction1 b) -> Interaction1 b

instance Monad Interaction1 where
    return = Return1
    (>>=)  = Bind1

instance Applicative Interaction1 where
    pure  = return
    (<*>) = ap

instance Functor Interaction1 where
    -- f::a->b; msg::String; Say1 msg :: Interaction String;  return : Interaction1 b
    fmap _ (Say1    _) = undefined
    fmap _ Ask1        = undefined
    fmap _ (Return1 _) = undefined
    fmap _ (Bind1 _ _) = undefined

say1 = Say1
ask1 = Ask1

example_1 = do
    say1 "Hello"
    say1 "Who are you?"
    name <- ask1
    say1 ("Nice to meet you, " ++ name ++ ".")

-- a program is then an instance of the Interaction1 data type

-- interpretation as IO

run1 :: Interaction1 a -> IO a
run1 (Say1 msg)  = putStrLn msg
run1 Ask1        = getLine
run1 (Return1 a) = return a
run1 (Bind1 m f) = do x <- run1 m; run1 (f x)

-- run1 example_1

-- but Interaction1 does NOT satisfy monad laws
-- depending how Interaction1 is used, that might be OK
-- but means every interpreter of Interaction1 must guarantee monad laws

------------------------------------------------------------------------------

-- instead guarantee that monad laws hold by construction

-- need Return
-- do not want Bind in full generality
-- left arg to Bind is atomic step : Say or Ask
-- so partially apply Bind:

data Interaction2 :: * -> * where
    Say2    :: String -> Interaction2 ()
    Ask2    :: Interaction2 String
    Return2 :: a -> Interaction2 a
    Bind2   :: Interaction2 a -> (a -> Interaction2 b) -> Interaction2 b

say2     :: String -> (() -> Interaction2 b) -> Interaction2 b
say2 msg  = Bind2 (Say2 msg)
ask2     :: (String -> Interaction2 b) -> Interaction2 b
ask2      = Bind2 Ask2

------------------------------------------------------------------------------

-- since say2/ask2 are only atomics then just use them as constructors rather than Say2/Ask2
-- do not need Bind because it has been fused into Say3 and Ask3

data Interaction3 :: * -> * where
    Say3    :: String -> (() -> Interaction3 b) -> Interaction3 b
    Ask3    :: (String -> Interaction3 b) -> Interaction3 b
    Return3 :: a -> Interaction3 a

-- Interaction3 is a monad

instance Monad Interaction3 where
    return = Return3
--  (>>=) :: Interaction3 a -> (a -> Interaction3 b) -> Interaction3 b
    -- 21'23"
    -- say is given something to say and function that always takes unit and returns how to continue
    -- walk all the way down the steps, find the return at the end, and plug (>>= f) to the end.
    -- It looks in the data representation for the Return3, then substitutes what's in the Return3 with what we bind it to.
    -- A form of substitution or grafting.
    Say3     msg k >>= f = Say3 msg ((>>= f) . k)
    Ask3         k >>= f = Ask3     ((>>= f) . k)
    -- left identity law satisfied by construction
    Return3  x     >>= f =                f       x

instance Applicative Interaction3 where
    pure  = return
    (<*>) = ap

instance Functor Interaction3 where
    -- f::a->b; msg::String; Say1 msg :: Interaction String;  return : Interaction1 b
    fmap _ (Say3  _ _) = undefined
    fmap _ (Ask3    _) = undefined
    fmap _ (Return3 _) = undefined

say3     :: String -> Interaction3 ()
say3 msg  = Say3 msg Return3 -- single say step then return results
ask3     :: Interaction3 String
ask3      = Ask3 Return3     -- single ask step then return results

-- interpreter

run3 :: Interaction3 a -> IO a
run3 (Say3     msg k) = putStrLn msg >>= run3 . k
run3 (Ask3         k) = getLine      >>= run3 . k
run3 (Return3  x    ) = return x

example_3 = do
    say3 "Hello"
    say3 "Who are you?"
    name <- ask3
    say3 ("Nice to meet you, " ++ name ++ ".")

-- run3 example_3

-- we now have a data representation of a monadic computation

-- another interpreter (simulator) 23'40"
-- use-case: test effectful computations with pure functions

--                             input       output
simulate3 :: Interaction3 a -> [String] -> [String]
simulate3 (Say3     msg k)    is  = msg : simulate3 (k ()) is
simulate3 (Ask3         k) (i:is) =       simulate3 (k i ) is
simulate3 (Return3  _    )     _  = []
simulate3                _     _  = error "simulate3: fall through pattern"

t3 = T.t "t3"
     (simulate3 example_3 ["hc"])
     ["Hello","Who are you?","Nice to meet you, hc."]

example_3' = do
    say3 "o"
    name <- ask3
    say3 ("oo:" ++ name)

t3' = T.tt "t3'"
     [ simulate3 example_3'                                                                                                              ["in"]
     , simulate3 ( say3 "o"          >>= \_ ->  ask3          >>= \name ->  say3 ("oo:" ++ name))                                        ["in"]
     , simulate3 ((Say3 "o" Return3) >>= \_ -> (Ask3 Return3) >>= \name -> (Say3 ("oo:" ++ name) Return3))                               ["in"]
     , simulate3  (Say3 "o"        ((>>= \_ -> (Ask3 Return3) >>= \name -> (Say3 ("oo:" ++ name) Return3)) . Return3))                   ["in"]
     , simulate3  (Say3 "o"        ((>>= \_ -> (Ask3        ((>>= \name -> (Say3 ("oo:" ++ name) Return3)) . Return3))) . Return3))      ["in"]
     ,      "o" : simulate3       (((>>= \_ -> (Ask3        ((>>= \name -> (Say3 ("oo:" ++ name) Return3)) . Return3))) . Return3) ())   ["in"]
     ,      "o" : simulate3       (((>>= \_ -> (Ask3        ((>>= \name -> (Say3 ("oo:" ++ name) Return3)) . Return3)))  (Return3  ()))) ["in"]
     ,      "o" : simulate3                    (Ask3        ((>>= \name -> (Say3 ("oo:" ++ name) Return3)) . Return3))                   ["in"]
     ,      "o" :                  simulate3               (((>>= \name -> (Say3 ("oo:" ++ name) Return3)) . Return3) "in")              []
     ,      "o" :                  simulate3                ((>>= \name -> (Say3 ("oo:" ++ name) Return3))  (Return3  "in"))             []
     ,      "o" :                  simulate3                               (Say3 ("oo:" ++ "in") Return3)                                []
     ,      "o" :                                                                ("oo:" ++ "in") : simulate3 (Return3 ())                []
     ,      "o" :                                                                ("oo:" ++ "in") :                                       []
     ]
     ["o","oo:in"]

------------------------------------------------------------------------------

-- Note: Interaction3 does not need GADT.  Can be described with ADT (since target types unconstrained):

data Interaction4 a =
    Say4    String (() -> Interaction3 a)
  | Ask4    (String -> Interaction3 a)
  | Return4 a

-- but will use GADT syntax in rest of examples anyway (because he likes GADT syntax)

------------------------------------------------------------------------------
-- generalize 27'30"

-- split the data type into

--  general
data Interaction5 :: * -> * where
    Return5 :: a -> Interaction5 a
    Wrap5   :: InteractionOp5 a -> Interaction5 a

-- domain-specific
data InteractionOp5 :: * -> * where
    Say5    :: String -> (() -> Interaction5 b) -> InteractionOp5 b
    Ask5    :: (String -> Interaction5 b) -> InteractionOp5 b

-- next step 28'38"

-- InteractionOp5 is parameterized over type b
-- but b just appears as arg to Interaction5

-- almost self-contained except depends on domain-specific InteractionOp6
data Interaction6 :: * -> * where
    Return6 :: a -> Interaction6 a
    Wrap6   :: InteractionOp6 (Interaction6 a) -> Interaction6 a

-- now this is self-contained - does not depend on anything else
data InteractionOp6 :: * -> * where
    Say6    :: String -> (() -> r) -> InteractionOp6 r
    Ask6    :: (String -> r) -> InteractionOp6 r

------------------------------------------------------------------------------

-- 30'11"

-- make InteractionOp a parameter of Interaction type
-- and rename it to Free, since it has nothing to do with InteractionOp anymore

-- completely generic
data Free7 :: (* -> *) -> * -> * where
    Return7 :: a -> Free7 f a
    Wrap7   :: f (Free7 f a) -> Free7 f a

-- same as InteractionOp6
data InteractionOp7 :: * -> * where
    Say7    :: String -> (() -> r) -> InteractionOp7 r
    Ask7    :: (String -> r) -> InteractionOp7 r

type Interaction7 = Free7 InteractionOp7

------------------------------------------------------------------------------

-- 31'25"

-- Free f is a monad whenever f is a functor

instance Functor f => Monad (Free7 f) where
--  return :: a -> Free7 f a
    return = Return7
--  (>>=) :: Free7 f a -> (a -> Free7 f b) -> Free7 f b
    Return7 x >>= f = f x
    Wrap7   c >>= f = Wrap7 (fmap (>>= f) c)

instance Functor f => Applicative (Free7 f) where
    pure  = return
    (<*>) = ap

instance (Functor f) => Functor (Free7 f) where
  fmap f (Return7 x) = Return7 (f x)
  fmap f (Wrap7  xs) = Wrap7 (fmap (fmap f) xs)

------------------------------------------------------------------------------
-- 33'42" is InteractionOp a Functor?

instance Functor InteractionOp7 where
    fmap f (Say7 msg k) = Say7 msg (f . k)
    fmap f (Ask7     k) = Ask7     (f . k)

-- now implement the interface

say7     :: String -> Interaction7 ()
say7 msg  = Wrap7 (Say7 msg Return7)
ask7     :: Interaction7 String
ask7      = Wrap7 (Ask7 Return7)

example_7 = do
    say7 "o"
    name <- ask7
    say7 ("oo:" ++ name)

--                             input       output
simulate7 :: Interaction7 a -> [String] -> [String]
simulate7 (Wrap7 (Say7     msg k))    is  = msg : simulate7 (k ()) is
simulate7 (Wrap7 (Ask7         k)) (i:is) =       simulate7 (k i ) is
simulate7        (Return7  _    )      _  = []
simulate7                       _      _  = error "simulate7: fall through pattern"

t7 = T.tt "t7"
 [ simulate7        example_7                                                                                                                                     ["in"]
 , simulate7        (say7 "o"          >>= \_ ->        ask7          >>= \name ->        say7 ("oo:" ++ name))                                                   ["in"]
 , simulate7 (Wrap7 (Say7 "o" Return7) >>= \_ -> Wrap7 (Ask7 Return7) >>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7))                                          ["in"]
 , simulate7 (Wrap7 (fmap             (>>= \_ -> Wrap7 (Ask7 Return7) >>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) (Say7 "o" Return7)))                     ["in"]
 , simulate7 (Wrap7 (fmap             (>>= \_ -> Wrap7 (fmap         (>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) (Ask7 Return7))) (Say7 "o" Return7)))    ["in"]
 , simulate7 (Wrap7 (Say7 "o"        ((>>= \_ -> Wrap7 (fmap         (>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) (Ask7 Return7))) .         Return7)))    ["in"]
 , simulate7 (Wrap7 (Say7 "o"        ((>>= \_ -> Wrap7 (Ask7        ((>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) .     Return7))) .         Return7)))    ["in"]
 , "o" :     simulate7              (((>>= \_ -> Wrap7 (Ask7        ((>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) .     Return7))) .         Return7) ())  ["in"]
 , "o" :     simulate7               ((>>= \_ -> Wrap7 (Ask7        ((>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) .     Return7)))          (Return7  ())) ["in"]
 , "o" :     simulate7                          (Wrap7 (Ask7        ((>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) .     Return7)))                         ["in"]
 , "o" :                                        simulate7          (((>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7)) .     Return7) "in")                     []
 , "o" :                                        simulate7           ((>>= \name -> Wrap7 (Say7 ("oo:" ++ name) Return7))      (Return7  "in"))                    []
 , "o" :                                        simulate7                         (Wrap7 (Say7 ("oo:" ++ "in") Return7))                                          []
 , "o" :                                        ("oo:" ++ "in") :  simulate7                                  (Return7 ())                                        []
 , "o" :                                        ("oo:" ++ "in") :  []
 ]
 ["o","oo:in"]

------------------------------------------------------------------------------
-- 36'00" more examples

-- ...
------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList $ t1 ++ t2 ++ t3 ++ t3' ++ t7

-- End of file.
