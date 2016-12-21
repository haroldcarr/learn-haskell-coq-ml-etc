{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LinearLam where

import Prelude hiding( (^), (*), (+))

--
-- Linear types
--

newtype a -<> b = Lolli { unLolli :: a -> b }
newtype Bang a  = Bang  { unBang  :: a }
type    Top     = ()
type    a & b   = (a, b)
data    a * b   = Tensor a b
data    a + b   = Inl a | Inr b
data    One     = One
data    Zero

--
-- linear variable vid in Haskell context
--

type LVar repr (vid::Nat) a =
  forall (v::Nat)
         (i::[Maybe Nat])
         (o::[Maybe Nat])
  . Consume vid i o => repr v False i o a

--
-- unrestricted variable in Haskell context
--

type UVar repr a =
  forall (vid::Nat)
         (i::[Maybe Nat])
  . repr vid False i i a

--
-- The syntax of LLC.
--

class LLC (repr :: Nat
                -> Bool
                -> [Maybe Nat]
                -> [Maybe Nat]
                -> *
                -> *
          ) where
  llam
    :: (VarOk tf var)
    => (LVar repr vid a -> repr (S vid)
                                tf
                                (Just vid ': i)
                                (var ': o)
                                b
       )
    -> repr vid tf i o (a -<> b)
  (^)
    :: (Or tf1 tf2 tf)
    => repr vid tf1 i h (a -<> b)
    -> repr vid tf2 h o a
    -> repr vid tf i o b
  ulam
    :: (UVar repr a -> repr vid tf i o b)
    -> repr vid tf i o (a -> b)
  ($$)
    :: repr vid tf0 i o (a -> b)
    -> repr vid tf1 '[] '[] a
    -> repr vid tf0 i o b
  bang
    :: repr vid tf '[] '[] a
    -> repr vid False i i (Bang a)
  {-
  letBang
    :: (Or tf0 tf1 tf)
    => repr vid tf0 i h (Bang a)
    -> (UVar repr a -> repr vid tf1 h o b)
    -> repr vid tf i o b
  -}
  top
    :: repr vid True i i Top
  (&)
    :: ( MrgL h0 tf0 h1 tf1 o
       , And tf0 tf1 tf
       )
    => repr vid tf0 i h0 a
    -> repr vid tf1 i h1 b
    -> repr vid tf i o (a & b)
  pi1
    :: repr vid tf i o (a & b)
    -> repr vid tf i o a
  pi2
    :: repr vid tf i o (a & b)
    -> repr vid tf i o b
  one
    :: repr vid False i i One
  letOne
    :: (Or tf0 tf1 tf)
    => repr vid tf0 i h One
    -> repr vid tf1 h o a
    -> repr vid tf i o a
  (*)
    :: (Or tf0 tf1 tf)
    => repr vid tf0 i h a
    -> repr vid tf1 h o b
    -> repr vid tf i o (a * b)
  {-
  letStar
    :: ( VarOk tf1 var0
       , VarOk tf1 var1
       , Or tf0 tf1 tf
       )
    => repr vid tf0 i h (a * b)
    -> (LVar repr vid a
        -> LVar repr (S vid) b
        -> repr (S (S vid))
                tf1
                (Just vid ': Just (S vid) ': h)
                (var0 ': var1 ': o)
                c
       )
    -> repr vid tf i o c
  -}
  inl
    :: repr vid tf i o a
    -> repr vid tf i o (a + b)
  inr
    :: repr vid tf i o b
    -> repr vid tf i o (a + b)
  {-
  letPlus
    :: ( MrgL o1 tf1 o2 tf2 o
       , And tf1 tf2 tf3
       , Or tf0 tf3 tf
       , VarOk tf1 var1
       , VarOk tf2 var2
       )
    => repr vid tf0 i h (a + b)
    -> (LVar repr vid a -> repr (S vid)
                                tf1
                                (Just vid ': h)
                                (var1 ': o1)
                                c
       )
    -> (LVar repr vid b -> repr (S vid)
                                tf2
                                (Just vid ': h)
                                (var2 ': o2)
                                c
       )
    -> repr vid tf i o c
  -}
  abort
    :: repr vid tf i o Zero
    -> repr vid True i o a

--
-- A definition for a closed LLC term.
--

type MrgLs i = ( MrgL i False i False i
               , MrgL i False i True i
               , MrgL i True i False i
               , MrgL i True i True i
               )
type Defn tf a =
  forall repr i vid
  . (LLC repr, MrgLs i)
  => repr vid tf i i a
defn :: Defn tf a -> Defn tf a
defn x = x

{-------------------------------------------------
Type level machinery
-------------------------------------------------}

--
-- We will use type level Nats
--

data Nat = Z | S Nat

class Or (x::Bool) (y::Bool) (z::Bool) | x y -> z
instance Or True y True
instance Or False y y

class And (x::Bool) (y::Bool) (z::Bool) | x y -> z
instance And False y False
instance And True y y

--
-- Type level machinery for consuming a variable
-- in a list of variables.
--

class Consume (v::Nat)
              (i::[Maybe Nat])
              (o::[Maybe Nat])
      | v i -> o

class Consume1 (b::Bool)
               (v::Nat)
               (x::Nat)
               (i::[Maybe Nat])
               (o::[Maybe Nat])
      | b v x i -> o

instance (Consume v i o)
  => Consume v (Nothing ': i) (Nothing ': o)
instance (EQ v x b, Consume1 b v x i o)
  => Consume v (Just x ': i) o
instance Consume1 True v x i (Nothing ': i)
instance (Consume v i o)
  => Consume1 False v x i (Just x ': o)

class EQ (x::k) (y::k) (b::Bool) | x y -> b
instance {-# OVERLAPPABLE #-} EQ x x True
instance {-# OVERLAPPABLE #-} (b ~ False) => EQ x y b

--
-- Type level machinery for merging outputs of
-- additive operations and getting right Top flag.
--

class MrgL (h1::[Maybe Nat])
           (tf1::Bool)
           (h2::[Maybe Nat])
           (tf2::Bool)
           (h::[Maybe Nat])
  | h1 h2 -> h
instance MrgL '[] v1 '[] v2 '[]
instance (MrgL h1 v1 h2 v2 h)
  => MrgL (x ': h1)
          v1
          (x ': h2)
          v2
          (x ': h)
instance (MrgL h1 True h2 v2 h)
  => MrgL (Just x ': h1)
          True
          (Nothing ': h2)
          v2
          (Nothing ': h)
instance (MrgL h1 v1 h2 True h)
  => MrgL (Nothing ': h1)
          v1
          (Just x ': h2)
          True
          (Nothing ': h)

--
-- Check, in -<> type rule, that Top flag
-- was set or hypothesis was consumed.
--

class VarOk (tf :: Bool) (v :: Maybe Nat)
instance VarOk True (Just v)
instance VarOk True Nothing
instance VarOk False Nothing

-----------------------

newtype Ev (v::Nat)
           (tf::Bool)
           (i::[Maybe Nat])
           (o::[Maybe Nat])
           a
  = Ev {ev :: a}

instance LLC
  (Ev :: Nat
      -> Bool
      -> [Maybe Nat]
      -> [Maybe Nat]
      -> * -> *
  )
 where
  llam f = Ev $ Lolli $ \x -> ev (f (Ev x))
  f ^ x = Ev $ unLolli (ev f) (ev x)
  ulam f = Ev $ \x -> ev (f (Ev x))
  f $$ x = Ev $ ev f (ev x)
  top = Ev ()
  x & y = Ev $ (ev x, ev y)
  pi1 = Ev . fst . ev
  pi2 = Ev . snd . ev
  bang = undefined
  -- letBang = undefined
  one = undefined
  letOne = undefined
  (*) = undefined
  -- letStar = undefined
  inl = undefined
  inr = undefined
  -- letPlus = undefined
  abort = undefined

eval :: Ev Z tf '[] '[] a -> a
eval = ev

{-
  (unLolli . eval $ llam (\x -> x)) "hello"
=> "hello"
:t                  llam (\x -> x)
=> :: LLC repr => repr vid 'False o o (b -<> b)
:t           eval $ llam (\x -> x)
=> ::                                  b -<> b
:t unLolli . eval $ llam (\x -> x)
=> ::                                  b ->  b
-}
