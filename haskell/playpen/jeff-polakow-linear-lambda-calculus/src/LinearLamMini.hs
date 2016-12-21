{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module LinearLamMini where

import           Prelude hiding ((*), (+), (^))

-- 2. Tagless Final Encodings

class Exp repr where
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b

-- 4.2 Haskell Encoding of Multiplicatives

newtype a -<> b = Lolli {unLolli :: a -> b}

data Nat = Z | S Nat
data CtxElm = Box | Elm Nat
{-
type LVar repr v a =
  forall (v'::Nat) (i::[CtxElm]) (o::[CtxElm]) .
  Consume v i o => repr v' i o a
-}
class Consume (v::Nat)
              (i::[CtxElm])
              (o::[CtxElm])
  | v i -> o
class Consume1 (b::Bool)
               (v::Nat)
               (x::Nat)
               (i::[CtxElm])
               (o::[CtxElm])
  | b v x i -> o
instance (Consume v i o)
  => Consume v (Box ': i) (Box ': o)
instance (EQ v x b, Consume1 b v x i o)
  => Consume v (Elm x ': i) o
instance Consume1 True v x i (Box ': i)
instance (Consume v i o)
  => Consume1 False v x i (Elm x ': o)
class EQ (x::k) (y::k) (b::Bool) | x y -> b
instance {-# OVERLAPPABLE #-} EQ x x True
instance {-# OVERLAPPABLE #-} (b ~ False) => EQ x y b
{-
class LLC
  (repr :: Nat -> [CtxElm] -> [CtxElm] -> * -> *)
 where
  llam :: (LVar repr v a ->
           repr (S v) (Elm v ': i) (Box ': o) b
          )
       -> repr v i o (a -<> b)
  (^) :: repr v i m (a -<> b)
      -> repr v m o a
      -> repr v i o b
-}
{-
:l LinearLamMini
:t llam $ \f -> llam $ \x -> f ^ x
=> :: (EQ ('S v) ('S v) 'True, EQ v v 'True, LLC repr)
   => repr v o o ((a -<> b) -<> (a -<> b))
-}
{-
type Defn a = forall repr (v::Nat) (i::[CtxElm])
              . LLC repr => repr v i i a
defn :: Defn a -> Defn a
defn x = x
-}
{-
:l LinearLamMini
:t defn $ llam $ \f -> llam $ \x -> f ^ x ^ x
  Could not deduce (Consume ('S v1) i1 i1)
        arising from a use of ‘x'
-- shows that a linear variable cannot be used twice
:t defn $ llam $ \f -> llam $ \x -> f
-- TODO - this gets a Couldn't match type error instead of expected Could not deduce error
-}

-- 5.2 Haskell Encoding of Additives

class Or (x::Bool) (y::Bool) (z::Bool) | x y -> z
instance Or True y True
instance Or False y y

class And (x::Bool) (y::Bool) (z::Bool) | x y -> z
instance And False y False
instance And True y y

class MrgL (h1::[CtxElm])
           (tf1::Bool)
           (h2::[CtxElm])
           (tf2::Bool)
           (h::[CtxElm])
  | h1 h2 -> h
instance MrgL '[] v1 '[] v2 '[]
instance (MrgL h1 v1 h2 v2 h)
  => MrgL (x ': h1) v1 (x ': h2) v2 (x ': h)
instance (MrgL h1 True h2 v2 h)
  => MrgL (Elm x ': h1) True (Box ': h2) v2 (Box ': h)
instance (MrgL h1 v1 h2 True h)
  => MrgL (Box ': h1) v1 (Elm x ': h2) True (Box ': h)

class VarOk (tf :: Bool) (v :: CtxElm)
instance VarOk True (Elm v)
instance VarOk True Box
instance VarOk False Box

type a & b = (a, b)
type Top = ()

type LVar repr v a =
  forall (v'::Nat) (i::[CtxElm]) (o::[CtxElm]) .
  Consume v i o => repr v' False i o a

class LLC
  (repr :: Nat -> Bool
  -> [CtxElm] -> [CtxElm] -> * -> *
  )
 where
   llam :: VarOk tf var
        => (LVar repr v a -> repr (S v) tf (Elm v ': i) (var ': o) b)
        -> repr v tf i o (a -<> b)
   (^) :: (Or tf0 tf1 tf)
       => repr v tf0 i m (a -<> b)
       -> repr v tf1 m o a
       -> repr v tf i o b
   top :: repr v True i i Top
   (&) :: ( MrgL h0 tf0 h1 tf1 o
          , And tf0 tf1 tf
          )
       => repr v tf0 i h0 a
       -> repr v tf1 i h1 b
       -> repr v tf i o (a & b)
   pi1 :: repr v tf i o (a & b)
       -> repr v tf i o a
   pi2 :: repr v tf i o (a & b)
       -> repr v tf i o b

type MrgLs i = ( MrgL i False i False i
               , MrgL i False i True i
               , MrgL i True i False i
               , MrgL i True i True i
               )
type Defn tf a =
  forall repr (v::Nat) (i::[CtxElm]) .
  (LLC repr, MrgLs i) => repr v tf i i a
defn :: Defn tf a -> Defn tf a
defn x = x

{-
:t defn $ llam $ \f -> llam $ \x -> llam $ \y -> (f ^ x ^ y) & (f ^ y ^ x)
-}

-- 6.1 Haskell Encoding of Unrestricted Functions

-- ...
