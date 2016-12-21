{-# LANGUAGE ConstraintKinds        #-}
> {-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

> module LinearLamMini where
>
> import           Prelude hiding ((*), (+), (^))

LLC : Linear Lambda Calculus

1. Intro

Higher-order abstract syntax (HOAS)
- representing binding structure for LLC (e.g,, lambdas in lambda calculus)
  with the binding structure of Haskell.

Central idea
- explicitly represent the variable environment
  which can be analyzed during type checking/inference by the type class machinery.

Uses tagless final encodings and type-level programming.

2. Tagless Final Encodings

Tagless final encoding uses Haskell terms to encode LLC (instead of values of Haskell ADTs).

> class Exp repr where
>   lam :: (repr a -> repr b) -> repr (a -> b)
>   app :: repr (a -> b) -> repr a -> repr b

repr : abstract type representing an `Exp` term being constructed
- decorated with the Haskell type of that Exp term.

HOAS encoding
- transcribe typing rules of LLC into Haskell types.

3. Mini Linear Lambda Calculus

LLC restricted to smallest fragment that captures the complexity of linear type checking and inference.

4. Multiplicatives

----------lvar
x:A ⊢ x:A

A variable can only be used if there are no other variables in scope.
- The lack of other variables prevents Weakening from holding
- i.e. variables must be used at least once.


   ∆,x:A ⊢ e:B
---------------- -<>I
 ∆⊢λˆx.e:A -<> B


∆0 ⊢ e0:A -<> B    ∆1 ⊢ e1:A
----------------------------- -<>E
      ∆0 >< ∆1 ⊢ e0ˆe1:B

The context split, read from conclusion to premises
- prevents Contraction from holding
- i.e. variables must be used at most once, since no variable can be copied into both premises

The context split is non-deterministic when read from conclusion to premises
- to do a Haskell encoding, need to remove this non-determinism from type checking

4.1 IO system

To remove the non-determinism in the E rule
- lazily split the context by passing all available variables to the first premise
- and passing the remaining ones to the second premise

-- 4.2 Haskell Encoding of Multiplicatives

Using HOAS, but behavior of Haskell variables does not match that of linear variables.
- typing rules do not match
- but binding and substitution of linear variables do follow that of Haskell variables
  - no need to explicitly define substitution for LLC terms as it is identical to subsitution on regular lambda calculus terms
- "forgetful" encoding of LLC into Haskell where linearity enforced statically when type checking
  - but underlying function is a Haskell function

Illegal declaration of a type or class operator ‘-<>’
Use TypeOperators to declare operators in type and declarations

> newtype a -<> b = Lolli { unLolli :: a -> b }

This encoding requires information for the type checker to decide whether a variable is being used linearly.
Accomplished by decorating representation type with an abstraction of the linear context.

Use DataKinds for a type level abstraction of the linear context:

> data Nat = Z | S Nat
> data CtxElm = Box | Elm Nat

Info needed is whether a variable has been used (no need to track type of the variable).
- so tag each in-scope linear variable with a type level Nat
- and store tags in abstract context

abstract representation type looks like

  repr :: Nat -> [CtxElm] -> [CtxElm] -> * -> *
  repr v i o a = v : counter for generating a new tag
                 i : input (abstract) context
                 o : output (abstract) context
                 a : LLC term of type a (derivation of a linear type a)

representation of -<>I rule (linear function)

Represent linear functions with Haskell functions.

Use lvar rule from 4.1: that is the derivation rule represented by the argument repr above.

direct transcription of lvar rule:

> type LVar repr v a =
>   forall (v'::Nat)
>          (i::[CtxElm])
>          (o::[CtxElm]) .
>   Consume v i o => repr v' i o a

States
- any i and o for which constraint Consume v i o holds
- can be used to form a derivation of the variablev of type a.

Consume is a type level relation specifying that v occurs in i and is replaced by Box in o.

> class Consume (v::Nat)
>               (i::[CtxElm])
>               (o::[CtxElm])
>   | v i -> o
> class Consume1 (b::Bool)
>                (v::Nat)
>                (x::Nat)
>                (i::[CtxElm])
>                (o::[CtxElm])
>   | b v x i -> o
>
> instance (Consume v i o)
>   => Consume v (Box ': i) (Box ': o)
> instance (EQ v x b, Consume1 b v x i o)
>   => Consume v (Elm x ': i) o

> instance Consume1 True v x i (Box ': i)
> instance (Consume v i o)
>   => Consume1 False v x i (Elm x ': o)

> class EQ (x::k) (y::k) (b::Bool) | x y -> b
> instance {-# OVERLAPPABLE #-} EQ x x True
> instance {-# OVERLAPPABLE #-} (b ~ False) => EQ x y b

EQ encodes equality with respect to the Haskell type checker’s unification machinery
subject to the constraint solver’s ability to make progress
- if EQ x y b holds then x and y unify and b will be ’True, or b will be ’False.

Type level equality that reflects unifiability
- necessary for encoding to work
  since Consume needs to realize that v does not equal S v

To finish encoding of linear functions:

> class LLC (repr :: Nat -> [CtxElm] -> [CtxElm] -> * -> *)
>  where
>   llam :: (LVar repr v a ->
>            repr (S v) (Elm v ': i) (Box ': o) b
>           )
>        -> repr v i o (a -<> b)
>   (^)  :: repr v i m (a -<> b)
>        -> repr v m o a
>        -> repr v i o b


Type of (^) is direct transcription of the  -<>E rule.

:l LinearLamMini
:t llam $ \f -> llam $ \x -> f ^ x
=> :: (EQ ('S v) ('S v) 'True, EQ v v 'True, LLC repr)
   => repr v o o ((a -<> b) -<> (a -<> b))

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
