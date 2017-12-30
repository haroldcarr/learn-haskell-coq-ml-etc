> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE KindSignatures         #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE PolyKinds              #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module LinearLamMini1 where
>
> import           Prelude hiding ((*), (+), (^))

LLC : Linear Lambda Calculus

1. Intro

Higher-order abstract syntax (HOAS)
- representing binding structure for LLC (e.g,, lambdas in lambda calculus)
  with the binding structure of Haskell.

Central idea
- explicitly represent the variable environment
  so it can be analyzed during type checking/inference by the type class machinery.

Uses tagless final encodings and type-level programming.

2. Tagless Final Encodings

Tagless final encoding uses Haskell _terms_ to encode LLC (instead of _values_ of Haskell ADTs).  See LLC below.

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
---------------- -<>I (abstraction)
 ∆⊢λˆx.e:A -<> B


∆0 ⊢ e0:A -<> B    ∆1 ⊢ e1:A
----------------------------- -<>E (application)
      ∆0 >< ∆1 ⊢ e0ˆe1:B

The context split, read from conclusion to premises
- prevents Contraction from holding
- i.e. variables must be used at most once, since no variable can be copied into both premises

The context split is non-deterministic when read from conclusion to premises
- to do a Haskell encoding, need to remove this non-determinism from type checking

4.1 IO system

To remove the non-determinism in the -<>E rule
- lazily split the context by passing all available variables to the first premise
- and passing the remaining ones to the second premise

-- 4.2 Haskell Encoding of Multiplicatives

Using HOAS, but behavior of Haskell variables does not match that of linear variables.
- typing rules do not match
- but binding and substitution of linear variables do follow that of Haskell variables
  - no need to explicitly define substitution for LLC terms
    since it is identical to subsitution on regular lambda calculus terms
- use "forgetful" encoding of LLC into Haskell where linearity enforced statically when type checking
  - but underlying function is a Haskell function

> newtype a -<> b = Lolli { unLolli :: a -> b }  -- TypeOperators to declare operators in type and declarations

Encoding requires info for the type checker to decide whether a variable is being used linearly.
Accomplished by decorating representation type with an abstraction of the linear context.

Use DataKinds for a type level abstraction of the linear context:

> data Nat = Z | S Nat
> data CtxElm = Box | Elm Nat

Info needed is whether a variable has been used (no need to track type of the variable).
- tag each in-scope linear variable with a type level Nat
- store tags in abstract context

abstract representation type looks like

  repr :: Nat -> [CtxElm] -> [CtxElm] -> * -> *
  repr    v : counter for generating a new tag
                 i : input (abstract) context
                             o : output (abstract) context
                                         a : LLC term of type a (derivation of a linear type a)

Representation of -<>I rule (linear function) with Haskell functions.

Use lvar rule from 4.1: the derivation rule represented by the argument repr above.

direct transcription of lvar rule:

> type LVar repr v a =
>   forall (v'::Nat)                -- KindSignatures
>          (i::[CtxElm])
>          (o::[CtxElm]) .          --  RankNTypes
>   Consume v i o => repr v' i o a

States
- any i and o for which constraint Consume v i o holds
- can be used to form a derivation of the variable v of type a.

Consume is a type level relation specifying that v occurs in i and is replaced by Box in o.

> class Consume (v::Nat)       -- MultiParamTypeClasses
>               (i::[CtxElm])
>               (o::[CtxElm])
>   | v i -> o                 -- FunctionalDependencies
> class Consume1 (b::Bool)
>                (v::Nat)
>                (x::Nat)
>                (i::[CtxElm])
>                (o::[CtxElm])
>   | b v x i -> o
>
> instance (Consume v i o)                -- UndecidableInstances
>   => Consume v (Box ': i) (Box ': o)    -- FlexibleInstances
> instance (EQ v x b, Consume1 b v x i o)
>   => Consume v (Elm x ': i) o

> instance Consume1 True v x i (Box ': i)
> instance (Consume v i o)
>   => Consume1 False v x i (Elm x ': o)

> class EQ (x::k) (y::k) (b::Bool) | x y -> b   -- 'k' kind variable : PolyKinds
> instance {-# OVERLAPPABLE #-} EQ x x True
> instance {-# OVERLAPPABLE #-} (b ~ False) => EQ x y b  -- equational constraint : GADTs

EQ encodes equality with respect to the Haskell type checker’s unification machinery
subject to the constraint solver’s ability to make progress
- if EQ x y b holds then x and y unify and b will be ’True, or b will be ’False.

Type level equality that reflects unifiability
- necessary for encoding to work
  since Consume needs to realize that v does not equal S v

Encoding of linear functions:

> class LLC (repr :: Nat -> [CtxElm] -> [CtxElm] -> * -> *)
>  where
>   -- -<>I (abstraction)
>   llam :: (LVar repr v a -> repr (S v) (Elm v ': i) (Box ': o) b)
>        -> repr v i o (a -<> b)
>   -- -<>E (application)
>   (^)  :: repr v i m (a -<> b)
>        -> repr v m o a
>        -> repr v i o b

Type of (^) is direct transcription of the  -<>E rule.

:l LinearLamMini1
:t llam $ \f -> llam $ \x -> f ^ x
=> :: LLC repr => repr v o o ((a -<> b) -<> (a -<> b))

But ill-typed example (i.e., linear variable `x` used more than once)
does not fail (if the following Defn/defn is commented out)
because Consume constraint can’t be further analyzed because
the input and output linear contexts have not been constrained.

:t llam $ \f -> llam $ \x -> f ^ x ^ x
=> :: (Consume ('S v) i o, LLC repr)
   => repr v i o ((a -<> (a -<> b)) -<> (a -<> b))

Need to provide way to declare a closed linear term, or definition.
- do not want to require that definitions have empty input and output contexts
  since that would preclude using them in a context where there are linear variables in scope
- want to declare that definitions do not change their input linear context:

> type Defn a = forall repr (v::Nat) (i::[CtxElm])
>               . LLC repr => repr v i i a
> defn :: Defn a -> Defn a
> defn x = x

Now it fails:

:l LinearLamMini1
:t defn $ llam $ \f -> llam $ \x -> f ^ x ^ x
  Could not deduce (Consume ('S v1) i1 i1)
        arising from a use of ‘x'

Also: linear variables cannot be ignored:

:l LinearLamMini1
:t defn $ llam $ \f -> llam $ \x -> f
-- TODO - this gets a Couldn't match type error instead of expected Could not deduce error

------------------------------------------------------------------------------

evaluation

> newtype Ev (v::Nat)
>            (i::[CtxElm])
>            (o::[CtxElm])
>            a
>   = Ev {ev :: a}

> instance LLC
>   (Ev :: Nat
>       -> [CtxElm]
>       -> [CtxElm]
>       -> * -> *)
>  where
>   llam f = Ev $ Lolli $ \x -> ev (f (Ev x))
>   f ^ x  = Ev $ unLolli (ev f) (ev x)

> eval :: Ev Z '[] '[] a -> a
> eval = ev

:l LinearLamMini1
  (unLolli . eval $ llam (\x -> x)) "hello"
=> "hello"

