> {-# LANGUAGE DeriveFunctor        #-}
> {-# LANGUAGE FlexibleContexts     #-}
> {-# LANGUAGE StandaloneDeriving   #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module F_JT_tour_of_recursive_types where

 https://medium.com/@jaredtobin/a-tour-of-some-useful-recursive-types-8fa8e423b5b9#.96mi9w169

These

    data Natural
      = One
      | Succ Natural

    data List a
      = Empty
      | Cons a (List a)

have similar recursion
- single base case
- single recursive point in a single constructor

------------------------------------------------------------------------------

> -- | Templated recursive structure.
> -- relies on functor ‘f’ to define scope of recursion that value with type ‘Fix f’ supports.
> -- base case (termination) controlled by functor (not Fix).
> newtype Fix f   = Fix      (f (Fix    f))

> -- | `Fix` with added structure : sum type with two branches.
> -- A program either runs an instruction from `f` or terminates/Pure with an `a` value.
> -- `Free` value contains at most one recursive point wrapping the value type ‘a’.
> -- `Pure` wraps a value of type ‘a’, does not use `f`.
> -- Consequence: `Free` can decide termination, regardless of `f`.
> -- `Free` is monadic: called `free monad`.
> data Free   f a = Free     (f (Free   f a))
>                 | Pure a
>                 deriving Functor

> -- | `Fix` with extra structure : product type with two fields. Categorical dual of `Free`.
> -- A program that runs an instruction from `f` and returns an `a` value.
> -- `Cofree` value contains potentially infinite recursive points — each tagged with a value of type ‘a’.
> -- A program where every instruction is annotated with a value of type ‘a’.
> -- Means (unlike `Free`, like `Fix`), it can’t decide termination : must let `f` decide.
> data Cofree f a = Cofree a (f (Cofree f a)) deriving Functor

> deriving instance         (Show (f (Fix    f)))   => Show (Fix    f)
> deriving instance (Show a, Show (f (Free   f a))) => Show (Free   f a)
> deriving instance (Show a, Show (f (Cofree f a))) => Show (Cofree f a)

------------------------------------------------------------------------------

> -- FIX : for all `f` can embed into or project out of anything with type ‘Fix f’
> -- | embeds `f` into recursive structure by adding a layer of recursion
> fix            :: f (Fix  f)   -> Fix  f
> fix             = Fix
> -- | projects a value of type ‘f’ out of a recursive structure by removing a layer of recursion
> unfix          :: Fix f -> f (Fix f)
> unfix (Fix f)   = f

> -- FREE : for all `f` can embed into anything with type ‘Fix f’
> -- | embedding
> free           :: f (Free f a) -> Free f a
> free            = Free
> -- | cannot implement TOTAL unfree
> unfree         :: Free f a -> f (Free f a)
> unfree (Free f) = f
> unfree (Pure a) = error "no `f` to pluck out"

> -- COFREE : for all `f` can project out of anything with type ‘Fix f’
> -- opposite situation to `Free`
> -- Forms a comonad : "cofree comonad"
> -- | cannot implementing TOTAL cofree
> cofree :: f (Cofree f a) -> Cofree f a
> cofree {-f-} = Cofree (error "cannot come up with arbitrary value of type `a`") {-f-}
> -- projection
> uncofree :: Cofree f a -> f (Cofree f a)
> uncofree (Cofree _ f) = f

------------------------------------------------------------------------------

no recursion functor (type param `r` is not used)

> data NoRecursionF r = NoRecursionF deriving (Functor, Show)
> type NoRecursion    = Fix NoRecursionF
> noRecursion        :: NoRecursion
> noRecursion         = fix NoRecursionF

infinite recursion (no base case)

> newtype InfiniteF r = InfiniteF r deriving (Functor, Show)
> type    Infinite    = Fix InfiniteF
> infinite           :: Infinite
> infinite            = fix (InfiniteF infinite)

> -- even though InfiniteF has no base/terminating case, will terminate because of `Pure`
> type NotSoInfinite  = Free InfiniteF
> notSoInfinite      :: NotSoInfinite () -- unit value, but could be anything
> notSoInfinite       = free (InfiniteF (Free (InfiniteF (Pure ()))))

one recursive point plus one base (terminating) case

> -- FIX
> data         NatF r = ZeroF | SuccF r deriving (Functor, Show)
> type         Nat    = Fix NatF
> zero               :: Nat
> zero                = fix ZeroF
> succ               :: Nat -> Nat
> succ                = fix . SuccF

> -- FREE
> type        NatFree = Free NatF
> zeroFree           :: NatFree a
> zeroFree            = free ZeroF
> succFree           :: NatFree a -> NatFree a
> succFree            = free . SuccF

> -- COFREE
> type      NatCofree = Cofree NatF
> zeroCofree         :: NatCofree ()
> zeroCofree          = Cofree () ZeroF
> succCoFree         :: NatCofree () -> NatCofree ()
> succCoFree       f  = Cofree () (SuccF f)

------------------------------------------------------------------------------

`Fix` can be thought of as defining a program that runs until `f` decides to terminate.
`f` can be thought of instruction set for the program.

> -- FIX : Program terminates only if `f` contains a base/terminating instruction.
> data InstructionF r = IncrementF r | DecrementF r | TerminateF deriving (Functor, Show)
> type InstructionFix = Fix InstructionF
> incrementFix       :: InstructionFix -> InstructionFix
> incrementFix        = fix . IncrementF
> decrementFix       :: InstructionFix -> InstructionFix
> decrementFix        = fix . DecrementF
> terminateFix       :: InstructionFix
> terminateFix        = fix TerminateF

> programFix :: InstructionFix
> programFix = incrementFix . incrementFix . decrementFix $ terminateFix

> -- FREE : Program terminates if `f` contains a base/terminating case or `Pure` used.
> type InstructionFree= Free InstructionF
> incrementFree      :: InstructionFree a -> InstructionFree a
> incrementFree       = free . IncrementF
> decrementFree      :: InstructionFree a -> InstructionFree a
> decrementFree       = free . DecrementF
> terminateFree      :: InstructionFree a
> terminateFree       = free TerminateF
> sigkillFree        :: InstructionFree Int -- independent of "instruction set"
> sigkillFree         = Pure 1

> programFree :: InstructionFree Int
> programFree = incrementFree . incrementFree . decrementFree $ sigkillFree

> -- COFREE : (like `Fix`) Program terminates only if `f` contains a base/terminating instruction.
> type InstructionCofree = Cofree InstructionF
> incrementCofree    :: InstructionCofree Int -> InstructionCofree Int
> incrementCofree p   = Cofree (remaining p) (IncrementF p)
> decrementCofree    :: InstructionCofree Int -> InstructionCofree Int
> decrementCofree p   = Cofree (remaining p) (DecrementF p)
> terminateCofree    :: InstructionCofree Int
> terminateCofree     = Cofree 0 TerminateF

> -- | counts number of instructions left in program
> remaining :: InstructionCofree Int -> Int
> remaining = loop where
>   loop (Cofree a f) = case f of
>     IncrementF p -> 1 + loop p
>     DecrementF p -> 1 + loop p
>     TerminateF   -> 1 + a

> programCofree :: InstructionCofree Int
> programCofree  = incrementCofree . incrementCofree . decrementCofree $ terminateCofree

------------------------------------------------------------------------------

‘Fix’, ‘Free’, and ‘Cofree’ : share similar recursive structure.

Useful for encoding programs, given some instruction set.

‘Fix’ richest : can both ‘embed’ things into and ‘project’ things in/out of its recursive structure.

‘Free’ : only embedding.

‘Cofree’ only projecting.

Implication: can’t use certain recursion schemes for ‘Free’ and ‘Cofree’ in same way  one can for ‘Fix’.
- there do exist analogues

Use-case: embedded languages.

