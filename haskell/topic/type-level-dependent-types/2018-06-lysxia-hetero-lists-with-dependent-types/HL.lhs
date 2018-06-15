> {-# LANGUAGE
>     AllowAmbiguousTypes,
>     ConstraintKinds,
>     DataKinds,
>     FlexibleContexts,
>     FlexibleInstances,
>     GADTs,
>     InstanceSigs,
>     MultiParamTypeClasses,
>     PolyKinds,
>     RankNTypes,
>     ScopedTypeVariables,
>     TypeApplications,
>     TypeFamilies,
>     TypeOperators,
>     UndecidableInstances #-}
>
> module HL where

https://blog.poisson.chat/posts/2018-06-06-hlists-dependent-haskell.html
Heterogeneous lists with dependent types in Haskell
June 6, 2018

InstanceSigs : type signatures to appear in instance declarations
- convenience: no need to go to type class to remember what is being defined

ConstraintKinds, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, UndecidableInstances
- lift historical restrictions

hlists often defined with GADTs, here, something different

> -- Infix "pair" type (a "cons")
> -- Will use '()' as nil.
> -- Infix TYPE constructor (:*) is enabled via TypeOperators
> data a :* b = a :* b deriving Show
> infixr 1 :*

> hlEx1 :: Int :* String :* String :* ()
> hlEx1 =  1   :* "One"  :* "1"    :* ()

why have a nil, since any type can be to the right of (:*)?:

> hlEx2 :: Int :* String :* String
> hlEx2 =  1   :* "One"  :* "1"

Is above a list of three elements, or a list of two elements (one Int and one String :* String)?

Avoid unnecessary complication by using clear termination: ().

Still possible to accidentally construct above ill-formed lists.

------------------------------------------------------------------------------

Use-case: get an element by type

extract element of a given type in the list.

------------------------------------------------------------------------------
CONVENTIONAL APPROACH

> class Get0 a bs where
>  get0 :: bs -> a

write two overlapping instances

> -- when type of head doesn’t match, keep looking in tail
> instance Get0 a bs => Get0 a (b :* bs) where
>   get0 :: (b :* bs) -> a
>   get0 (_ :* ys) = get0 ys
>
> -- when types match, return head
> instance {-# OVERLAPPING #-} Get0 a (a :* bs) where -- more specific : 'a' at head
>   get0 :: (a :* bs) -> a
>   get0 (x :* _) = x

> get0Ex1 :: String
> get0Ex1 = get0 @String ((1 :: Int) :* "One" :* "1" :* ()) -- uses TypeApplications : @
> -- "One"

more abstract operation:

  getThird0 :: (a :* b :* c :* ds) -> c
  getThird0 = get0

Error: Overlapping instances for Get0 c (a :* (b :* (c :* ds)))

cannot determine whether a is equal to c, so can’t choose one of above instances
- when overlap, rule prefers most specific instance
- one way to make work
  - add constraint : resolves at use sites with concrete types

> getThird0
>   :: Get0 c (a :* b :* c :* ds)
>   => (a :* b :* c :* ds)
>   -> c
> getThird0 = get0
>
> get0Ex2a :: String
> get0Ex2a = getThird0 ((1::Int) :* (2.0::Double) :* "Three" :* 'c' :* ())
> -- "Three"

but if c is equal to a or b, above will not do what its name and type say

> -- a, b, c all equal to String, ds ~ (String :* ())
> get0Ex2b :: String
> get0Ex2b = getThird0 ("One" :* "Two" :* "Three" :* "Four" :* ())
> -- "One" (but should be "Three")

one solution: add constraint that c is not equal to a or b
- makes getThird less general

------------------------------------------------------------------------------
TYPE DISEQUALITY CONSTRAINTS

Haskell has equality constraint (~), but no disequality constraint “(/~)”,
nor any general way to negate a constraint (incompatible with open type classes).

solution: define equality as a boolean-valued closed TypeFamily

> type family a == b :: Bool where
>   a == a = 'True
>   a == b = 'False

type families enable nonlinear pattern-matching
- first clause matches twice on same variable
- not allowed at term level
  - because no universal way of comparing values
  - especially infinite values, functions, and values of abstract types such as IO

Now can match on outcome of equality
    'False ~ (a == b) -- holds iff a and b are not equal
    'True  ~ (a == b) -- otherwise
         a ~ b        -- better style for True equality

> -- 'aeqb' param to carry comparison result
> -- superclass is for safety : ensuring there is only one way use class
> -- - must set boolean aeqb to a == b, and instances must satisfy that constraint
> class (aeqb ~ (a == b))                 => GetIf aeqb   a b bs where
>   getIf :: (b :* bs) -> a
>
> instance (a ~ b)                        => GetIf 'True  a b bs where
>   getIf :: (a :* bs) -> a
>   getIf (x :* _) = x
>
> instance ('False ~ (a == b), Get1 a bs) => GetIf 'False a b bs where
>   getIf :: (b :* bs) -> a
>   getIf (_ :* ys) = get1 ys
>
> class                                      Get1         a   bs where
>   get1 :: bs -> a
>
> instance GetIf (a == b) a b bs          => Get1    a (b :* bs) where
>   get1 = getIf

main benefit of this over previous : avoids overlapping instances
- overlap still exists (in definition of (==)), but in a general and reusable construct

for clarity : define "class synonym" for type disequality
- for constraints, advantage over type synonyms
  - type synonyms can’t be partially applied
  - class synonyms can

> class    ('False ~ (a == b)) => a /~ b
> instance ('False ~ (a == b)) => a /~ b

> getThird1 :: (c /~ a, c /~ b) => (a :* b :* c :* ds) -> c
> getThird1 = get1

now using getThird1 with types for 1st or 2nd element equal to 3rd results in type error.

  getThird1Example :: String
  getThird1Example = getThird1 @String ("One" :* "Two" :* "Three" :* "Four" :* ())
  * Couldn't match type ‘'False’ with ‘'True’

above is cumbersome to implement
- new type class; original class just calls new one: just to match on a boolean

so:

------------------------------------------------------------------------------
TYPE-LEVEL CONDITIONALS

> type family If (b :: Bool) (c :: k) (d :: k) :: k where
>   If 'True  c _ = c
>   If 'False _ d = d

above is polykinded : polymorphism of type-level constructs

    If :: forall k. Bool -> k -> k -> k

need class to pattern match on type-level boolean and decide what to do at run time
- as opposed to type families, which only live at compile time.

first attempt:

> class    IsBool0 b      where
>   _If0 :: forall r. r -> r -> r
> instance IsBool0 'True  where
>   _If0 x _ = x
> instance IsBool0 'False where
>   _If0 _ y = y

b does not appear in type of _If0
- so b cannot be inferred from context where _If0 is applied
- AllowAmbiguousTypes extension says to accept definitions: ambiguity resolved at use sites
  with TypeApplications

> _If0Examples :: [String]
> _If0Examples =
>   [ _If0 @'True         "This" "That" -- "This"
>   , _If0 @'False        "This" "That" -- "That"
>   , _If0 @(Int == Bool) "This" "That" -- "That"
>   ]

but trouble implementing Get

instance (???) => Get a (b :* bs) where
  get :: b :* bs -> a
  get (y :* ys) =
    _If @(a == b)
      y         -- When (a == b) ~ 'True,  i.e., a  ~ b
      (get ys)  -- When (a == b) ~ 'False, i.e., a /~ b

two branches of _If make assumptions that the type checker (currently) rejects
- True branch : return y :: b when expected type is a : assumes a equal to b
- False branch: call get tail: requires a 'Get a bs' constraint
  - could add to context : (???)
  - implies traversal of whole type list
  - behavior of previous impl stopped on match

type of _If doesn’t encode the condition that the boolean is True in one branch,
and False in the other, so fix it:

> class IsBool b where
>   _If :: forall r
>       .  (('True  ~ b) => r)
>       -> (('False ~ b) => r)
>       -> r

RankNTypes : enables quantifiers/constraints in type signature in more locations than just beginning

Thus, two args now parameterized by constraints
- can only use 1st arg if b is True, etc
- those args can use the constraints to deduce useful facts locally

> instance IsBool 'True where
>   _If :: forall r. r -> (('False ~ 'True) => r) -> r
>   _If x _ = x
>
> instance IsBool 'False where
>   _If :: forall r. (('True ~ 'False) => r) -> r -> r
>   _If _ y = y

now complete the (???)

_If requires an IsBool instance

use If type family to construct context which depends on value of (a == b)

define type synonym If' to collapse IsBool and If constraints together

> type If' b c d = (IsBool b, If b c d)
>
> class                                       Get a       bs  where
>   get :: bs -> a
>
> instance If' (a == b) (a ~ b) (Get a bs) => Get a (b :* bs) where
>   get :: b :* bs -> a
>   get (y :* ys) =
>     _If @(a == b)
>       y         -- (If 'True  (a ~ b)         _) becomes (a ~ b)
>       (get ys)  -- (If 'False      _ (Get a bs)) becomes (Get a bs)

------------------------------------------------------------------------------
DEPENDENTLY TYPED PROGRAMMING WITH SINGLETONS

singleton : type with single inhabitant

IsBool is Church encoding of Bool singleton

param b of SBool below is tied to constructor
- STrue  is the unique inhabitant of SBool 'True
- SFalse is the unique inhabitant of SBool 'False

use-case: pattern matching on SBool b to refine value of b in each branch
- similar to _If

> data SBool (b :: Bool) where
>   STrue  :: SBool 'True
>   SFalse :: SBool 'False

isomorphism between SBool and IsBool instances
- witness by desugaring IsBool instances to the polymorphic functions that they contain:

> -- Desugar IsBool by unwrapping _If
> type IsBool_ b
>   =  forall r
>   .  (('True  ~ b) => r)
>   -> (('False ~ b) => r)
>   -> r
>
> {-# ANN _SBoolTosBool "HLint: ignore Use const" #-}
> _SBoolTosBool :: forall b. SBool b -> IsBool_ b
> _SBoolTosBool sb = case sb of
>   STrue  -> \x _ -> x
>   SFalse -> \_ y -> y
>
> _IsBoolToSBool :: forall b. IsBool_ b -> SBool b
> _IsBoolToSBool _If = _If STrue SFalse

_IsBoolToSBool . _SBoolTosBool = id = _SBoolTosBool . _IsBoolToSBool

"Church encoding" claim made more explicit with the following reformulation of SBool.
- shows another characteristic of GADTs:
  - constructors can be given arbitrary function types,
    especially polymorphic functions with constraints,
    as long as result matches type being defined.
  - the function constraints and arguments become constructor fields.

> data SBool' (b :: Bool) where
>   STrue'  :: ('True  ~ b) => SBool' b
>   SFalse' :: ('False ~ b) => SBool' b

compare to Either

> data Either' a b where
>   Left'  :: a -> Either' a b
>   Right' :: b -> Either' a b

and its Church encoding

> type Either_ a b = forall r. (a -> r) -> (b -> r) -> r

