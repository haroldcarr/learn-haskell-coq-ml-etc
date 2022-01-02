{-# OPTIONS --type-in-type   #-} -- NOT SOUND!

module Tile where

--open import Category.Applicative   using (RawApplicative)
open import Level
--open        RawApplicative {{...}} using (pure; _⊗_)
open import Function

{-
Initial Encoding

By construction, every algebra gives rise to an “obvious” implementation called its initial encoding.
Initial encodings called “interpreter pattern" in object-oriented programming.
Initial encoding is a name for implementing each of our algebra’s constructors as a data constructor directly in the implementation language.

Build an explicit data structure representing a tile.
Later, “interpret” it — that is, rasterize it
An initial encoding is characterized by creating a tree of data whose nodes correspond precisely with the algebraic constructors.
Might refer to “trees” and “nodes” in the following section, but these refer only to terms and constructors, respectively.

pick core set of “primitive” operations in algebra (by trial and error)
use laws to write other combinators in terms of the primitives
-}

data Tile : Set -> Set where
  Cw     : ∀ {a   : Set} ->                  Tile a -> Tile a
  FlipH  : ∀ {a   : Set} ->                  Tile a -> Tile a
  Above  : ∀ {a   : Set} -> Tile  a       -> Tile a -> Tile a
  Pure   : ∀ {a   : Set} ->                       a -> Tile a
  Ap     : ∀ {a b : Set} -> Tile (a -> b) -> Tile a -> Tile b

cw : {a : Set} -> Tile a -> Tile a
cw (Cw (Cw (Cw t))) = t
cw             t    = Cw t

flipH : {a : Set} -> Tile a -> Tile a
flipH (FlipH t) = t
flipH        t  = FlipH t

above : ∀ {a : Set} -> Tile a -> Tile a -> Tile a
above = Above

pure : ∀ {a : Set} -> a -> Tile a
pure = Pure

ap : ∀ {a b : Set} -> Tile (a -> b) -> Tile a -> Tile b
ap = Ap

{-

instance Applicative Tile where
  pure = Pure
  (<*>) = Ap

find derivations of non-primitive constructors in terms of above primitives.

derivation of ccw:

  ccw t
= (via cw/cw/cw/cw)
  ccw (cw (cw (cw (cw t))))
= (via ccw/cw)
  cw (cw (cw t)))

maps directly to an implementation:
-}

ccw : {a : Set} -> Tile a -> Tile a
ccw = cw ∘ cw ∘ cw

{-
careful in use of primitive forms.
e.g., implement ccw here in terms of the constructor cw rather than primitive Cw.
to ensure that by-fiat laws get a chance to simplify the expression.

flipV:

∀ (t :: Tile).
  flipV t
= (via rotated flipH)
  ccw (flipH (cw t))
= (via cw/cw/cw/cw)
  ccw (cw (cw (cw (cw (flipH (cw t))))))
= (via ccw/cw)
  cw (cw (cw (flipH (cw t))))

tip : do derivations only using constructors already implemented
-}

flipV : {a : Set} -> Tile a -> Tile a
flipV = ccw ∘ flipH ∘ cw

{-
  beside t1 t2
= (via above)
  ccw (above (cw t1) (cw t2))
-}

beside : {a : Set} -> Tile a -> Tile a -> Tile a
beside t1 t2 = ccw (above (cw t1) (cw t2))

{-
  quad t1 t2 t3 t4
= (via quad)
  above (beside t1 t2) (beside t3 t4)
-}

quad : {a : Set} -> Tile a -> Tile a -> Tile a -> Tile a -> Tile a
quad t1 t2 t3 t4 = above (beside t1 t2) (beside t3 t4)

{-
  swirl t
= (via swirl)
  quad t (cw t) (ccw t) (cw (cw t))
-}

swirl : {a : Set} ->  Tile a -> Tile a
swirl t = quad t (cw t) (ccw t) $ cw $ cw t

{-
applicative laws:

  fmap f t
= (via pure/ap)
  pure f <*> t

instance Functor Tile where
  fmap f t = pure f <*> t

Finally, empty and behind turn out to be specific cases of the applicative operations when generalized to monoids:

empty :: Monoid a => Tile a
empty = pure mempty

behind :: Monoid a => Tile a -> Tile a -> Tile a
behind = flip (liftA2 (<>))

above is lawful set of implementations for initial encoding.

Now need code that generates an image — OBSERVATIONS
by giving an interpreter for the Tile syntax tree.

laws suggest another implementation, one which operates over two-dimensional lists.

We proceed by implementing rasterize :: Int -> Int -> Tile a -> [[a]] as a piece-wise fashion, pattern matching on each data constructor of Tile. The Pure case is simplest; it
corresponds to a pixel matrix with a constant value in every cell. Recalling that the result of rasterize should be row-major, we can construct a row with the right width of
pixels via replicate, and then replicate that to get the correct height.

rasterize w h (Pure a) = replicate h $ replicate w a

When designing the algebra, we took great care also to provide a rasterize' observation which is equivalent to rasterize, but which is in a form more amenable to applicative
homomorphisms. We can exploit that machinery now in order to implement Ap:

rasterize w h (Ap f a) =
  coerce (rasterize' w h f <*> rasterize' w h a)

Horizontally flipping an image is also easy; we simply rasterize the underlying tile and then reverse each of its rows:

rasterize w h (FlipH t) = fmap reverse $ rasterize w h t

Rotation of a tile is a little trickier. Intuition tells us that rotating a non-square matrix will swap the width and height dimensions, and since we would like the result of cw
to have the specified dimensions, we must swap the width and height that we give when rasterizing the inner tile:

rasterize w h (Cw t) = rotate2d $ rasterize h w t
  where
    rotate2d = fmap reverse . transpose

Our only remaining primitive to interpret is Above, which conceptually builds two half-height tiles and glues them vertically. This definition again maps directly to an
implementation:

rasterize w h (Above t1 t2) =
    rasterize w (div h 2) t1 <>
    rasterize w (h - div h 2) t2  ❶

At 1 we use h - div h 2 as the second width in case the desired height is odd — we wouldn’t want to drop a pixel accidentally. And just like that, we have plucked a fully
working implementation seemingly out of thin air.

Finally, let’s give some instances for Tile. It admits obvious Semigroup and Monoid instances, simply by lifting an instance from the “pixel” type:

instance Semigroup a => Semigroup (Tile a) where
  (<>) = liftA2 (<>)
instance Monoid a => Monoid (Tile a) where
  mempty = pure mempty

Additionally, we’ll need a Show instance in order for QuickCheck to test our properties later. This instance unfortunately can’t be automatically derived due to the existential
type in Ap, but we can write it by hand without much effort:

instance Show a => Show (Tile a) where
  show (Cw t) = "cw (" ++ show t ++ ")"
  show (FlipH t) = "flipH (" ++ show t ++ ")"
  show (Above t1 t2)
    = "above (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (Pure a) = "pure (" ++ show a ++ ")"
  show (Ap _ _) = "ap _ _"  ❶

There is no Show instance for the existentially quantified type in Ap, so we ignore those two arguments when printing ap terms at 1. This isn’t a perfect solution by any means,
but it’s practical and gets the job done.

Generating Tests

In the previous section, we followed our algebra’s rules to derive an implementation automatically. The resulting code, while inefficient and naive, is overwhelmingly simple and
is guaranteed to follow our specification. The next step is to ensure that the specification corresponds with our intuition, as the specification is only useful insofar as it
helps us solve problems we are interested in.

Here again, we can automate a great deal of the tedium involved in checking the implementation. Most systems are verified using hand-written unit tests: little checks that the
software behaves predictably in particular scenarios. Unit tests are a good start but suffer from the problem that they are boring and written by humans. To quote Hughes (2016):

 Imagine writing a suite of unit tests for software with, say, nn different features. Probably you will write 3-4 test cases per feature. This is perfectly manageable — it’s
 a linear amount of work. But, we all know you will not find all of your bugs that way, because some bugs can only be triggered by a pair of features interacting. Now, you
 could go on to write test cases for every pair of features — but this is a quadratic amount of work, which is much less appealing.

Claessen and Hughes (2000) present property tests as an alternative to unit tests. Property tests can be thought of as templates for generating unit tests; by specifying exactly
how input and output should be related, a property testing system can create randomly generated inputs and ensure that the property always holds. If we generate ten thousand
random inputs, and the property holds for each one, we should be pretty confident that our code is working as intended. Of course, equality is undecidable in general, but after
ten thousand tests, if the two haven’t been shown yet to be unequal, they probably never will.

Property testing often requires a better understanding of the software under test, as its authors must be able to describe classes of correctness, rather than instances of
correctness. However, this additional effort is well rewarded; property tests can be used to stamp out an arbitrary number of unit tests, driving our confidence of the system
asymptotically up to 100%.

But in this section, we will take the automation ladder one rung higher, and automatically generate our property tests. How is such a thing possible? By giving a reference
implementation and a description of the constructors of our algebra to QuickSpec (Smallbone et al. (2017)) — a theorem searching program — it can simply try every possible term,
and use property testing to see which ones are equal.

By treating the matching terms found by QuickSpec as new equations of our algebra, we are, in essence, discovering property tests that must be true of any correct
implementation. The result? An automatically generated suite of regression tests.

We will present just enough of QuickCheck and QuickSpec in this section to get our work done, but each is given a more robust treatment in chapter 4.1 and chapter 4.2,
respectively.

Our first step is to write a generator for tiles, which is a way of creating random tiles. Generators run in the Gen monad provided by the QuickCheck library, and come with a
few primitive actions for picking elements at random. Generators in QuickCheck are usually given via an Arbitrary instance, which defines an arbitrary function used to produce
random values.

instance (CoArbitrary a, Arbitrary a)
      => Arbitrary (Tile a) where
  arbitrary = sized $ \n ->  ❶
    case n <= 1 of
      True -> pure <$> arbitrary  ❷
      False -> frequency  ❸
        [ (3,) $ pure <$> arbitrary  ❹
        , (9,) $ beside <$> decayArbitrary 2  ❺
                        <*> decayArbitrary 2
        , (9,) $ above <$> decayArbitrary 2
                       <*> decayArbitrary 2
        , (2,) $ cw <$> arbitrary
        , (2,) $ ccw <$> arbitrary
        , (4,) $ flipV <$> arbitrary
        , (4,) $ flipH <$> arbitrary
        , (6,) $ swirl <$> decayArbitrary 4
        , (3,) $ quad <$> decayArbitrary 4
                      <*> decayArbitrary 4
                      <*> decayArbitrary 4
                      <*> decayArbitrary 4
        , (2,) $ (<*>)
              <$> decayArbitrary @(Tile (a -> a)) 2
              <*> decayArbitrary 2
        ]
decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary

Every generator has access to an implicit size parameter provided by the testing engine, which roughly corresponds to how complicated the generated term should be. At 1 we get
access to the size parameter via the sized function. If the size is less than or equal to one, we simply return a pure tile whose color is itself arbitrary. This check ensures
that our arbitrary tile eventually terminates.

At 3, we use the frequency combinator to assign random weights to the different possibilities of Tile constructors. In 4 we also build an arbitrary pure colored tile, with a
weight of 3. At 5 however, we give a weight of 9 — meaning three times more likely — to the beside constructor, because it results in more complex tiles. Rather than filling in
the parameters of beside with arbitrary tiles directly, we instead use the decayArbitrary 2 combinator, which asks for a tile that is half as complicated as the one we are being
asked to generate. In this way, we are “splitting our complexity budget” between the two sub-tiles. Due to polymorphism, the call to arbitrary at 5 creates a Tile a, but at 4 it
creates just a bare a.

Notably missing from the definition of arbitrary are the behind and empty constructors. This is a technical limitation; those constructors require a to be a monoid, but nothing
else in the algebra does. If we included them in the list, we’d only generate tiles of monoids, which is somewhat annoying for testing purposes. We console ourselves with the
understanding that empty and behind are specializations of pure and (<*>) respectively, both of which get generated by this instance. In a real codebase, you’d probably want to
do some trickery to allow both instances to exist, but such a thing is out of this book’s scope.

The remainder of the instance carries on in this way, listing the constructors with relative weights, and building them out of arbitrary smaller pieces. There is nothing of real
interest here; every generator you write should have the same shape, containing:

1 a check of the size parameter, terminating in a simple constructor if required.
2 a list of every constructor of the algebra, each lifted into the Gen monad, with arbitrary children.

It’s extremely important that your generator use the algebra’s constructors, and not the data constructors (that is, it should use cw instead of Cw.) Recall that our algebraic
constructors possibly contain pattern matching that implements by-fiat laws; thus, building terms out of the data constructors directly is likely to break your invariants.

As it happens, this Arbitrary instance (and one for Color, elided here) is all that’s required for us to generate random tiles — some samples of which are given in figures
39-41.

 Figure 39: Random Tile 1
 Figure 40: Random Tile 2
 Figure 41: Random Tile 3

Our next course of action is to teach the testing program about what equality means. Recall that we are explicitly not using whatever notion of equality Haskell gives us,
instead choosing to reason via obs eq, reproduced here:

∀ (t1 :: Tile) (t2 :: Tile).
  (∀ (w :: Int) (h :: Int).
    rasterize w h t1 == rasterize w h t2) => t1 = t2

This law states that two tiles t1 and t2 are equal if and only if they rasterize to the same pixels for every imaginable width and height. Regardless of the actual memory layout
for our tiles, this is the metric by which we’d like to consider two tiles equal. This notion can be encoded by giving an instance of the Observe typeclass from the quickspec
library.

Observe allows us to describe observational equality between terms — possibly requiring quantified arguments. As an interface, it requires us to fill in one function, observe,
which has a parameter for the quantified arguments, and another for the term we’d like to observe. These arguments are generated randomly via the property testing Arbitrary
machinery, and two terms require observed equality for every input thrown at them.

We’d like to write an instance that looks like this:

instance Observe
            (Small Int, Small Int)  ❶
            [[a]]  ❷
            (Tile a) ❸
            where
  observe (Small w, Small h) t
    = rasterize (max 1 w) (max 1 h) t  ❹

This says that given two small integers (1) corresponding to the width and height, we can observe a Tile a value (3) as a [[a]] (2). We implement the observe function as
rasterize at 4, ensuring that the width and height are both at least one.

While this instance is what we want conceptually, it doesn’t work as written. The issue is that pesky universally quantified type a; we have no guarantees that it can be
observed. The fix is a little mind-bending: we must introduce an Observe constraint for a, and observe our matrix of pixels in terms of the observation on a. The working
instance is listed below, but it’s important to keep in mind that this is an implementation detail, not a moral one.

instance Observe test outcome [[a]]
      => Observe
            (Small Int, Small Int, test)
            outcome
            (Tile a) where
  observe (Small w, Small h, x) t
    = observe x (rasterize (max 1 w) (max 1 h) t)

Our new Observe instance gives us access to the (=~=) operator, which creates a property test showing observational equality between two terms. For example, we can now test
cw/cw/cw/cw experimentally:

> quickCheck $ cw @Bool . cw . cw . cw =~= id
+++ OK, passed 100 tests.

We need to specify @Bool to tell Haskell which sort of tiles to generate — left to its own devices it will pick the single-valued type () which is always equal to itself, and
thus particularly unhelpful for testing equality. The quickCheck function then generated 100 different random tiles and checked to see that each was observationally equal to
rotating the tile four times clockwise. In essence, it created one hundred unique unit tests, and our implementation passed each!

Compare the amount of code we wrote in this section to the amount of code we would have needed to write to create one hundred unit tests. When you take into account that the
number one hundred is arbitrary and could just as easily have been ten thousand (use quickCheckWith stdArgs {maxSuccess = 10000} instead), perhaps the power of this approach
becomes more apparent to you.

More amazingly, this is just the tip of the iceberg. While we certainly could go and write property tests for every law we discovered in chapter 2.1, it feels like a wasted
effort. After all, every one should pass, since our implementation is derived from those same laws. Of course, just because they should pass doesn’t mean they will, and we must
still exercise prudence. But rather than write these tests ourselves, let’s get the computer to do it for us.

Here is where the quickspec library comes in. We need only to write a signature of our algebra, describing what constructors and types are available, and it will do the rest. By
enumerating every possible well-typed expression up to a maximum size and comparing them observationally to one another, QuickSpec will find every law that holds for our
implementation. We can then look over the results, ensuring ours are present. If everything looks good, QuickSpec has an option which emits valid Haskell property tests for
each.

Several combinators exist for building QuickSpec signatures, with the most important of them being con :: String -> a -> Sig. This signature is used to describing constructors.
I like to list my constructors together in one big signature:

sig_cons :: Sig
sig_cons = signature
  [ con "cw"     $ cw     @A  ❶
  , con "ccw"    $ ccw    @A
  , con "beside" $ beside @A
  , con "above"  $ above  @A
  , con "flipV"  $ flipV  @A
  , con "flipH"  $ flipH  @A
  , con "pure"   $ pure   @Tile @A
  , con "<*>"    $ (<*>)  @Tile @A @B
  , con "quad"   $ quad   @A
  , con "swirl"  $ swirl  @A
  , con "behind" $ liftC @(Monoid A) $ behind @A  ❷
  , con "empty"  $ liftC @(Monoid A) $ empty  @A
  ]

At 1, we use the @A notation to let QuickSpec know that the cw function is polymorphic in its argument. The exact details aren’t important right now but are described thoroughly
in chapter 4.2. Additionally, at 2, we use the liftC @(Monoid A) combinator to inform QuickSpec that behind is only available if there is a Monoid instance available. Don’t
worry; if you get it wrong, the compiler will yell at you. Unfortunately, the path forward in such cases isn’t always as straightforward as might be desired.

Additionally, there are some latent constructors we should teach QuickSpec about. Although our algebra doesn’t deal directly with monoids, they are present “in the background” —
for example, with empty and behind. The monoid operations aren’t directly applicable to our algebra, but we’d like to know about any “interesting” laws mentioning them. By using
the background signature, we can “hide” constructors:

sig_bg :: Sig
sig_bg = background
  [ con "<>"     $ liftC @(Monoid A) $ (<>)   @A
  , con "mempty" $ liftC @(Monoid A) $ mempty @A
  ]

By wrapping the contents of sig_bg inside background, it means QuickSpec will still tell us about monoidal laws for our types — so long as they’re not true for all monoids!

Finally, we must teach QuickSpec about the relevant types in our algebra:

sig_types :: forall m. (m ~ [Word8]) => Sig
sig_types = signature
  [ mono        @m  ❶
  , monoObserve @(Tile m)  ❷
  , monoObserve @(Tile (m -> m))
  , instanceOf  @(Monoid m)  ❸
  , instanceOf  @(Monoid (Tile m))
  , vars ["t"]  $ Proxy @(Tile A)  ❹
  , vars ["tf"] $ Proxy @(Tile (A -> B))
  , defaultTo $ Proxy @m  ❺
  , withMaxTermSize 5
  ]

We use the mono combinator at 1 to inform QuickSpec of a type that can use everyday Haskell equality. Similarly, the monoObserve combinator at 2 is used for types that require
observational equality. At 3, instanceOf is used to declare the presence of a Monoid instance. The vars combinator gives hints for how to name variables of the specified type,
like at 4. Finally, at 5, we ask QuickSpec to instantiate all polymorphic type variables at [Word8].

Why [Word8]? Because I’ve coyly been avoiding discussing colors, as getting them right is tricky and not particularly enlightening. As such, we need some monoid, and any monoid
would do. But Word8 has some desirable properties: 256 values is small enough that we can expect any particular number to be hit, but large enough that the collisions should be
relatively rare. Hitting every value (being surjective) is vital to ensure there are no edge cases, but having a high number of collisions might accidentally show equalities
where there are none. As an extreme example of this last case, imagine what would happen if we used a type that had only one value.

So that explains why we use Word8. By why a list? Simply to introduce a monoid.

Signatures themselves form a monoid, so we can build an aggregate signature out of these smaller pieces:

sig :: Sig
sig = sig_bg <> sig_cons <> sig_types

We can now ask QuickSpec to discover all of the laws of our algebra. The following command might take upwards of ten minutes to complete, so now would be a good time for a
coffee break. Remember, the goal is not yet to be fast; we are satisfied temporarily with mere correctness.

Without further ado:

> quickSpec sig
  1. empty = mempty
  2. pure mempty = mempty
  3. ccw mempty = mempty
  4. cw mempty = mempty
  5. flipH mempty = mempty
  6. flipV mempty = mempty
  7. swirl mempty = mempty
  8. behind t t2 = t2 <> t
  9. above mempty mempty = mempty
 10. beside mempty mempty = mempty
 11. ccw (pure x) = pure x
 12. ccw (cw t) = t
 13. cw (pure x) = pure x
 14. cw (ccw t) = t
 15. cw (cw t) = ccw (ccw t)
 16. cw (flipH t) = ccw (flipV t)
 17. flipH (pure x) = pure x
 18. flipH (ccw t) = ccw (flipV t)
 19. flipH (flipH t) = t
 20. swirl (pure x) = pure x
 21. above mempty (cw t) = cw (beside mempty t)
 22. above mempty (flipH t) = flipH (above mempty t)
 23. above (cw t) mempty = cw (beside t mempty)
 24. above (flipH t) mempty = flipH (above t mempty)
 25. ccw t <*> mempty = ccw (t <*> mempty)
 26. flipH t <*> mempty = flipH (t <*> mempty)
 27. swirl t <*> mempty = swirl (t <*> mempty)
 28. flipH (swirl (ccw t)) = cw (swirl (flipH t))
 29. quad mempty mempty mempty mempty = mempty
 30. pure x <> pure y = pure (x <> y)
 31. ccw t <> ccw t2 = ccw (t <> t2)
 32. flipH t <> flipH t2 = flipH (t <> t2)
 33. flipV t <> flipV t2 = flipV (t <> t2)
 34. swirl t <> swirl t2 = swirl (t <> t2)
 35. above mempty (beside t t2) = quad mempty mempty t t2
 36. above (pure x) (pure x) = pure x
 37. above (cw t) (cw t2) = cw (beside t t2)
 38. above (flipH t) (flipH t2) = flipH (above t t2)
 39. above (beside t t2) mempty = quad t t2 mempty mempty
 40. beside mempty (above t t2) = quad mempty t mempty t2
 41. beside (pure x) (pure x) = pure x
 42. beside (flipV t) (flipV t2) = flipV (beside t t2)
 43. beside (above t t2) mempty = quad t mempty t2 mempty
 44. ccw t <*> ccw t2 = ccw (t <*> t2)
 45. flipH t <*> flipH t2 = flipH (t <*> t2)
 46. flipV t <*> flipV t2 = flipV (t <*> t2)
 47. swirl t <*> swirl t2 = swirl (t <*> t2)

The laws presented here are the ones which QuickSpec couldn’t falsify. It’s uncomputable in general to determine if two computer programs are equivalent, but we expect programs
that are significantly different to diverge quickly. Thus QuickSpec has pruned any equations that diverged in a finite number of tests, and these are the ones that remain. It’s
possible that throwing more CPU at them might find a counterexample, but we’re still in the discovery phase right now, and we can manually test any dubious properties by hand.

As you can see, QuickSpec has done a top-notch job, and found nearly fifty different properties that hold for our algebra. While many of them are old friends that we discovered
in chapter 2.1, many others are new to us — though obvious in retrospect. For example, we hadn’t spotted the vast swathe of monoid homomorphisms like laws 30, 31, 32, 33 and 34.
Curiously missing from this list is a monoid homomorphism for cw — does such a thing not hold? QuickSpec tries to be clever and hide laws which can be derived from others, so
its absence from this list might not be evidence of a bug. We can find out by writing a quick property test:

> quickCheck $ \t1 t2 -> cw @[Word8] t1 <> cw t2 =~= cw (t1 <> t2)
+++ OK, passed 100 tests.

Looks good. I’m not sure why QuickSpec doesn’t mention this law, which is a good reminder that we haven’t entirely automated away the engineer’s job! Besides, this approach of
using a property test to verify a hypothesis about our codebase is a powerful tool. Property tests need not necessarily be tests per se; we can also use them in a scientific
context to learn things!

In this case, we got lucky. Often QuickSpec will emit what appear to be crazy, nonsensical laws. More often than not, such behavior is indicative of either a faulty generator or
a buggy implementation. Chapter 4.2 discusses these issues and their fixes in more detail.

Now that we are happy with our laws, we can freeze the semantics of our implementation, immortalizing its equations in executable property tests. Remember that QuickCheck and
QuickSpec are inherently stochastic, working by Monte Carlo methods to approximate answers to uncomputable questions. This is an important thing to keep in mind when generating
property tests — it would be exceedingly disorienting if QuickSpec got lucky with all of its random numbers and emitted an invalid law. We’d be tearing our hair out trying to
fix a “bug” that never existed in the first place! This imagery is a stark reminder to set an exceptionally high bar of confidence for QuickSpec when generating tests.

By setting the withMaxTests signature to a big number (I like 1,000,000), we can ask QuickSpec for extra diligence when testing its laws. We can also set withMaxTestSize to
increase the complexity of the generated arguments it tests. Both of these options have the potential to increase dramatically QuickSpec’s running time, so I like to set them to
big numbers and then go home for the day; if anyone complains, tell them you’ll have thousands of unit tests written for them by tomorrow.

To illustrate this functionality in QuickSpec, I will also use the withMaxTermSize signature1 to significantly reduce the size of expressions that QuickSpec generates; there is
not much value in reading through the code for automatically generated property tests. When doing this for real, you should use a value somewhere between 7 and 9.

> quickSpec $ sig <> withPrintStyle ForQuickCheck <> withMaxTests 1000000 <> withMaxTestSize 20 <> withMaxTermSize 2
quickspec_laws :: [(String, Property)]
quickspec_laws =
  [ ( "empty = mempty"
    , property $ (empty :: Tile [Word8]) =~= mempty)
  , ( "pure mempty = mempty"
    , property $
            (pure mempty :: Tile [Word8]) =~= mempty)
  , ( "ccw mempty = mempty"
    , property $
            (ccw mempty :: Tile [Word8]) =~= mempty)
  , ( "cw mempty = mempty"
    , property $ (cw mempty :: Tile [Word8]) =~= mempty)
  , ( "flipH mempty = mempty"
    , property $
            (flipH mempty :: Tile [Word8]) =~= mempty)
  , ( "flipV mempty = mempty"
    , property $
            (flipV mempty :: Tile [Word8]) =~= mempty)
  , ( "swirl mempty = mempty"
    , property $
            (swirl mempty :: Tile [Word8]) =~= mempty)
  ]

The resulting laws_quickspec can be pasted into a test module, and executed via:

runTests :: IO ()
runTests
  = traverse_
      (quickCheck . uncurry counterexample)
      laws_quickspec

As an added precaution, it’s good form to ensure that your new tests are green before continuing.

> runTests

Looks good!

To reiterate, these tests we’ve generated correspond to all the observable semantics of our implementation. Because our implementation is extremely simple and derived directly
from the design work we did, it is exceedingly likely correct. In aggregate, these two facts mean that any other implementation which passes the tests must also be correct. We
will turn our attention to a better implementation in the next section.

An Efficient Implementation

With our semantics now perfected and nailed down, we have successfully described what we’re trying to build. And now for the fun part of the problem — actually making it! Of
course, our initial encoding is indeed an implementation, and many times that’s good enough. Not every issue needs a fast, creative solution. The abstraction boundary we’ve set
up — our public interface, its semantics and the regression tests witnessing those semantics — is completely airtight. We can thus swap out the implementation at any point in
time, and our users would be none the wiser, except perhaps in that their programs’ performance improved.

This is a very different state of affairs than what we usually see in software engineering. Because the library which perfectly hides its implementation is very rare indeed,
implementation details are often accidentally interwoven with semantics. The maintenance programmer is unable to make sweeping architectural changes, as it would be a breaking
change. As a result, programmers are taught from their early education to be hyper-aware of their programs’ performance; they probably won’t get a chance to improve the
asymptotics later on.

Our abstraction barrier makes these performance concerns non-issues. We can do something stupid at first, and swap it out wholesale later if it turns out to be too slow. Maybe
our component will need to be changed later, but because the new implementation must be indistinguishable from the last, this happens to be an amortized constant amount of work.
Call sites don’t need to be updated, and there is a good chance that your first implementation is sufficient anyway.

Let’s turn these concerns aside and assume that improving our tile library’s performance is time well spent. Can Algebra-Driven Design help us? In general, yes, but no longer
can it do all of the work for us. Instead, the value of Algebra-Driven Design becomes helping to ask the right questions, and allowing our human intuition to take the reins.

The messiest part by far of our design are the laws around rasterize. Recall that our rule of thumb is “messy things are where the problems lie”, so this seems like a good place
to focus our attention. Each rasterize law performs some extremely list-specific operations, many of which are hard to motivate other than “they seem to work.”

Of particular concern is the asymmetry between the rows and columns. While our algebra treats the horizontal and vertical directions completely equivalently, our implementation
does not. Implementing a matrix as a list-of-lists requires us to choose which list means what. We arbitrarily chose a row-major ordering, but in making that choice, have
resigned rasterize/beside to look significantly different than rasterize/above. Something is fishy here. It’s not wrong per se, merely inelegant.

But the real smoking gun in our specification of rasterize is in the applicative homomorphism rasterize/ap. To be more specific, that rasterize/pure isn’t also an applicative
homomorphism. Let’s see why not, and then investigate what that means.

We chose the ZipList applicative for our rasterize' function because it combines two lists element-wise, rather than taking the usual Cartesian-product applicative instance for
lists. Chapter 4.3.2.6 discusses the difference between these two applicatives in more detail, and readers unfamiliar with zip lists are encouraged to read that section before
continuing here.

Even though rasterize/ap is an applicative morphism, its partner rasterize/pure isn’t. This makes no sense semantically; an applicative is made up of both (<*>) and pure; they
don’t come individually wrapped. The two are defined relative to one another, and if we are missing one of these homomorphisms while the other holds, it is merely by accident.
Which raises the question: is ours only by accident?

Resoundingly, the answer is no! When you look at rasterize/pure, morally, it is attempting to to fill all of space with a single value. This fact is hindered by our choice of
trying to generate finite, nested lists.

All of this analysis is characteristic of our having used the wrong observation. Whatever this thing we’re trying to build is, it’s too specific. That is to say, rasterize might
be the right thing to implement, but it is the wrong thing to reason about. Instead, we should be looking for some other observation, one from which we can define rasterize
itself.

Our missing rasterize/pure homomorphism is the clue we need. Whatever this observation is, it should fill infinite space, zip elements together pairwise, and as a bonus, treat
the horizontal and vertical directions symmetrically. Being familiar with the standard canon of algebraic structures comes in handy here, because the algebra of functions
themselves has the exact properties we’re looking for!

Imagine an alternative universe in which we implemented Tile as:

type Tile a = Point -> a

For our purposes, we will say that a Point is defined as:

type Point = (Double, Double)

although you can imagine what sorts of exciting things might happen if we were to play around with this — perhaps adding extra dimensions, or parameterizing the entire type. We
will not further explore this line of inquiry , but the curious reader is encouraged to do so.

We can write out the applicative instance for functions, just to familiarize ourselves and to get some intuition behind why this is the instance we’re looking for. First, pure:

pure :: a -> (Point -> a)
pure a = \_ -> a  ❶

The implementation of pure for functions takes the value a that we’d like, and then creates a function which ignores its argument and always gives back a. This is analogous to
the zip list equivalent, which created an infinitely long list of the same value. Here instead, we are creating a function that “fills” its entire domain with the same value.
Let’s now look at (<*>):

(<*>)
    :: (Point -> (a -> b))
    -> (Point -> a)
    -> (Point -> b)
ff <*> fa = \p -> (ff p) (fa p)

Here we have two functions, the first being a function from a Point to another function — this time from a to b. Our other function is one from Point to a. Our goal is to
produce a function from points to b! The solution is simple: we apply the Point to both of our functions, resulting in an a -> b and an a. All that’s left is to apply them.

The result of this odd (<*>) for functions is similar to zip lists. In essence, it combines a function at a given point in space with a value at the same point in a different
space. This, too, is performed “element-wise.”

Exercise

 Prove ap/pure/id is satisfied by the definitions of pure and (<*>) given here.

Let’s assume that our one true observation, that is to say, the denotation of a Tile is given by sample:

sample :: Point -> Tile a -> a

This observation, unlike rasterize, is continuous in its domain, and allows for infinitely2 sized tiles. Of course, at some point we still need to be able to rasterize it to be
viewed on a screen, but often working with continuous functions is much more elegant than their discrete counterparts.

We will designate by convention the area (-1, -1) and (1, 1) to be the surface of the Tile that will be rasterized. While a tile might be infinite, this is the only area that we
will pay attention to, and for which we will care about the semantics. Let’s re-specify our algebraic constructors in terms of sample, and see what is simplified. To flip a
tile, we now negate the x-coordinate that is being sampled:

∀ (t :: Tile a) (x :: Double) (y :: Double).
  sample (x, y) (flipH t) =
    sample (negate x, y) t

The flipV is completely analogous:

∀ (t :: Tile a) (x :: Double) (y :: Double).
  sample (x, y) (flipV t) =
    sample (x, negate y) t

This is already a marked improvement over rasterize/flipH and rasterize/flipV, which required asymmetric machinery to target which list was supposed to be reversed. By contrast,
sample/flipH and sample/flipV are exact counterparts.

If we decide (by convention) that a negative x-coordinate corresponds to the left, and a negative y-coordinate to up, then cw can also be specified elegantly:

∀ (t :: Tile a) (x :: Double) (y :: Double).
  sample (x, y) (cw t) =
    sample (y, negate x) t

Without the challenging transpose machinery that we saw in sample/cw, we can specify ccw directly, without needing to relegate to ccw:

∀ (t :: Tile a) (x :: Double) (y :: Double).
  sample (x, y) (ccw t) =
    sample (negate y, x) t

Again, this pair of laws is elegant, symmetric, and clearly shows why cw . ccw = id must be true.

We can think of beside as a spatial transformation from the coordinate space of both c1 c2 (the “source”) into the coordinate spaces of c1 and c2 (the “destinations.”) The
cut-off between the two happens tiles at x=0x = 0, therefore for x<0x < 0 we should choose c1 and for 0≤x0 \leq x choose c2. Due to the contravariance of spatial
transformations, this operation’s mathematics are rather hard to visualize mentally. Figure 42 shows the c1 case and should help.

We first need to align these spaces’ centers by adding 0.5 to the sampled xx coordinate in the source. Because the yy coordinate isn’t affected by beside, the point (−0.5,0)
(-0.5, 0) in the source should correspond with (0,0)(0, 0) in the c1 destination. Since the source is sampled at half the entire space’s width, it thus has width 1, but the
destination has width 2. Therefore we can map c1 in the source to the c1 destination through the following transformation:

f(x,y)=((x+0.5)*2,y) f(x, y) = ((x + 0.5) * 2, y)

The c2 case is identical, except that we need to subtract 0.5 instead of adding it. This analysis leads to two sample/beside laws:

 Figure 42: Transformation from the coordinate space of beside c1 c2 to the coordinate space of c1.
∀ (t1 :: Tile a) (t2 :: Tile a) (x :: Double)
      (y :: Double).
  x < 0 =>
    sample (x, y) (beside t1 t2) =
      sample ((x + 0.5) * 2, y) t1

and

∀ (t1 :: Tile a) (t2 :: Tile a) (x :: Double)
      (y :: Double).
  x >= 0 =>
    sample (x, y) (beside t1 t2) =
      sample ((x - 0.5) * 2, y) t2

We choose 0 <= x as the condition for sample/beside (right) to mirror our decision in rasterize/beside that the right-side should “pick up” the middle pixel.

As you would expect, the laws for above are again symmetrical and analogous.

Exercise

 Give sample/above (top) and sample/above (bottom).

Finally, we can show as we intentionally designed that sample has a true applicative morphism:

∀ (a :: a) (x :: Double) (y :: Double).
  sample (x, y) (pure a) = pure a

and:

∀ (tf :: Tile (a -> b)) (ta :: Tile a) (x :: Double)
      (y :: Double).
  sample (x, y) (tf <*> ta) =
    sample (x, y) tf <*> sample (x, y) ta

Very elegant.

This discovery of simpler semantics suggests an alternative implementation for tiles: we can represent them in this function form directly, and rely on Haskell to “fuse” the
functions together — optimizing any statically-known tiles at compile time. Such an optimization will not work in all languages, so domain expertise is required. Nevertheless,
in this case, we have found an optimization — at the very least, we won’t need to allocate intermediary linked lists!

Rather than introducing sample as an implementation detail in our program, we can use the sample laws directly. Because all of our observations can be derived from sample, we
conclude that sample is the abstraction. By manipulating the type of sample so that its Tile a argument is front, we can find an equivalent formulation that is more amenable for
defining a type.

  Point -> Tile a -> a
=  (via flip)
  Tile a -> Point -> a
=  (via defn of Point)
  Tile a -> (Double, Double) -> a
=  (via curry)
  Tile a -> Double -> Double -> a

With our Tile a parameter in the first position, it’s clearer to see that the implementation of a Tile is a function which takes it to a function Double -> Double -> a. That is:

sample :: Tile a -> (Double -> Double -> a)

which we can encode via a newtype:

newtype Tile a = Tile
  { sample :: Double -> Double -> a
  }
  deriving (Functor, Applicative)
    via (Compose ((->) Double) ((->) Double))

Just to confirm that we haven’t done any tricks here, we can investigate the type of sample:

> :t sample
sample :: Tile a -> Double -> Double -> a

As expected! This transformation takes some time to get used to, but is always applicable and very powerful. To reiterate, the steps are:

1 Find the denotation, that is, the elegant, homomorphic observation from which all other observations can be derived. The denotation will always be a function.
2 Manipulate the type of that observation, so the algebra’s type comes as the first parameter in the function.
3 Drop this first parameter from the type of the denotation.
4 Implement the algebra as the remaining type of the denotation.

With Tile defined as the denotation, we can now use the sample laws as direct implementations for every constructor in the algebra. For example, we can implement flipV as:

flipV :: Tile a -> Tile a
flipV t = Tile $ \x y -> sample t x (negate y)

and beside as:

beside :: Tile a -> Tile a -> Tile a
beside t1 t2 = Tile $ \x y ->
  if x < 0
    then sample t1 ((x + 0.5) * 2) y
    else sample t2 ((x - 0.5) * 2) y

Exercise

 Implement the remaining constructors of our algebra.

After implementing each constructor, we are left only to write rasterize. But such a thing is trivial. We need only divide up the tile’s renderable area into the correct number
of rows and columns, sampling each:

rasterize
    :: forall a
     . Int  -- ^ resulting width
    -> Int  -- ^ resulting heigeht
    -> Tile a
    -> [[a]]  -- ^ the resulting "pixels" in row-major order
rasterize w h t = do
  y <- [0 .. (h - 1)]
  pure $ do
    x <- [0 .. (w - 1)]
    pure $ samplePixel w h t x y

The changes in implementation here are straightforward if a little finicky. A few bugs might likely make it in during the process, but not to fear: our generated regression
tests are guaranteed to catch any observable differences between our new implementation and our last. Finally, we are left with a fast, general implementation of functional
geometry and needed to do very little work other than thinking to make it happen.

-}
