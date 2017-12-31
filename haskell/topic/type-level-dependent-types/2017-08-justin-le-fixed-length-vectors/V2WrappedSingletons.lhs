> module V2WrappedSingletons where

TypeNats provides a "primitive" interface
Sometimes it doesn’t play well with other type-level mechanisms.

Implement singletons library : unified interface for type-level programming.

Instead of
- KnownNat, Proxy, natVal, SomeNat, and someNatVal
- use singletons equivalents
- Sing, fromSing, SomeSing, and toSing:2

For singletons > 2.3
- fromSing and toSing give/take Natural
For singletons <= 2.3.1
- fromSIng and toSing give/take Integer

-- TypeNats style
natVal :: KnownNat n => p n -> Natural

-- Singletons style
sing     :: KnownNat n => Sing n
fromSing :: Sing n -> Natural       -- (for n :: Nat)

-- TypeNats style
data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)
someNatVal :: Natural -> SomeNat

-- Singletons style
data SomeSing Nat = forall n. SomeSing (Sing n)
toSing :: Natural -> SomeSing Nat

withSomeSing :: Natural -> (forall n. Sing n -> r) -> r

-- TypeNats style
sameNat :: (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Maybe (n :~: m)

-- Singletons style
-- from Data.Singletons.Decide
-- for our purposes, Decision is basically a fancy Maybe
data Decision a = Proved a | Disproved (a -> Void)
(%~) :: Sing n -> Sing m -> Decision (n :~: m)

Hopefully the above should give you a nice “key” for translating between the two styles. But here are some practical translations:

View full source
-- "explicit Sing" style

mkVec_ :: Sing n -> V.Vector a -> Maybe (Vec n a)
mkVec_ s v | V.length v == l = Just (UnsafeMkVec v)
           | otherwise       = Nothing
  where
    l = fromIntegral (fromSing s)

-- "implicit" style

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (fromSing (sing :: Sing n))

-- alternatively, re-using `mkVec_`
mkVec :: KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec = mkVec_ sing

As you can see, in singletons, we have the luxury of defining our functions in “explicit” style (where the user passes in a Sing token which reveals what length they want) or “implicit” style (where the length is inferred from the return type, requiring a KnownNat n => constraint), like we have been writing up to this point. Sing n -> and KnownNat n => really have the same power. You can think of Sing n as a token that carries around KnownNat n =>, in a way.

View full source
replicate_ :: Sing n -> a -> Vec n a
replicate_ s x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (fromSing s)

replicate :: KnownNat n => a -> Vec n a
replicate = replicate_ sing

withVec :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec v f = case toSing (fromIntegral (V.length v)) of
    SomeSing s -> f s (UnsafeMkVec v)

-- alternatively, skipping `SomeSing` altogether:

withVec' :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec' v0 f = withSomeSing (fromIntegral (V.length v0)) $ \s ->
    f s (UnsafeMkVec v0)

exactLength_ :: Sing m -> Sing n -> Vec n a -> Maybe (Vec m a)
exactLength_ sM sN v = case sM %~ sN of
    Proved Refl -> Just v
    Disproved _  -> Nothing

exactLength :: (KnownNat m, KnownNat n) => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing sing

Note that you aren’t required to implement both a replicate_ and replicate — I’m just including them here to show that both API’s (implicit and explicit) are possible. (You can always just directly use sing right away before getting started to get the Sing n that those functions use, and so skip replicate_ and other explicit variants)

One slight bit of friction comes when using libraries that work with KnownNat, like finite-typelits and the Finite type. But we can convert between the two using SNat or withKnownNat

-- SNat can be used to construct a `Sing` if we have a `KnownNat` constraint
-- It can also be pattern matched on to reveal a `KnownNat constraint`
SNat :: KnownNat n => Sing n

-- we can give a `Sing n` and be able to execute something in the context where
-- that `n` has a `KnownNat` constraint
withKnownNat :: Sing n -> (KnownNat n => r) -> r

View full source
generate_ :: Sing n -> (Finite n -> a) -> Vec n a
generate_ s f = withKnownNat s $
    UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

-- alternatively, via pattern matching:

generate'_ :: Sing n -> (Finite n -> a) -> Vec n a
generate'_ s@SNat f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)


generate :: KnownNat n => (Finite n -> a) -> Vec n a
generate = generate_ sing

You can see most of our original code (with pure KnownNat) rewritten to work with singletons in this file.
Why Singletons?top

As you can see, singletons-style programming completely subsumes programming with TypeNats and KnownNat. What we don’t see here is that singletons style integrates very well with the rest of the singletons ecosystem…so you might just have to take my word for it :)

What we have just witnessed is the bridge between the singletons ecosystem and the rest of the Haskell ecosystem’s usage of GHC.TypeNats. KnownNat, because it is provided by GHC itself, is universal. However, I recommend any new projects or libraries you write that do anything more than the most trivial of usages of KnownNat should take a look at doing things singletons-style.

Working with just GHC.TypeNats and KnownNat, you run into limitations very quickly unless you stick to very basic things. And, if you ever work with any other type-level stuff, singletons integrates very well and very smoothly with everything else type-level you do. If you plan on doing other type-level things besides just the most basic, you will not regret starting singletons-style from the beginning.
Real-World Examplestop

This exact pattern is used in many real-world libraries. The canonical fixed-length vector library implemented in this style is vector-sized, which more or less re-exports the entire vector library, but with a statically-sized interface. This is the library I use for all my my modern sized-vector needs.

It’s also used to great benefit by the hmatrix library, which I take advantage of in my dependently typed neural networks tutorial series.

It’s also provided in the linear library, which was one of the first major libraries to adopt this style. However, it offers an incomplete API, and requires lens — its main purpose is for integration with the rest of the linear library, which it does very well.

Anyway, if all you really wanted was performant fixed-size containers, feel free to stop reading now (or jump to the conclusion). But if you want to explore a bit deeper into the world of inductive dependent types … continue on :)
The Structural Waytop

So, the (a?) problem with TypeNats from GHC is that it has no internal structure. It’s basically the same as the Integer or Natural type — every single value (constructor) is completely structurally unrelated to the next.

Just like we can imagine

data Int = .. -2 | -1 | 0 | 1 | 2 ...

We can also think of Nat as just 0 | 1 | 2 | 3 | 4 .... Each constructor is completely distinct.

This is useful for most practical applications. However, when we want to use our fixed-length types in a more subtle and nuanced way, it might help to work with a length type that is more…structurally aware.

We’ve also noticed that the structure of our Vec and the structure of our Nat have nothing in common, so we can’t take advantage of any shared structure to help us with type-safety in our implementation…everything we wrote was pretty much implemented using “unsafe” functions.

So, enough of this non-structural blasphemy. We are proper dependent type programmers, dangit! We want structural verification! Compiler verification from the very bottom!

For this, we’ll dig into inductive type-level nats.

data Nat = Z | S Nat
  deriving Eq

We’re using the DataKinds extension, so not only does that define the type Nat with the values Z and S :: Nat -> Nat, it also defines the kind Nat with the types 'Z and 'S :: Nat -> Nat! (note the backticks)

ghci> :t S Z
Nat
ghci> :k 'S 'Z
Nat

So 'Z represents 0, and 'S represents the “successor” function: one plus whatever number it contains. 'S 'Z represents 1, 'S ('S 'Z) represents 2, etc.

And now we can define a fixed-length list, which is basically a normal haskell list “zipped” with Ss.

View full source
data Vec :: Nat -> Type -> Type where
    VNil :: Vec 'Z a
    (:+) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :+

Here, we’re using GADT syntax to define our type using its constructors: the VNil constructor (which creates a Vec 'Z a, or the empty vector, like []) and the (:+) constructor (like cons, or (:)), which conses an item to a Vec n a to get a Vec ('S n) a, or a vector with one more element.

Basically, all usage of nil and cons (VNil and :+) keeps track of the current “length” of the vectors in its type. Observe that the only way to construct a Vec ('S ('S 'Z)) a is by using two :+s and a VNil!

ghci> :t VNil
Vec 'Z a
ghci> :t True :+ VNil
Vec ('S 'Z) Bool
ghci> :t False :+ True :+ VNil
Vec ('S ('S 'Z)) Bool

Type-level Guarantees are Structurally Freetop

One nice thing about this is that there is no “unsafe” way to construct a Vec. Any Vec is inherently of the correct size. The very act of constructing it enforces its length.

Remember our “unsafe” mapVec? We had to implement it unsafely, and trust that our implementation is correct. Even worse — our users have to trust that our implementation is correct!

But writing such a mapVec function using Vec is guaranteed to preserve the lengths:

View full source
mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f = \case
    VNil    -> VNil
    x :+ xs -> f x :+ mapVec f xs

-- compare to
map :: (a -> b) -> [a] -> [b]
map f = \case
    [] -> []
    x:xs -> f x : map f xs

Our implementation is guaranteed to have the correct length. Neat! We get all of the documentation benefits described in our previous discussion of mapVec, plus more.

We can write zip too:

View full source
zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec = \case
    VNil -> \case
      VNil -> VNil
    x :+ xs -> \case
      y :+ ys -> (x,y) :+ zipVec xs ys

Isn’t it neat how the code reads exactly like the code for map/zip for lists? Because their structure is identical, their only real difference is the type-level tag. All of the functions we write are the same.
Type-Level Arithmentictop

GHC provided our + before, so we have to write it ourselves if we want to be able to use it for our Nats. We can write it as a type family:

View full source
type family (n :: Nat) + (m :: Nat) :: Nat where
    'Z   + m = m
    'S n + m = 'S (n + m)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) = \case
    VNil    -> \ys -> ys
    x :+ xs -> \ys -> x :+ (xs ++ ys)

This works! However, we have to be careful that GHC can verify that the final vector really does have the length n + m. GHC can do this automatically only in very simple situations. In our situation, it is possible because + and ++ have the exact same structure.

Take a moment to stare at the definition of + and ++ very closely, and then squint really hard. You can see that + and ++ really describe the “same function”, using the exact same structure. First, if the first item is a Z-y thing, return the second item as-is. If the first item is a consy thing, return the second item consed with the rest of the first item. Roughly speaking, of course.

This is a part of what we mean when we say that we can take advantage of the structure of the length type. Here, the structure of Nat aligns so well with the structure of Vec what we can prove structural properties about Nat and the Vec together by exploiting their shared inductive structure.

Unfortunately, for examples where the function we write doesn’t exactly match the structure as the type family we write, this won’t work. And sometimes, the structural properties might get in the way of what we are trying to prove/produce. An example here would be a snoc function (cons to the end of a list). If you try writing it, you’ll see that the structure of Nat and Vec fight back against you pretty hard. So, exploiting structure isn’t universally useful, but it definitely helps in many situations! Handling tricky cases like this is a subject for a whole other blog post.
Indexingtop

To index our previous type, we used some abstract Finite type, where Finite n conveniently represented the type of all possible indices to a Vec n a. We can do something similar, inductively, as well:

View full source
data Fin :: Nat -> Type where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)

deriving instance Show (Fin n)

I always thought of this inductive definition of Fin as a cute trick, because I don’t think there was any way I could have thought of it on my own. But if you play around it enough, you might be able to convince yourself that there are exactly n inhabitants of Fin n.

For example, for Fin ('S 'Z) (indices for a one-item vector), there should be only one inhabitant. And there is! It’s FZ. FS FZ is not a valid inhabitant, because it has type Fin ('S ('S m)) for some m, so cannot possibly have the type Fin ('S 'Z).

Let’s see the inhabitants of Fin ('S ('S ('S 'Z))) (indices for three-item vectors):

ghci> FZ              :: Fin ('S ('S ('S 'Z)))
FZ
ghci> FS FZ           :: Fin ('S ('S ('S 'Z)))
FS FZ
ghci> FS (FS FZ)      :: Fin ('S ('S ('S 'Z)))
FS (FS FZ)
ghci> FS (FS (FS FZ)) :: Fin ('S ('S ('S 'Z)))
TYPE ERROR!  TYPE ERROR!  TYPE ERROR!

As GHC informs us, FS (FS (FS FZ)) is not an inhabitant of Fin ('S ('S ('S 'Z))), which is exactly the behavior we wanted. This is because FS (FS (FS FZ)) has type Fin ('S ('S ('S ('S m)))) for some m, and this can’t fit Fin ('S ('S ('S 'Z))).

Also, note that there are no inhabitants of Fin 'Z. There is no constructor or combination of constructors that can yield a value of that type.

Armed with this handy Fin type, we can do structural type-safe indexing:

View full source
index :: Fin n -> Vec n a -> a
index = \case
    FZ -> \case
      x :+ _ -> x
    FS i -> \case
      _ :+ xs -> index i xs

Note that our Fin type structurally precludes us from being able to index into a Vec 'Z a (an empty vector), because to do that, we would have to pass in a Fin 'Z…but there is no such value with that type!
Generatingtop

Now, generating these requires some more thought. Naively writing a replicate :: a -> Vec n a is not possible; ideally, we’d want to “pattern match” on our length n, and use VNil if it’s 'Z, etc.

However, we can’t pattern match on types in Haskell, because types are erased at runtime. They’re just used by the compiler to verify your code, but they don’t exist at runtime. So, you can’t just say “do this if n is 'Z, otherwise do this”.

Recall that, in our previous vector type, we needed to use a KnownNat n constraint to be able to reflect a n type down to the value level. We can do something similar using the singletons machinery!

First, we need to get singletons for our Nat:

View full source
$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])

-- this creates:
data instance Sing :: Nat -> Type where
    SZ :: Sing 'Z
    SS :: Sing n -> Sing ('S n)

Sing n is a singleton for our Nat, in that there is only one Sing n for every n. So, if we receive a value of type Sing n, we can pattern match on it to figure out what n is. Essentially, we can pattern match on n.

View full source
singSize :: Sing (n :: Nat) -> String
singSize = \case
    -- here, n is 'Z
    SZ        -> "Size of zero!"
    -- here, n is ('S 'Z)
    SS SZ     -> "Size of one!"
    -- here, n is ('S ('S n))
    SS (SS _) -> "Wow, so big!"

We can now branch depending on what n is!

Basically, we can use a singleton if we ever want to “pattern match” or branch our program’s output based on the type. This is a general rule you will observe as we continue on this article.

Note that because of the inductive nature of our original Nat type, the singletons are also inductive, as well. This is handy, because then our whole ecosystem remains inductive.

Now, to write replicate:

View full source
replicate_ :: Sing n -> a -> Vec n a
replicate_ = \case
    SZ   -> \_ -> VNil
    SS l -> \x -> x :+ replicate_ l x

And we can recover our original “implicit” style, with type-inference-driven lengths, using SingI and sing :: SingI n => Sing n:

View full source
replicate :: SingI n => a -> Vec n a
replicate = replicate_ sing

You can think of SingI as the “generic singletons” equivalent of KnownNat. KnownNat lets us reflect out a GHC.TypeNats.Nat to a Sing…SingI lets us reflect any type that has singletons defined to its corresponding Sing. Since our new Nat type has singletons, we basically get a free “KnownNat equivalent”!

See how useful the whole singletons ecosystem is? :)
Generating with indicestop

Writing generate using the inductive Fin and Nat is an interesting challenge. It’s actually a fairly standard pattern that comes up when working with inductive types like these. I’m going to leave it as an exercise to the reader – click the link at the top corner of the text box to see the solution, and see how it compares to your own :)

View full source
generate_ :: Sing n -> (Fin n -> a) -> Vec n a

generate :: SingI n => (Fin n -> a) -> Vec n a
generate = generate_ sing

The one thing I will point out is that it is very useful that GHC verifies our code for us, and that we have typed holes to help us develop our code. If we ever don’t know something, we can just use a typed hole _, and GHC will tell us what type it expects, and what values in scope have that type. It is infinitely useful for situations like this, especially when you are new to this sort of dependently typed inductive programming!

If you ever get stuck, try throwing in a _ and seeing what types GHC expects…these clues will help you get your bearings!
Between Sized and Unsizedtop

Converting from sized to unsized vectors (to lists) is something that is pretty straightforward, and can be done by just pattern matching on the vector and recursing on the tail. I’ve left it as an excercise to write Vec n a -> [a].

More interesting is the other way around; our the API of converting unsized to sized vectors will be the same:

View full source
withVec :: [a] -> (forall n. Sing n -> Vec n a -> r) -> r

But implementing it inductively is also an interesting challenge. See my tip above about typed holes (_). I recommend taking a break here to try to solve it yourself.

Ready?

Welcome back! Hope you had a fun time :) Here’s the solution!

View full source
withVec :: [a] -> (forall n. Sing n -> Vec n a -> r) -> r
withVec = \case
    []   -> \f -> f SZ VNil
    x:xs -> \f -> withVec xs $ \l ys ->
        f (SS l) (x :+ ys)

To handle the empty list, we just return immediately, giving f the proper singleton and vector (SZ and VNil). For the non-empty list, first we convert the tail xs into a vector (ys) and its corresponding length-singleton (l), and then we give f the “correct” length singleton of our complete vector (SS l) and the correct complete vector (x :+ ys)

One nice property where (in contrast with our previous non-structural withVec) is that GHC ensures that the length of the vector we give to f is actually what we claim it is.
Verifying propertiestop

We can create some corresponding example of exactLength using the exact same process we did before

First, it’d be nice to get a witness for the length of a given vector just from the vector itself:

View full source
vecLength :: Vec n a -> Sing n
vecLength = \case
    VNil    -> SZ
    _ :+ xs -> SS (vecLength xs)

The type of vecLength :: Vec n a -> Sing n says that it is possible, from the structure of the vector given alone, to get a witness to its length. And, because the structure of the vector and the structure of the length type are so similar, this is possible! (Note that this is not possible for our non-structural “wrapped” Vec, without some unsafe operations)

Now, our code will be identical to the code for our wrapped/non-structural vectors, using %~ and Decision and Refl:

View full source
exactLength_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLength_ sM v = case sM %~ vecLength v of
    Proved Refl -> Just v
    Disproved _ -> Nothing

exactLength :: SingI m => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing

It’s nice that this is exactly the same as before, and that’s a testament to how useful the singletons library is at unifying all of these distinct type-level stuffs.

We could also write exactLength in a cute way by inducting on the length we want and the vector, so it might be fun to look at this version instead –

View full source
exactLengthInductive_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLengthInductive_ = \case
    SZ -> \case
      VNil   -> Just VNil
      _ :+ _ -> Nothing
    SS l -> \case
      VNil    -> Nothing
      x :+ xs -> (x :+) <$> exactLengthInductive_ l xs

exactLengthInductive :: SingI m => Vec n a -> Maybe (Vec m a)
exactLengthInductive = exactLengthInductive_ sing

This is another way you can take advantage of the structure of the length type. Here, we explicitly take advantage of the inductive structure of the Nat type and how it matches with the structure of the Vec type, and do bold things with it!3

But I digress. Like in the last section, checking for a given length is literally the least interesting property you can check for. But, again, the same process is usable here: find a way to get your witness, and then pattern match on that witness.

For example, we can make a witness that n is less than or equal to m, as well as a way to construct such a witness:

View full source
data LTE :: Nat -> Nat -> Type where
    LEZ :: LTE 'Z n
    LES :: LTE n m -> LTE ('S n) ('S m)

isLTE :: Sing n -> Sing m -> Decision (LTE n m)
isLTE = \case
    SZ   -> \_ -> Proved LEZ
    SS n -> \case
      SZ -> Disproved $ \case       -- EmptyCase
      SS m -> case isLTE n m of
        Proved l    -> Proved $ LES l
        Disproved p -> Disproved $ \case
          LES l -> p l

So, it is impossible to construct an LTE n m if n is not less than or equal to m. I dare you to try!

We can write code to check for this property in our vectors:

View full source
atLeast_ :: Sing n -> Vec m a -> Maybe (LTE n m, Vec m a)
atLeast_ sN v = case isLTE sN (vecLength v) of
    Proved l    -> Just (l, v)
    Disproved _ -> Nothing

atLeast :: SingI n => Vec m a -> Maybe (LTE n m, Vec m a)
atLeast = atLeast_ sing

atLeast_ sN will only return our vector if its length is at least the length of the length indicated by sN. Basically, we check if our vector is “at least” a certain length.

We can write a function that can “take” an arbitrary amount from a vector, given (via proof) that the vector has at least that many elements:

View full source
takeVec :: LTE n m -> Vec m a -> Vec n a
takeVec = \case
    LEZ   -> \_ -> VNil
    LES l -> \case
      x :+ xs -> x :+ takeVec l xs

And, we can combine that with our atLeast function, to be able to take (maybe)4 from any vector:

View full source
takeVecMaybe_ :: Sing n -> Vec m a -> Maybe (Vec n a)
takeVecMaybe_ sN v = uncurry takeVec <$> atLeast_ sN v

takeVecMaybe :: SingI n => Vec m a -> Maybe (Vec n a)
takeVecMaybe v = uncurry takeVec <$> atLeast v

In the Real Worldtop

This type is more like a list than a vector, so it’s in a bit of an awkward position, utility-wise. You usually chose a list over a vector in Haskell when you want some sort of lazy streaming, but the cases where you want to lazily stream something and you know exactly how many items you want to stream are admittedly a bit rare. GHC can’t handle infinite Vecs, so there’s that, too. For “containers”, vector is great, so the non-structural Vec is seen a lot more.

However, if you are working with a lot of other inductive types, Vec works very naturally alongside them. It makes sense, then, that a “canonical” package offering Vec is type-combinators, an actively maintained library with loads of useful inductive types for type-level programming, exporting its own Nat and Sing-equivalents. If I am doing the sort of type-level programming that Vec is useful for, chances are I already have type-combinators imported. This is the library that I personally suggest if you want to use this Vec in the real world.
Wrapping uptop

There’s obviously more to look at, and much more we can do with fixed-length vectors and inductive types. And, there will definitely be more issues that come up when you start working with these in the real world, with real applications.

If you plan on moving into learning about dependent types, I hope that guide would be a good launching point. But if all you wanted to do was learn how to use fixed-length vectors effectively in Haskell…hopefully after reading this, you have confidence to work with these things directly, and to know what to google if anything else comes up :)

Feel free as always to leave a comment or a tweet, or find me the freenode #haskell channel, as jle`. I always welcome feedback, suggestions, or questions!

    Users who are used to GHC 8.0 and below might remember Nat coming from GHC.TypeLits. Well, GHC 8.2 is here, TypeLits is out, TypeNats is in. The difference is that, in TypeLits, the Nat kind reifies/reflects with Integer. In TypeNats, the Nat kind reifies/reflects with Natural from Numeric.Natural.↩

    For singletons > 2.3 fromSing and toSing give and take Natural when going to Nat. However, for 2.3.1 and below, they give/take Integer instead.↩

    Note, however, that if you unroll the definition of %~ for Nat, you pretty much get the exact same thing.↩

    Remember the whole point of this exercise — that the Maybe is required only in the completely polymorphic case, where we get our lengths at runtime and don’t know them at compile-time. If we knew n and m at compile-time, and knew that n was less than or equal to m, we could construct an LTE n m and call takeVec directly, and not return a Maybe.↩

    #haskell
    #types
    @HASKELL
    @REFERENCE
    @TUTORIALS

submit to reddit

    ← Verify your Typeclass Instances in Haskell Today! (Previous)
    (Next) Hamiltonian Dynamics in Haskell →

© 2016 Justin Le

        TwitterGoogle+LinkedInGithubKeybaseBitcoin
        RSSMailing list
