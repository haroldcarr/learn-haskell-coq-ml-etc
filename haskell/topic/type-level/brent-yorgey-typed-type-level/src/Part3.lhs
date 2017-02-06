
 https://byorgey.wordpress.com/2010/07/19/typed-type-level-programming-in-haskell-part-iii-i-can-haz-typs-plz/

In Part II, I showed how type families can be used to do type-level programming in a functional style. For example, here is addition of natural numbers again:

  data Z
  data S n

  type family Plus m n :: *
  type instance Plus Z n = n
  type instance Plus (S m) n = S (Plus m n)
Now, why might we want to do such a thing? One example (I know, I know, this is always the example… but hey, it’s a good example) is if we wanted to have a type of polymorphic length-indexed vectors (or as they are sometimes known, “Length-Observed Lists”) where the type of a vector includes its length. Using a generalized algebraic data type (GADT), we can write something like this:

  data LOL :: * -> * -> * where
    KThxBye :: LOL Z a
    Moar    :: a -> LOL n a -> LOL (S n) a
This says that

LOL is a type constructor of kind * -> * -> *, that is, it takes two type arguments of kind * and produces a type of kind *. The intention is that the first argument records the length, and the second records the type of the elements.
KThxBye constructs a vector of length zero.
Given an element of type a and a vector of as of length n, Moar constructors a vector of length S n.
The type-level function Plus comes in when we implement an append function for our length-indexed vectors: in order to express the type of append we have to add the lengths of the input vectors.

  append :: LOL m a -> LOL n a -> LOL (Plus m n) a
  append KThxBye     v = v
  append (Moar x xs) v = Moar x (append xs v)
If you haven’t already seen things like this, it’s a good exercise to figure out why this definition of append typechecks (and why it wouldn’t typecheck if we put anything other than Plus m n as the length of the output).

OK, great! We can make GHC check the lengths of our lists at compile time. So what’s the problem? Well, there are (at least) three obvious things which this code leaves to be desired:

It doesn’t matter whether we have already declared a Nat type with constructors Z and S; we have to redeclare some empty types Z and S to represent our type-level natural number “values”. And declaring empty types to use like “values” seems silly anyway.
It also doesn’t matter whether we’ve already implemented a plus function for our Nat values; we must re-code the addition algorithm at the type level with the type family Plus. Especially irksome is the fact that these definitions will be virtually identical.
Finally, and most insidiously, LOL is essentially untyped. Look again at the kind of LOL :: * -> * -> *. There’s nothing in the kind of LOL that tells us the first argument is supposed to be a type-level number. Nothing prevents us from accidentally writing the type LOL Int (S Z) — we’ll only run into (potentially confusing) problems later when we try to write down a value with this type.
Wouldn’t it be nice if we could reuse (1) values and (2) functions at the type level, and (3) get more informative kinds in the bargain? Indeed, inspired by Conor McBride’s SHE, our work aims precisely to enable (1) and (3) in GHC as a start, and hopefully eventually (2) (and other features) as well. Hopefully soon, you’ll be able to write this:

  data Nat = Z | S Nat

  type family Plus (m::Nat) (n::Nat) :: Nat
  type instance Plus Z n = n
  type instance Plus (S m) n = S (Plus m n)

  data LOL :: Nat -> * -> * where
    KThxBye :: LOL Z a
    Moar    :: a -> LOL n a -> LOL (S n) a

  append :: ...  -- exactly the same as before
…or even this:

  data Nat = Z | S Nat

  plus :: Nat -> Nat -> Nat
  plus Z n = n
  plus (S m) n = S (plus m n)

  data LOL :: Nat -> * -> * where ... -- same as above

  append :: LOL m a -> LOL n a -> LOL (plus m n) a
  append = ...  -- same as before
In another post I’ll explain what the above fantasy code would be doing in a bit more detail, talk about precisely how we propose to accomplish this, and discuss why we might want to do things this way, rather than introducing full dependent types (or just chucking Haskell and all moving to Agda).

