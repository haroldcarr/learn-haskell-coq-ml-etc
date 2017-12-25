> {-# LANGUAGE DataKinds                 #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE FlexibleInstances         #-}
> {-# LANGUAGE GADTs                     #-}
> {-# LANGUAGE MultiParamTypeClasses     #-}
> {-# LANGUAGE StandaloneDeriving        #-}
> {-# LANGUAGE TypeFamilies              #-}
> {-# LANGUAGE TypeOperators             #-}
>
> module REST where

> import           Data.Char       (ord)
> import           Test.HUnit      (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util as U (t, tt, e)

import Debug.Trace

debug = flip trace

------------------------------------------------------------------------------
* Generic Programming with GADTs (p. 12)

Datatype-generic : functions take types as an arg, change behavior depending on type.
- http://en.wikipedia.org/wiki/Generic_programming

Example: encode data in binary form (can be done with type classes too).

Representation type : values represent types:

> data Type t where
>   TInt  :: Type Int
>   TChar :: Type Char
>   TList :: Type t -> Type [t]
>   TDyn  :: Type Dynamic        -- not used until p. 14

:t TInt
-- TInt :: Type Int
:t TList
-- TList :: Type t -> Type [t]
:t TList TInt
-- TList TInt :: Type [Int]

Since Haskell =String= is =[Char]=, define value constructor:

> tString :: Type String
> tString = TList TChar

Output of encoding function is list of bits:

> data Bit = F | T deriving (Eq, Show)

Encoding function (p. 13):
- TODO: look into details of encoding functions (but does not matter for this GADT example)

> encode :: Type t -> t -> [Bit]
> encode TInt i           = encodeInt i
> encode TChar c          = encodeChar c
> -- note T consed on front and F on end as separators
> encode (TList _) []     = F : []
> encode (TList t) (x:xs) = T : (encode t x) ++ encode (TList t) xs
> encode TDyn (Dyn t v)   = encode t v                               -- not used until p. 14

[[http://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell]] :

> encodeInt :: Int -> [Bit]
> encodeInt 0 = [F]
> encodeInt n = reverse $ helper n
>     where helper 0 = []
>           helper n = let (q,r) = n `divMod` 2 in (mkBit r) : helper q
>           mkBit  i = if i == 1 then T else F

> encodeChar :: Char -> [Bit]
> encodeChar c = encodeInt $ ord c

> tei0 = U.t "tei0" (encodeInt 0) [F]
> tei5 = U.t "tei5" (encodeInt 5) [T,F,T]
> tei331 = U.t "tei331" (encode TInt 331) [T,F,T,F,F,T,F,T,T]
> tei333 = U.t "tei333" (encode TInt 333) [T,F,T,F,F,T,T,F,T]
> -- Note: paper shows [T,F,T,...,F,F,F] for this
> tei1 = U.t "tei1" (encode TInt 1)   [T]
> tei2 = U.t "tei2" (encode TInt 2) [T,F]
> tei3 = U.t "tei3" (encode TInt 3) [T,T]

> t1 = U.t "t1"
>       (encode TInt 1 ++      encode TInt 2 ++       encode TInt 3)         [  T,  T,F,  T,T]
> t2 = U.t "t2"
>   (T : encode TInt 1 ++ (T : encode TInt 2) ++ (T : encode TInt 3) ++ [F]) [T,T,T,T,F,T,T,T,F]
> t3 = U.t "t3"
>   (encode (TList TInt) [1,2,3])                                            [T,T,T,T,F,T,T,T,F]
> -- Note: paper shows [T,T,F,...,F,F,F]

[[http://thid.thesa.com/thid-0513-0671-th-1425-3196][Universal Data Type]] : Organize around single universal type
(e.g., APL/real number arrays; SNOBOL/strings; LISP/lists; fun prog/exprs, Object/Java - except unboxed primitives).

Pair representation type with value (requires =ExistentialQuantification=) (p. 13):

> data Dynamic' = forall t. Dyn' (Type t) t

Previous defines [[http://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types][existential data type]]:
way of "squashing" a group of types into one, single type (in Haskell).

Can also be represented as GADT:

> data Dynamic where
>    Dyn :: Type t -> t -> Dynamic

> encode' :: Dynamic -> [Bit]
> encode' (Dyn t v) = encode t v

> c = Dyn (TList TInt) [1,2,3]

:t c
-- c :: Dynamic

> tec = U.tt "tec"
>   [ encode' c
>   , encode (TList TInt) [1,2,3]
>   ]
>   [T,T,T,T,F,T,T,T,F]

Define heterogeneous lists (p. 14):

> d = [Dyn TInt 10, Dyn tString "test"]

:t d
--      [Dyn TInt 10, Dyn tString "test"] :: [Dynamic]
-- (Note: paper had : Dyn TString "test")

But cannot make this list =Dynamic=.

FIX:
- extend representation, adding value constructor (done above in =data Type t=).
- add to patterns of encode functions (done above in =encode TDyn=).

> e = Dyn (TList TDyn) d

:t e
-- e :: Dynamic

> tee = U.t "tee"
>   (encode' e)
>   [T,T,F,T,F,T,T,T,T,T,F,T,F,F,T,T,T,F,F,T,F,T,T,T,T,T,F,F,T,T,T,T,T,T,F,T,F,F,F,F]

Dynamic data type is useful for communication with env when type not known in advance.
Then a type cast is required (p. 14):

> castInt :: Dynamic -> Maybe Int
> castInt (Dyn TInt i) = Just i
> castInt (Dyn _    _) = Nothing

More generic solution that works for all types referenced (but not shown) in paper.

Conclusion:
- PRO: generic programming possible
- CON: must extend representation type whenever define new data type

------------------------------------------------------------------------------
* Proving Correctness of List Operations (p. 15)

Types can ensure only a non-empty List is passed to =head=.
Types can encode other properties: e.g., non-empty lists; lists of certain length.

ADT:
data List t = Nil | Cons t (List t)

-- GADT:

> data List t where
>   Nil  ::                List t
>   Cons :: t -> List t -> List t

> listHead :: List t -> t
> listHead (Cons a _) = a
> listHead Nil        = error "empty list"

--------------------------------------------------
** Encode empty/non-empty list in type

To ensure no failure, define non-empty lists:

> data Empty
> data NonEmpty

> -- param f is Empty when list is empty, NonEmpty otherwise
> data SafeList1 t f where
>   Nil1  ::                       SafeList1 t Empty
>   Cons1 :: t -> SafeList1 t f -> SafeList1 t NonEmpty

> -- head that can ONLY take non-empty lists (p. 16):
> headSafe1 :: SafeList1 t NonEmpty -> t
> headSafe1 (Cons1 t _) = t

headSafe1 Nil1
--    Couldn't match type `Empty' with `NonEmpty'
--    Expected type: SafeList1 t0 NonEmpty
--      Actual type: SafeList1 t0 Empty

> hs = Cons1 1 $ Cons1 2 $ Cons1 3 Nil1

:t hs
hs :: SafeList1 Integer NonEmpty

> ths1 = U.t "ths1" (headSafe1 hs) 1

PROBLEM:

repeatElem1 :: a -> Int -> SafeList1 a ???
repeatElem1 a 0 = Nil1
repeatElem1 a n = Cons1 a (repeatElem a (n-1))

Cannot determine return type because =Empty= / =NonEmpty= lists have completely different types.

FIX: relax =Cons1= value constructor:

> data SafeList2 t f where
>   Nil2  ::                       SafeList2 t Empty
>   Cons2 :: t -> SafeList2 t f -> SafeList2 t f'     -- note f'
>
> deriving instance Show t => Show (SafeList2 t f)

Now =SafeList t Empty= is a type of possibly empty lists:

:t Nil2
-- Nil2 :: SafeList2 t Empty
:t Cons2 'a' Nil2
-- Cons2 'a' Nil2 :: SafeList2 Char f'
:t Cons2 'a' Nil2 :: SafeList2 Char Empty
-- Cons2 'a' Nil2 :: SafeList2 Char Empty    :: SafeList2 Char Empty
:t Cons2 'a' Nil2 :: SafeList2 Char NonEmpty
-- Cons2 'a' Nil2 :: SafeList2 Char NonEmpty :: SafeList2 Char NonEmpty

Now can define (p. 17):

> repeatElem2 :: a -> Int -> SafeList2 a Empty
> repeatElem2 a 0 = Nil2
> repeatElem2 a n = Cons2 a (repeatElem2 a (n-1))

> a = repeatElem2 'c' 3

:t a
-- a :: SafeList2 Char Empty

PROBLEM: anything can slip through =f=':

:t Cons2 'a' Nil2 :: SafeList2 Char Bool
-- Cons2 'a' Nil2 :: SafeList2 Char Bool :: SafeList2 Char Bool

:t Cons2 'a' Nil2 :: SafeList2 Char Int
-- Cons2 'a' Nil2 :: SafeList2 Char Int :: SafeList2 Char Int

FIX: give =Empty= / =NonEmpty= same kind.   Discussed later for =Nat=.

--------------------------------------------------
** Encode list length in type

Stronger invariant: list length (p. 17):

Note:  =Empty= / =NonEmpty= not enough.  Need to encode length in type.

(Requires =DataKinds=.)

> -- Peano numbers
> data Zero3
> data Succ3 n

> data List3 a n where
>   Nil3  ::                   List3 a Zero3
>   Cons3 :: a -> List3 a n -> List3 a (Succ3 n)

> headSafe3 :: List3 t (Succ3 n) -> t
> headSafe3 (Cons3 t _) = t

> -- type encode that map does not change length
> mapSafe3 :: (a -> b) -> List3 a n -> List3 b n
> mapSafe3 _         Nil3 = Nil3
> mapSafe3 f (Cons3 x xs) = Cons3 (f x) (mapSafe3 f xs)

> hs3 = headSafe3 $ Cons3 1 $ Cons3 2 $ Nil3

:t hs3
-- hs3 :: Integer

> ms = mapSafe3 (\x -> x + 1) $ Cons3 1 $ Cons3 2 $ Nil3

:t ms
-- ms :: List3 Integer (Succ3 (Succ3 Zero3))

To implement concatenation need type-level Peano addition.
- One way: type families (here understood as type-level function)
- Requires =TypeFamilies=
- p. 18

> type family Plus3 a b
> type instance Plus3 Zero3     n = n
> type instance Plus3 (Succ3 m) n = Succ3 (Plus3 m n)

> concatenate3 :: List3 a m -> List3 a n -> List3 a (Plus3 m n)
> concatenate3 Nil3 ys = ys
> concatenate3 (Cons3 x xs) ys = Cons3 x (concatenate3 xs ys)

> c3 = concatenate3 (Cons3 1 $ Cons3 2 $ Nil3) (Cons3 3 $ Cons3 4 $ Nil3)

:t c3
-- c3 :: List3 Integer (Succ3 (Succ3 (Succ3 (Succ3 Zero3))))

PROBLEM: =Succ= has a type parameter of =kind *=.
- allows nonsense: =Succ Int=

 #+BEGIN_COMMENT
TODO: I get: Not in scope: data constructor `Succ'
 #+END_COMMENT

FIX: Types classify values.  Kinds classify types.  So declare a new kind:

- =Nat'= is a type, =Zero'= / =Succ'= are value constructors.
- But, due to promotion, =Nat'= also a kind; =Zero'= / =Succ'= also types.
- Sometimes necessary to prepend quote (e.g., '=Succ=') to refer to *type* (not value constructor)

> data Nat4 = Zero4 | Succ4 Nat4

> -- Type-level representation of number two (although prepended quote not necessary here):
> type    Two = 'Succ4 ('Succ4 'Zero4)

:i Two
-- type Two = 'Succ4 ('Succ4 'Zero4)

Now =Succ Int= will be rejected.

Specify type of second parameter has kind Nat (p. 19):

> data List4 a (n::Nat4) where
>   Nil4  ::                   List4 a  'Zero4
>   Cons4 :: a -> List4 a n -> List4 a ('Succ4 n)

PROBLEM: But can't write return type for:

repeatElem4 :: a -> Int -> List4 ????

Need count both a runtime and type-check time.

FIX: singleton types (types with only one value other than bottom):

> data NatSing (n::Nat4) where
>   ZeroSing ::              NatSing  'Zero4
>   SuccSing :: NatSing n -> NatSing ('Succ4 n)

=NatSing= constructors mirror =Nat=4 constructors.
Thus every TYPE of kind =Nat= corresponds to exactly *one* VALUE of the singleton data type where parameter =n= has exactly this type.

:t ZeroSing
-- ZeroSing :: NatSing 'Zero4

:t SuccSing $ SuccSing ZeroSing
-- SuccSing $ SuccSing ZeroSing :: NatSing ('Succ4 ('Succ4 'Zero4))

Can now define:

> repeatElem4 :: a -> NatSing n -> List4 a n
> repeatElem4 _ ZeroSing     = Nil4
> repeatElem4 x (SuccSing n) = Cons4 x (repeatElem4 x n)  -- note: subtraction done by structural induction

> re4 = repeatElem4 'C' (SuccSing $ SuccSing ZeroSing)

:t re
-- re :: List4 Char ('Succ4 ('Succ4 'Zero4))

--------------------------------------------------
** Encode length comparison in type

Example: do not exceed list length

Requires =TypeOperators=

Requires type-level magnitude comparison function (defined by structural induction):

> type family   (m::Nat4)  :< (n::Nat4) :: Bool
> type instance  m         :< 'Zero4     = 'False
> type instance 'Zero4     :< ('Succ4 n) = 'True
> type instance ('Succ4 m) :< ('Succ4 n) = m :< n

- given
  - list of length      =m=
  -         index  =n=
- ensure           =n :< m=
- note: =~= is equality constraint

> nthElem4 :: (n :< m) ~ 'True => List4 a m -> NatSing n -> a
> nthElem4 (Cons4 x  _) ZeroSing     = x
> nthElem4 (Cons4 _ xs) (SuccSing n) = nthElem4 xs n

ne4 = nthElem4 (repeatElem4 'C' (SuccSing $ SuccSing ZeroSing)) (SuccSing $ SuccSing ZeroSing)
--    Couldn't match type 'False with 'True
--    Expected type: 'True
--      Actual type: 'Succ4 ('Succ4 'Zero4)
--                   :< 'Succ4 ('Succ4 'Zero4)

> ne4 = nthElem4 (repeatElem4 'C' (SuccSing $ SuccSing ZeroSing))            (SuccSing ZeroSing)

:t ne
-- ne :: Char

--------------------------------------------------
** LIST SUMMARY (p. 21):

- Used GADTs to specify correctness of list operations verified by type-checker.
- Specified necessary properties in the data type.
- Set of properties motivated by the actual operations to be performed.
- =head= : only needed =Empty= / =NonEmpty=
- Other operations need count of elements it contains.

------------------------------------------------------------------------------
* Proving Correctness of Red-Black Tree Insert (p. 21)

- [[http://en.wikipedia.org/wiki/Red%E2%80%93black_tree][Red-Black Trees]]
- [[http://www.seas.upenn.edu/~sweirich/][Stephanie Weirich's]]
  - slides ([[http://www.seas.upenn.edu/~sweirich/talks/flops2012.pdf][pdf]]) for reference [7] (p. 33)
  - [[http://www.seas.upenn.edu/~cis552/12fa/schedule.html][course/code]] - scroll down to RedBlack[1|2|3]

> data Color   = R | B deriving (Eq, Show)
> data Node' a = E' | N' Color (Node' a) a (Node' a)
> type Tree' a = Node' a

For any node =N c l x r=, values less than =x= are stored in =l=, otherwise =r=:

> member' :: Ord a => a -> Tree' a -> Bool
> member' _ E' = False
> member' x (N' _ l a r)
>   | x < a = member' x l
>   | x > a = member' x r
>   | otherwise = True

Invariants (guarantee tree is balanced) (p. 22)
- ensure longest path from root
  - containing alternating red-black nodes)
- can only be twice as long as the shortest path
  - containing only red nodes.

Ensure operations take /O/ (log /n/ ) time,
(where /n/ is number of elements) in worst case.

1. Root is black.
-  Leafs are black.
-  Red nodes have black children.
-  /Black Height/: For each node, all paths from that node to leaf
   contain same number of black nodes.

> insert' :: Ord a => Tree' a -> a -> Tree' a
> insert' t v = blacken (insert'' t v) where
>     insert'' n@(N' c l a r) x
>         | x < a = leftBalance'  (N' c (insert'' l x) a           r)
>         | x > a = rightBalance' (N' c           l    a (insert'' r x))
>         | otherwise = n
>     insert''    E'     x    = N' R E' x E'
>     blacken    (N' _ l x r) = N' B l  x r

Same recursive descent to leaf nodes as binary search trees, except
ensuring invariants:
- 4: red node inserted
- 1: blacken root
- 3: =leftBalance= / =rightBalance=

> leftBalance' :: Node' a -> Node' a
> leftBalance' (N' B (N' R (N' R a x       b) y       c)  z d) =
>               N' R (N' B       a x       b) y (N' B c   z d)
> leftBalance' (N' B (N' R       a x (N' R b  y       c)) z d) =
>               N' R (N' B       a x       b) y (N' B c   z d)
> leftBalance' n = n

> rightBalance' :: Node' a -> Node' a
> rightBalance' (N' B       a x (N' R       b  y (N' R c  z d))) =
>                N' R (N' B a x             b) y (N' B c  z d)
> rightBalance' (N' B       a x (N' R (N' R b  y       c) z d))  =
>                N' R (N' B a x             b) y (N' B c  z d)

--------------------------------------------------
** Proving 4th invariant maintained by insert (p. 23)

Add black height:

> data Nat = Zero | Succ Nat deriving (Eq, Show)

data Node a (bh::Nat) where
    -- leaf has bh 0
    E :: Node a 'Zero
    -- bh must be conditionally incremented based on color
    N :: Color -> Node a bh -> a -> Node a bh -> Node a ???
-}

Increment done via type family (requires =TypeFamilies=, =DataKinds=) (p. 24):

> type family IncBH (c::Color) (bh::Nat) :: Nat
> type instance IncBH R bh =      bh
> type instance IncBH B bh = Succ bh

Requires color to be passed as type (for =IncBH=) and as a value (for
=Node= value constructor).  Use singleton type as bridge:

> data ColorSingleton (c::Color) where
>   SR :: ColorSingleton R
>   SB :: ColorSingleton B

> instance Show (ColorSingleton c) where
>   show SR = "R"
>   show SB = "B"

Value of singleton type passed to =Node= value constructor and
color type used for =IncBH=:

> data Node4 a (bh::Nat) where
>   E4 :: Node4 a 'Zero
>   N4 :: ColorSingleton c -> Node4 a bh -> a -> Node4 a bh
>                          -> Node4 a (IncBH c bh)

In Haskell, when creating a new type, every type variable on
right-hand side of definition must also appear on left-hand
side. Therefore (p. 24):

PROBLEM: cannot write:

type Tree4 a = Node4 a bh

FIX 1: use /existential types/ (requires =RankNTypes=):

type Tree4 a = forall bh. Node4 a bh

FIX 2: GADT:

> data Tree4 a where
>   Root4 :: Node4 a bh -> Tree4 a

=insert= same as above except type annotations (p. 36):

insert4 :: Ord a => Tree4 a -> a -> Tree4 a
insert4 (Root4 t) v = blacken (insert' t v) where
    insert' :: Ord a => Node4 a n -> a -> Node4 a n
    insert' n@(N4 c l a r) x
        | x < a = leftBalance4  (N4 c (insert' l x) a          r)
        | x > a = rightBalance4 (N4 c          l    a (insert' r x))
        | otherwise = n
    insert'    E4     x    =        N4 SR E4 x E4
    blacken   (N4 _ l x r) = Root4 (N4 SB l x r)

> leftBalance4  :: Node4 a bh -> Node4 a bh
> leftBalance4  (N4 SB (N4 SR (N4 SR a x        b) y        c)  z d) =
>                N4 SR (N4 SB        a x        b) y (N4 SB c   z d)
> leftBalance4  (N4 SB (N4 SR        a x (N4 SR b  y        c)) z d) =
>                N4 SR (N4 SB        a x        b) y (N4 SB c   z d)
> leftBalance4 n = n

> rightBalance4 :: Node4 a bh -> Node4 a bh
> rightBalance4 (N4 SB        a x (N4 SR        b  y (N4 SR c   z d))) =
>                N4 SR (N4 SB a x               b) y (N4 SB c   z d)
> rightBalance4 (N4 SB        a x (N4 SR (N4 SR b  y        c)  z d))  =
>                N4 SR (N4 SB a x               b) y (N4 SB c   z d)

--------------------------------------------------
** Proving 3rd invariant maintained by insert (p. 25)

Valid colors for a node on type level.  Can be done via type families
(as above) or type classes (here) (requires =MultiParamTypeClasses=):

 #+BEGIN_SRC haskell
class ValidColors (parent::Color) (child1::Color) (child2::Color)
 #+END_SRC

Functions not needed on =ValidColors=, just valid instance (requires =FlexibleInstances=):

 #+BEGIN_SRC haskell
instance ValidColors R B  B  -- red with only black children
instance ValidColors B lc rc -- black with children of any color
 #+END_SRC

Add color type as param to =Node= and restrict to =ValidColors=
(also ensures 2nd invariant):

 #+BEGIN_SRC haskell
data Node a (bh::Nat) (c::Color) where
    E :: Node a 'Zero B
    N :: ValidColors c lc rc => ColorSingleton c
           -> Node a bh lc -> a -> Node a bh rc
              -> Node a (IncBH c bh) c

instance Show a => Show (Node a b c) where
    show  E          = "eb"
    show (N c l x r) = "(N"
                         ++ " " ++ (show c)
                         ++ " " ++ (show l)
                         ++ " " ++ (show x)
                         ++ " " ++ (show r)
                         ++ ")"
 #+END_SRC

Root of =Tree= is black (1st invariant):

 #+BEGIN_SRC haskell
data Tree a where
    Root :: Node a bh B -> Tree a

instance Show a => Show (Tree a) where
    show (Root t) = "(Root " ++ show t ++")"
 #+END_SRC

Insert can temporarily invalidate 3rd invariant.
So cannot use =Tree=.  Instead a =Node= with color restrictions:

 #+BEGIN_SRC haskell
data IntNode a (n::Nat) where
    IntNode :: ColorSingleton c
                 -> Node a n c1 -> a -> Node a n c2
                    -> IntNode a (IncBH c n)
 #+END_SRC

Update type of =insert= functions (p. 26/38):

 #+BEGIN_SRC haskell
insert :: Ord a => Tree a -> a -> Tree a
insert (Root t) v = blacken (insert' t v) where
    insert' :: Ord a => Node a n c -> a -> IntNode a n
    insert'     n@(N c l a r) x
        | x < a = leftBalance  c (insert' l x) a          r    `debug` "i<"
        | x > a = rightBalance c          l    a (insert' r x) `debug` "i>"
        | otherwise = IntNode  c          l    a          r    `debug` "i="
    insert'        E          x =
                      IntNode  SR         E    x          E    `debug` "iE"
    blacken (IntNode _ l x r) =
                      Root  (N SB         l    x          r)   `debug` "blacken"
 #+END_SRC

Before, passed whole =Node= as param.
But 3rd invariant can be temporarily violated.
So explicitly pass params of =Node= and left child
using =IntNode=:

 #+BEGIN_SRC haskell
leftBalance :: ColorSingleton c
               -> IntNode a n -> a -> Node a n c'
                  -> IntNode a (IncBH c n)
-- 1:
leftBalance         SB (IntNode SR (N SR a              x       b) y       c)   z d =
            IntNode SR (N       SB       a              x       b) y (N SB c    z d)    `debug` "lb1"
-- 2:
leftBalance         SB (IntNode SR       a              x (N SR b  y       c))  z d =
            IntNode SR (N       SB       a              x       b) y (N SB c    z d)    `debug` "lb2"
-- 3:
-- tree balanced, but need to change type from IntNode to Node:
leftBalance         c  (IntNode SB       a              x       b)              z d =
            IntNode c  (N       SB       a              x       b)              z d     `debug` "lb3"
-- 4:
-- red nodes must have black children
leftBalance         c  (IntNode SR       a@(N SB _ _ _) x       b@(N SB _ _ _)) z d =
            IntNode c  (N       SR       a              x       b)              z d     `debug` "lb4"
-- 5:
-- red nodes must have black children
leftBalance         c  (IntNode SR       E              x          E)           z d =
            IntNode c  (N       SR       E              x          E)           z d     `debug` "lb5"

-- cannot happen, but not enough type info to omit:
leftBalance         _  (IntNode SR        (N SR _ _ _)  _          _)           _ _ =
                error "cannot happen"
leftBalance         _  (IntNode SR      _               _         (N SR _ _ _)) _ _ =
                error "cannot happen"

-- The case of one regular node and one leaf node is not valid,
-- because nodes have different black heights
-- so no need to look for that case.
 #+END_SRC

p. 38

 #+BEGIN_SRC haskell
rightBalance :: ColorSingleton c
               -> Node a n c' -> a -> IntNode a n
                  -> IntNode a (IncBH c n)
-- 1:
rightBalance               SB a x (IntNode SR       b              y (N SR c  z d)) =
             IntNode SR (N SB a x                   b)             y (N SB c  z d)    `debug` "rb1"
-- 2:
rightBalance               SB a x (IntNode SR (N SR b              y       c) z d)  =
             IntNode SR (N SB a x                   b)             y (N SB c  z d)    `debug` "rb2"
-- 3:
rightBalance         c        a x (IntNode SB       b              y            d)  =
             IntNode c        a x (N       SB       b              y            d)    `debug` "rb3"
-- 4:
rightBalance         c        a x (IntNode SR       b@(N SB _ _ _) y            d@(N SB _ _ _)) =
             IntNode c        a x (N       SR       b              y            d)    `debug` "rb4"
-- 5:
rightBalance         c        a x (IntNode SR  E                   y  E)            =
             IntNode c        a x (N       SR  E                   y  E)              `debug` "rb5"

rightBalance _                _ _ (IntNode SR (N SR _ _ _)         _  _)            =
             error "cannot happen"
rightBalance _                _ _ (IntNode SR _                    _ (N SR _ _ _))  =
             error "cannot happen"
 #+END_SRC

 # --------------------------------------------------
** Red-Black Trees in Action

 #+BEGIN_SRC haskell
member :: Ord a => a -> Tree a -> Bool
member x (Root t) = mem x t where
    mem :: Ord a => a -> Node a bh c -> Bool
    mem x E = False
    mem x (N _ l y r)
        | x < y     = mem x l
        | x > y     = mem x r
        | otherwise = True

elements :: Ord a => Tree a -> [a]
elements (Root t) = aux t [] where
    aux :: Ord a => Node a bh c -> [a] -> [a]
    aux E xs = xs
    aux (N _ l y r) xs = aux l (y : aux r xs)
 #+END_SRC

 #+BEGIN_EXAMPLE
member 100 $                         insert (Root E) 100
member   0 $                 insert (insert (Root E) 100) 101
member 100 $         insert (insert (insert (Root E) 100) 101) 1
member   0 $ insert (insert (insert (insert (Root E) 100) 101) 1) 0
elements   $ insert (insert (insert (insert (Root E) 100) 101) 1) 0
-- [0,1,100,101]
 #+END_EXAMPLE

[[http://cs.lmu.edu/~ray/notes/redblacktrees/]]

In progress ...

 #+BEGIN_EXAMPLE
let root  = (Root E)

(Root eb)

let one   = insert root   4

iE
blacken
(Root (N B eb 4 eb))
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let two   = insert one    7

i>
iE
rb5
blacken
(Root (N B eb 4 (N R eb 7 eb)))
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let three = insert two   12

i>
i>
iE
rb5
rb1
blacken
(Root (N B (N B eb 4 eb) 7 (N B eb 12 eb)))
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let four  = insert three 15
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let five  = insert four   3
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let six   = insert five   5
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let seven = insert six   14
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let eight = insert seven 18
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let nine  = insert eight 16
 #+END_EXAMPLE
 #+BEGIN_EXAMPLE
let ten   = insert nine  17
 #+END_EXAMPLE

 # --------------------------------------------------
** Red-Black Tree proofs in Agda and Coq

- [[http://wiki.portal.chalmers.se/agda/pmwiki.php][Agda]]
  - See [[http://www.cs.cmu.edu/~drl/][Dan Licata's]] lecture videos at
    [[http://www.cs.uoregon.edu/research/summerschool/summer13/curriculum.html][Oregon Programming Languages Summer School 2013]]
    (scroll down)
- [[http://coq.inria.fr/][Coq]]
  - In [[http://adam.chlipala.net/][Ada Chlipala's]]
    /Certified Programming with Dependent Types/
    [[http://adam.chlipala.net/cpdt/html/MoreDep.html][MoreDep]]
    chapter (scroll down)

 # END OF FILE.

------------------------------------------------------------------------------

> testR :: IO Counts
> testR =
>     runTestTT $ TestList $ tei0 ++ tei5 ++ tei331 ++ tei333 ++ tei1 ++ tei2 ++ tei3 ++ t1 ++ t2 ++ t3 ++
>                            tec ++ tee ++ ths1
