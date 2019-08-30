> {-# LANGUAGE GADTs #-}
>
> module Ch06_01_generics_sum_of_products where
>
> import Test.Hspec

------------------------------------------------------------------------------

example data

> data List' a = Nil' | Cons' a (List' a) deriving (Eq, Show)
>
> data Tree a
>   = Node a (Tree a) (Tree a)
>   | Leaf a
>   deriving (Eq, Show)
>
> aList = Cons' 2 (Cons' 3 (Cons' 5 Nil'))
>
> intTree = Node 2 (Leaf 3)
>                  (Node 5 (Leaf 7)
>                          (Leaf 11))

type-specific functions

> sizeT (Leaf _)       = 1
> sizeT (Node _ lt rt) = 1 + sizeT lt + sizeT rt
>
> sizeL Nil'           = 0
> sizeL (Cons' _ xs)   = 1 + sizeL xs

shape of functions follows shape of type

to make type-generic functions, first, define a type representation

------------------------------------------------------------------------------

Lightweight Implementation of Generics and Dynamics (LIGD) by Cheney and Hinze, 2002
as revised in
Libraries for Generic Programming in Haskell by Jeuring et al., 2009

SUM OF PRODUCTS TYPE REPRESENTATION

List' deconstructed as either Nil or the combination of an element with another list.

> data U          = U         deriving (Eq, Show) -- rep for no-arg constructors
> data Choice a b = L a | R b deriving (Eq, Show) -- rep for choice between multiple constructors
> data Combo  a b = Combo a b deriving (Eq, Show) -- rep for two args to constructor

List' represented as:

> type RList a = Choice U (Combo a (List' a))

Note : RList does not recurse. It refers to List'. This is called a shallow type representation.

Express multiple arguments through nesting.

Tree rep:

> type RTree a =
>   Choice (Combo U a)
>          (Combo a
>                 (Combo (Tree a)
>                        (Tree a)))

sum refers to Choice
product refers to Combo
unit refers to U

------------------------------------------------------------------------------

Translating between the type and representation

> fromL :: List' a -> RList a
> fromL            Nil' = L U
> fromL    (Cons' x xs) = R (Combo x xs)
>
> toL   :: RList a -> List' a
> toL   (L U)            = Nil'
> toL   (R (Combo x xs)) = Cons' x xs

> ch06_01_e1 = it "ch06_01_e1" $ fromL aList `shouldBe` R (Combo 2 (Cons' 3 (Cons' 5 Nil')))
> ch06_01_e2 = it "ch06_01_e2" $ toL (R (Combo 2 (Cons' 3 (Cons' 5 Nil')))) `shouldBe` aList
> ch06_01_e3 = it "ch06_01_e3" $ (toL . fromL) aList
>                                   `shouldBe` aList
> ch06_01_e4 = it "ch06_01_e4" $ (fromL . toL) (R (Combo 2 (Cons' 3 (Cons' 5 Nil'))))
>                                   `shouldBe`  R (Combo 2 (Cons' 3 (Cons' 5 Nil')))

translation functions in one type:

> data EP d r = EP
>   { from :: d -> r
>   , to   :: r -> d
>   }

------------------------------------------------------------------------------

Writing a datatype-generic function

Group 'U' 'Choice' 'Combo' into one type (using GADTs)

> data TypeRep t where
>   RUnit   ::                           TypeRep U
>   RChoice :: TypeRep a -> TypeRep b -> TypeRep (Choice a b)
>   RCombo  :: TypeRep a -> TypeRep b -> TypeRep (Combo a b)
>   RInt    ::                           TypeRep Int
>   RType   :: EP d r    -> TypeRep r -> TypeRep d

Previously

  type RList a = Choice U (Combo a (List' a))

Now same thing but based on the TypeRep constructors.

> -- the list representation together and toL and fromL
> rList :: TypeRep a -> TypeRep (List' a)
> rList tr = RType (EP fromL toL)
>                  (RChoice RUnit
>                  (RCombo tr (rList tr)))

1st argument (TypeRep a) guides the type resolution of List', e.g.,  (TypeRep Int) results in:

   TypeRep Int -> TypeRep (List' Int)

but

  rList (TypeRep Int) –- INVALID

need RInt:

  rList RInt          –- VALID

also need constructors for RFloat, RDouble, RChar, etc.

generic gSize : parameterized by
- type representation and
- instance of the type

> gSize :: TypeRep a -> a -> Int
> gSize  RUnit             U          = 0
> gSize (RChoice trA   _) (L a)       = gSize trA a
> gSize (RChoice   _ trB) (R b)       =               gSize trB b
> gSize (RCombo  trA trB) (Combo a b) = gSize trA a + gSize trB b
> gSize  RInt              _          = 1
> gSize (RType ep tr)      t          = gSize tr (from ep t)

> ch06_01_e5 = it "ch06_01_e5" $ gSize (rList RInt) aList `shouldBe` 3

equational reasoning:

   gSize (rList RInt) aList
   gSize (RType ep listRep) aList
   gSize listRep (from ep aList)

  -- substitute listRep, apply 'from' to aList
  gSize (RChoice RUnit (RCombo RInt (rList RInt)))
        R (Combo 2 (Cons' 3 (Cons' 5 Nil')))

  -- choose the 2nd type-rep because of R in list rep
  gSize (RCombo RInt (rList RInt))
        (Combo  2    (Cons' 3 (Cons' 5 Nil'))

  –- add the matching type-rep and list-rep pairs
  (gSize RInt 2)
         + (gSize (rList RInt) (Cons' 3 (Cons' 5 Nil'))

  –- evalulate (gSize RInt 2)
  1  + (gSize (rList RInt) (Cons' 3 (Cons' 5 Nil'))
  ...

TypeRep constructors guide pattern matching of data contents

------------------------------------------------------------------------------

Adding a new datatype does not require changing generic functions (but might want to override for other reasons).

Given RTree (above), then:


> fromT :: Tree a -> RTree a
> fromT (Leaf x)                     = L (Combo U x)
> fromT (Node x lt rt)               = R (Combo x (Combo lt rt))
>
> toT   :: RTree a -> Tree a
> toT   (L (Combo U x))              = Leaf x
> toT   (R (Combo x (Combo lt rt)))  = Node x lt rt
>
> rTree :: TypeRep a -> TypeRep (Tree a)
> rTree tr =
>   RType (EP fromT toT)
>         (RChoice (RCombo RUnit tr)
>                  (RCombo tr
>                          (RCombo (rTree tr)
>                                  (rTree tr))))

> ch06_01_e6 = it "ch06_01_e6" $ gSize (rTree RInt) intTree `shouldBe` 5

Can now all generic functions that use underlying representation.

------------------------------------------------------------------------------

GHC.Generics – a generic deriving mechanism

2010 introduced to synthesize (and surpass) the Derivable type-classes of Haskell 98
- A Generic Deriving Mechanism for Haskell by Magalhaes et al, 2010

Generic deriving mechanism was based sum of products approach.  It supports autoderiving of user-defined types.

------------------------------------------------------------------------------

> ch06_01_Test = describe "Ch06_01_Test" $ do
>   ch06_01_e1
>   ch06_01_e2
>   ch06_01_e3
>   ch06_01_e4
>   ch06_01_e5
>   ch06_01_e6
