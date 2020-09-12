> module Ch06_02_generics_origami_recursion_schemes_fix where
>
> import Test.Hspec

origami programming : focuses on patterns of recursion: map, fold, and unfold.

Tying the recursive knot via Fix:

> data List' a = Nil'   | Cons' a (List' a)
> data Tree  a = Leaf a | Node  a (Tree  a) (Tree a)

> -- s represents the shape
> -- a refers to an instance of the type
> newtype Fix s a = FixT { getFix :: s a (Fix s a) }

named after a fixed point of a function: f (fix f) = fix f

Tree / List via Fix (i.e., implicit recursion notated 'r')

> data List_ a r = Nil_    | Cons_ a r   deriving (Eq, Show)
> data Tree_ a r = Leaf_ a | Node_ a r r deriving (Eq, Show)

> type ListF a = Fix List_ a
> type TreeF a = Fix Tree_ a

Fix ties the recursive knot around the shape (see Datatype-Generic Programming by Gibbons).

> aList1 = Cons_ 12 Nil_            -- :: List_ Integer                (List_ a r)
> aList2 = Cons_ 12 (Cons_ 13 Nil_) -- :: List_ Integer (List_ Integer (List_ a r))

to construct ListF lists, wrap FixT around each nesting:

> aListF = FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_)))) -- :: ListF Integer -- Fix List_ Integer

------------------------------------------------------------------------------

write generic functions against Fix type.

map for fixed recursive type ListF:

> mapL f listF = case list_ of
>   Cons_ x r -> FixT $ Cons_ (f x) (mapL f r)
>   Nil_      -> FixT Nil_
>  where
>   list_ = getFix listF

> showListF :: (Show a) => ListF a -> String
> showListF (FixT (Cons_ x r)) = show x ++ ", " ++ showListF r
> showListF (FixT  Nil_)       = "Nil_"

> ch06_02_e1 = it "ch06_02_e1" $ showListF (mapL (*2) aListF) `shouldBe` "24, 26, Nil_"

clumsy : need to un/wrap

> class Bifunctor s where
>   bimap :: (a -> c) -> (b -> d) -> (s a b -> s c d)
>
> instance Bifunctor List_ where
>   bimap _ _ Nil_        = Nil_
>   bimap f g (Cons_ x r) = Cons_ (f x) (g r)
>
> instance Bifunctor Tree_ where
>   bimap f _ (Leaf_ x)       = Leaf_ (f x)
>   bimap f g (Node_ x rl rr) = Node_ (f x) (g rl) (g rr)

generic map

> gmap :: Bifunctor s => (a -> b) -> Fix s a -> Fix s b
> gmap f = FixT . bimap f (gmap f) . getFix

> ch06_02_e2 = it "ch06_02_e2" $ showListF (gmap (*2) aListF) `shouldBe` "24, 26, Nil_"

generic fold

> gfold :: Bifunctor s => (s a b -> b) -> Fix s a -> b
> gfold f = f . bimap id (gfold f) . getFix

unwrap with getFix, but, instead of rewrapping, apply f.
gfold replaces the occurrences of FixT with f:

   FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))
   f    (Cons_ 12 (f    (Cons_ 13 (f    Nil_))))


> addL (Cons_ x r) = x + r
> addL Nil_        = 0

> ch06_02_e3 = it "ch06_02_e3" $ gfold addL aListF `shouldBe` 25

fold : consumer of data

unfold : producer

> unfoldL stopF nextF val =
>   if stopF val then []
>   else              val : unfoldL stopF nextF (nextF val)

> ch06_02_e4 = it "ch06_02_e4" $ unfoldL (< (-10)) (\x -> x - 1) 10 `shouldBe`
>              [10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]

generic unfold

> gunfold :: Bifunctor s => (b -> s a b) -> b -> Fix s a
> gunfold f = FixT . bimap id (gunfold f) . f

> toList 0 = Nil_
> toList n = Cons_ n (n-1)

> ch06_02_e5 = it "ch06_02_e5" $ showListF (gunfold toList 10) `shouldBe`
>             "10, 9, 8, 7, 6, 5, 4, 3, 2, 1, Nil_"

Generic unfold and fold : hylomorphisms

Composing unfold/fold : connecting a producer with a consumer

> ch06_02_e6 = it "ch06_02_e6" $ gfold addL (gunfold toList 100) `shouldBe` 5050

see ibbons' origami programming and datatype-generic programming

unfold / fold are mirror images:

  gunfold f = FixT . bimap id (gunfold f) . f
  gfold f   = f    . bimap id (gfold f)   . getFix

hylo is their composition:

> hylo f g = g . bimap id (hylo f g) . f

> ch06_02_e7 = it "ch06_02_e7" $ hylo toList addL 100 `shouldBe` 5050

Direct relationship between the shape of these patterns and shape of data they  manipulate.
"origami" because of folds and unfolds

Origami design patterns

Jeremy Gibbons demonstrates four key Gang of Four patterns captured by recursion operators:

* Composite pattern:
- recursive datatypes
- Fix (chapter 1)

* Iterator pattern
- linear access preserving structure
- applicative traversals (chapter 4)

* Visitor pattern
- polymorphic dispatch (chapter 1)
- structured traversal of a composite changing structure
- fold

* Builder pattern
- separates data construction from data representation, so same
  construction process can create different representations.
- structured construction : unfold, hylo (unfold + fold)

see Jeremy Gibbons' Design Patterns as Higher-Order Datatype-Generic Programs

------------------------------------------------------------------------------

> ch06_02_Test = describe "Ch06_02_Test" $ do
>   ch06_02_e1
>   ch06_02_e2
>   ch06_02_e3
>   ch06_02_e4
>   ch06_02_e5
>   ch06_02_e6
>   ch06_02_e7
