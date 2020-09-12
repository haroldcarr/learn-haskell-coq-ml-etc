> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE Rank2Types         #-}
>
> module Ch06_03_generics_scrap_your_boilerplate where
>
> import Data.Maybe
> import Data.Typeable
> import Test.Hspec

Scrap your boilerplate (SYB)
- another approach to datatype-generic programming
- generic functions over a "universal" type representation

differs from SOP and origami : the type representation is obfuscated from the user

example: generic traversal over a complex nested data structure:

> type Title   = String
> type Para    = String
> data Book    = Book    Title [Chapter] deriving (Eq, Show, Typeable)
> data Chapter = Chapter Title [Section] deriving (Eq, Show, Typeable)
> data Section = Section Title [Para]    deriving (Eq, Show, Typeable)
>
> haskellDP = Book "Haskell Design Patterns" bChapters
> bChapters = [Chapter "Chapter 1" sections1, Chapter "Chapter 2" sections2]
> sections1 = [Section "1.1" ["S1"],   Section "1.2" ["S1.2.1", "S1.2.2"]]
> sections2 = [Section "2.1" ["S2.1"], Section "2.2" ["S2.2.1", "S2.2.2"]]

want to apply fSection to all sections

(practical note: use Lens : can precisely target a function to elements in a complex structure)

> fSection (Section _t lines') = Section "!!!" lines'

> ch06_03_e1 = it "ch06_03_e1" $ fSection (Section "1.1" ["S1"]) `shouldBe` Section "!!!" ["S1"]

strategy : change fSection so it can be applied to all parts, ignoring some parts.

type-safe cast with typeable
- do a type comparison to check whether an element of a book is in fact a section.

Data.Typeable has a type-safe cast function.

autoderive Typeable (since GHC 7.8, the compiler does not allow implement Typeable ourselves)

need {-# LANGUAGE DeriveDataTypeable #-} and import Data.Typeable

Typeable.cast in action:

> ch06_03_e2 = it "ch06_03_e2" $ (cast 'a' :: Maybe Char) `shouldBe` Just 'a'
> ch06_03_e3 = it "ch06_03_e3" $ (cast 'a' :: Maybe Int)  `shouldBe` Nothing
> ch06_03_e4 = it "ch06_03_e4" $ (cast (Section "t" ["s1", "s2"]) :: Maybe Section) `shouldBe`
>                                 Just (Section "t" ["s1", "s2"])
> ch06_03_e5 = it "ch06_03_e5" $ (cast (Book "title" []) :: Maybe Section) `shouldBe` Nothing
> ch06_03_e6 = it "ch06_03_e6" $ (cast (++ "a") :: Maybe String) `shouldBe` Nothing
>
> -- No 'Show' for functions, so use isJust
> ch06_03_e7 = it "ch06_03_e7" $ isJust (cast (++ "a") :: Maybe (String -> String)) `shouldBe` True

Type-safe function application

> typesafeF :: (Typeable a, Typeable b) => (b -> b) -> a -> a
> typesafeF f = fromMaybe id (cast f)

> ch06_03_e8 = it "ch06_03_e8" $ typesafeF (+(1::Int)) (3::Int) `shouldBe`  4
> ch06_03_e9 = it "ch06_03_e9" $ typesafeF (+(1::Int)) "3"      `shouldBe` "3"


lift 'fSection' into a type-safe function that can be applied to any part of a 'Book'
(or any Typeable type)

> ch06_03_e10 = it "ch06_03_e10" $ typesafeF fSection (Section "1.1" ["s1", "s2"]) `shouldBe`
>                                                      Section "!!!" ["s1", "s2"]
> ch06_03_e11 = it "ch06_03_e11" $ typesafeF fSection (Book "title" []) `shouldBe`
>                                                      Book "title" []

The shallow traversal and the data type-class

Previous sum of products and origami examples used shallow recursion.

Explore traversing Book using a shallow approach.

gmap f (Chapter title sections)
  = Chapter (f title) (f sections)
-- INVALID

The function cannot handle all the types it is being applied to.
Need Rank2Types to define a gmap function that accepts a generic mapping function.

Make gmap a part of the Data' type-class to be implemented by all the types we plan to
traverse over:

> -- Data' inherits from Typeable
> class Typeable a => Data' a where
>   gmap :: (forall b. Data' b => b -> b) -> a -> a
>
> -- note the shallow recursion in gmap implementations...
>
> instance Data' Book where
>   gmap f (Book    title chapters) = Book    (f title) (f chapters)
> instance Data' Chapter where
>   gmap f (Chapter title sections) = Chapter (f title) (f sections)
> instance Data' Section where
>   gmap f (Section title paras   ) = Section (f title) (f paras)
> instance Data' a => Data' [a] where
>   gmap _ []     = []
>   gmap f (x:xs) = f x : f xs
> instance Data' Char where
>   gmap _ c = c

> ch06_03_e12 = it "ch06_03_e12" $ gmap (typesafeF fSection)
>                                       (Chapter "The building blocks" sections1) `shouldBe`
>                                        Chapter "The building blocks" sections1
> ch06_03_e13 = it "ch06_03_e13" $ gmap (typesafeF fSection) sections1 `shouldBe`
>                                  [Section "!!!" ["S1"],Section "1.2" ["S1.2.1","S1.2.2"]]


- traverse a chapter : sections not reached
- traverse list of sections : only first is affected

gmap f (section1: sections)
–-   = f section1 : f sections
–-    f sections = f sections
–-               = f [Section]
--               = id [Section]

because of shallow traversal of the gmap function.

advantage : use it with different kinds of recursions:

> -- bottom-up traversal: traverse x before applying f
> traverseBU :: Data' a =>(forall b . Data' b => b->b) -> a -> a
> traverseBU f x = f (gmap (traverseBU f) x)

> -- top-down traversal: apply f x then traverse
> traverseTD :: Data' a =>(forall b . Data' b => b->b) -> a -> a
> traverseTD f x = gmap (traverseTD f) (f x)

now traversals will reach all Sections

> ch06_03_e14 = it "ch06_03_e14" $ traverseBU (typesafeF fSection)
>                                             (Chapter "The building blocks" sections1)
>                                  `shouldBe`
>   Chapter "The building blocks" [Section "!!!" ["S1"],Section "!!!" ["S1.2.1","S1.2.2"]]
> ch06_03_e15 = it "ch06_03_e15" $ traverseBU (typesafeF fSection) sections1 `shouldBe`
>                                  [Section "!!!" ["S1"],Section "!!!" ["S1.2.1","S1.2.2"]]
> ch06_03_e16 = it "ch06_03_e16" $ traverseTD (typesafeF fBook) haskellDP `shouldBe`
>   Book "!!!" [Chapter "Chapter 1" [Section "1.1" ["S1"],Section "1.2" ["S1.2.1","S1.2.2"]]
>              ,Chapter "Chapter 2" [Section "2.1" ["S2.1"],Section "2.2" ["S2.2.1","S2.2.2"]]]
>  where fBook (Book _ chapters) = Book "!!!" chapters

gmap + typesafeF enable delivering type-specific functions deeply into nested structures.

"Recursive traversal in two steps — first define a one-layer map, and
then tie the recursive knot separately...  we call it 'the non-recursive map trick'"
-- Lammel and Peyton Jones, 2003
   Scrap your Boilerplate:  A Practical Design Pattern for Generic Programming

Typeable and data

data' type-class above mimics Haskell's Data type-class, which is based on more general gfoldl.

SYB relies on compiler to auto-generate instances of Typeable and Data.
Purposeful hiding of the underlying structure.
Contrasts with the sum of products and origami styles, which expose it.

SYB API is generic combinators based on Typeable and Data.
- Typeable provides the backend of SYB
- data provides the frontend

Scrap your boilerplate

Lammel and Peyton Jones cover:

* Generalizing to traversals that transform the shape of a data structure ("queries")
* Generalizing the monadic traversals
* Unifying all of the preceding techniques with a generic fold

SYB was extended to enable extensibility : being able to override generic behavior
for a specific type.

Uniplate is a simplified (and less powerful) phrasing of SYB.

Uniplate library has since been embedded in the Lens library.

------------------------------------------------------------------------------

> ch06_03_Test = describe "Ch06_03_Test" $ do
>   ch06_03_e1
>   ch06_03_e2
>   ch06_03_e3
>   ch06_03_e4
>   ch06_03_e5
>   ch06_03_e6
>   ch06_03_e7
>   ch06_03_e8
>   ch06_03_e9
>   ch06_03_e10
>   ch06_03_e11
>   ch06_03_e12
>   ch06_03_e13
>   ch06_03_e14
>   ch06_03_e15
>   ch06_03_e16
