module Main

------------------------------------------------------------------------------
{-
Interfaces in Idris are similar to type classes in Haskell.
some differences
- interfaces can be parameterized by values of any type
  - not limited to types or type constructors
- interfaces can have multiple implementations

An interface describes generic operations that can be implemented
in different ways for differ ent concrete types.
-}

-- count number of occurrences of a specific value, of a generic type ty, in a list.
occurrences : Eq ty
           => (item : ty) -> (values : List ty)
           -> Nat
occurrences item [] = 0
occurrences item (value :: values) =
 case value == item of
   False => occurrences item values
   True  => 1 + occurrences item values

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

{-
occurrences 'b' ['a','a','b','b','b','c']
occurrences 100 [50,100,100,150]
occurrences Liquid [Solid, Liquid, Liquid, Gas]
-}

data Tree elem
  = Empty
  | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==)              Empty                  Empty  = True
  (==) (Node left e right) (Node left' e' right') = left == left' && e == e' && right == right'
  (==)                  _                      _  = False

{-
Cannot parameterize implementations by everything of type Type.
Limited to names introduced by either a data or record declaration, or primitive types.
Cannot parameterize implementations by type synonyms or functions that compute types.
-}

record Album where
  constructor MkAlbum
  artist : String
  title  : String
  year   : Integer

Eq Album where
  (==) (MkAlbum artist title year) (MkAlbum artist' title' year') =
         artist == artist' && title == title' && year == year'

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year') =
            case compare artist artist' of
              EQ          => case compare year year' of
                               EQ        => compare title title'
                               diff_year => diff_year
              diff_artist => diff_artist

help : Album
help = MkAlbum "The Beatles" "Help" 1965
rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965
clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969
hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971
heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977
collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]

{-
heroes > clouds
help <= rubbersoul
map title (sort collection)
-}

------------------------------------------------------------------------------
-- Interfaces defined in Prelude

{-
Show


Integer, Int, Nat, Double
interface Num ty where

Integer, Int, Double
interface Num ty => Neg ty where

Interger, Int, Nat
interface Num ty => Integral ty where

Double
interface Num ty => Fractional ty where

fromInteger : Num ty => Integer -> ty

cast : Cast from to => from -> to

map   : Functor f     => (a -> b) -> f a -> f b
pure  : Applicative f => a -> f a
(>>=) : Monad m       => m a -> (a -> m b) -> m b
-}
