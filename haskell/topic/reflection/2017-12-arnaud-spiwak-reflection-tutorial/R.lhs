> {-# LANGUAGE FlexibleContexts     #-}
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module R where
>
> import Data.Proxy
> import Data.Reflection

http://www.tweag.io/posts/2017-12-21-reflection-tutorial.html

All about reflection: a tutorial

Arnaud Spiwack  |  21 December 2017

type class reflection

Type class reflection
- Haskell extension
- enables using a value as a type class instance
- https://www.stackage.org/haddock/lts-9.0/reflection-2.1.2/Data-Reflection.html


Example: sort a list

> newtype SortedList a = Sorted [a] -- just tags it, no REAL sorted enforcement
>
> forget :: SortedList a -> [a]
> forget (Sorted l) = l
>
> nil :: SortedList a
> nil = Sorted []
>
> singleton :: a -> SortedList a
> singleton a = Sorted [a]
>
> merge :: Ord a => SortedList a -> SortedList a -> SortedList a
> merge (Sorted left0) (Sorted right0) = Sorted $ mergeList left0 right0
>   where
>     -- 'mergeList l1 l2' returns a sorted permutation of 'l1++l2' provided
>     -- that 'l1' and 'l2' are sorted.
>     mergeList :: Ord a => [a] -> [a] -> [a]
>     mergeList []         right       = right
>     mergeList left                [] = left
>     mergeList left@(a:l) right@(b:r) =
>         if a <= b then a : mergeList l    right
>         else           b : mergeList left r

Ord needed to define merge.

Type classes are global and coherent
- there is only one 'Ord a' instance
- guaranteed that merge always uses the same comparison function for 'a'.

If 'Ord a 'holds, then 'SortedList a' represents lists of 'a' sorted according to order defined by unique 'Ord a' instance.

In contrast, fun arg defining an order is local to fun call:

  sortBy :: (a->a->Ordering) -> [a] -> [a]

therefore could change order for each call - could not state that 'SortedList a' are sorted.

> -- | Sort lists with the SortedList interface
> fromList :: Ord a => [a] -> SortedList a
> fromList  [] = nil
> fromList [a] = singleton a
> fromList  l  = merge orderedLeft orderedRight   -- 3. then merge the parts
>   where
>     orderedLeft  = fromList left                -- 2. recursively sort parts
>     orderedRight = fromList right
>     (left,right) = splitAt (div (length l) 2) l -- 1. split in two
>
> sort :: Ord a => [a] -> [a]
> sort l = forget (fromList l)

Note: sort can be defined in terms of sortBy:

> {-# ANN sortX ("HLint: ignore Use sort" :: String) #-}
> sortX :: Ord a => [a] -> [a]
> sortX = sortByX compare

But want to implement sortBy in terms of sort

> sortByX :: (a->a->Ordering) -> [a] -> [a]
> sortByX _ _ = undefined

Needed type class for type safety of 'SortedList' interface.

USE A VALUE AS A TYPE CLASS INSTANCE.

again : property of type classes : globally attached to a type.

Cannot do:
  sortBy      myOrd :: [a] -> [a]
  sortBy myOtherOrd :: [a] -> [a]
because two different instances of Ord a.

Instead create a new type each time an order is needed for 'a'

  newtype ReflectedOrd a = ReflectOrd a

But cannot do a newtype on each dynamic call to sortBy. Instead:

> newtype ReflectedOrd s a = ReflectOrd a
>
> {-# ANN reflectOrd ("HLint: ignore Eta reduce" :: String) #-}
> -- | Like `ReflectOrd` but takes a `Proxy` argument to help GHC with unification
> reflectOrd :: Proxy s -> a -> ReflectedOrd s a
> reflectOrd _ a = ReflectOrd a
>
> unreflectOrd :: ReflectedOrd s a -> a
> unreflectOrd (ReflectOrd a) = a

Need to create param 's'  at each sortBy call:

  reifyOrd :: (forall s. Ord (ReflectedOrd s a) => …) -> …

reifyOrd
- takes arg that works for any 's'
- if each call uses a different 's', program still correctly typed
- not actually creating types: but can reason as if we were
- 'ReflectOrd s1 a' and 'ReflectOrd s2 a' are seen as two distinct types
- therefore their Ord instance can be different
- This is called a RANK 2 QUANTIFICATION

to export a single reify function, rather than one for every type class,
the reflection package introduces a generic type class:

  reify :: forall d r. d -> (forall s. Reifies s d => Proxy s -> r) -> r

- d           : like a dictionary for Ord
- Reifies s d : way to retrieve that dictionary
- Proxy s     : satisfy the type-checker

's' can be seen as a unique generated type that is valid only in the scope of the reify function.

  class Reifies s d | s -> d where
    reflect :: proxy s -> d

| s -> d
- functional dependency : a given 's' uniquely determines 'd'
- used by GHC to decide which type class instance to use

Sorting with reflection

use reflection to give Ord instance to ReflectedOrd

need a dictionary for Ord
- in order to build Ord instance
  - need equality function for Eq subclass
  - comparison function for the instance proper

> data ReifiedOrd a = ReifiedOrd
>   { reifiedEq      :: a -> a -> Bool
>   , reifiedCompare :: a -> a -> Ordering
>   }

Given a dictionary of type ReifiedOrd
- can define instances for Eq and Ord of ReflectedOrd

type class instances only take type class instances as an argument, so
- need to provide the dictionary as a type class, using Reifies:

> instance Reifies s (ReifiedOrd a) => Eq (ReflectedOrd s a) where
>   (==)    (ReflectOrd x) (ReflectOrd y) = reifiedEq      (reflect (Proxy :: Proxy s)) x y
>
> instance Reifies s (ReifiedOrd a) => Ord (ReflectedOrd s a) where
>   compare (ReflectOrd x) (ReflectOrd y) = reifiedCompare (reflect (Proxy :: Proxy s)) x y

because of the Reifies on the left of the instances
- GHC does not know that it will for sure terminate during type class resolution (therefore: UndecidableInstances)

But these are global instances: by definition, they are the only way to have an Ord instances on the ReflectedOrd type.

When : reify a ReifiedOrd a
- have a scoped instance of Ord (ReflectedOrd s a) (for some locally generated s)

To sort list, need to convert between [a] and ReflectedOrd s a.

> sortBy :: (a->a->Ordering) -> [a] -> [a]
> sortBy ord l =
>   reify (fromCompare ord) $ \p ->
>     map unreflectOrd . sort . map (reflectOrd p) $ l
>
> -- | Creates a `ReifiedOrd` with a comparison function. The equality function
> --   is deduced from the comparison.
> fromCompare :: (a -> a -> Ordering) -> ReifiedOrd a
> fromCompare ord = ReifiedOrd
>   { reifiedEq      = \x y -> ord x y == EQ
>   , reifiedCompare = ord
>   }

in sortBy definition
- map reflectOrd/unreflectOrd to convert between a and ReflectedOrd s a
- reflectOrd/unreflectOrd have no computational cost
- but using them in combination with map will cost the traversal of the list
- to avoid: learn about Coercible type class: start with video by Simon Peyton Jones
  - https://skillsmatter.com/skillscasts/5296-safe-zero-cost-coercions-in-haskell

Use safety of type classes AND having flexibility to instantiate the type class from a function argument.
(e.g., CLI options).

- talk by Edward Kmett, author of reflection package, on importance of global coherence of type classes and about reflection
  - https://www.youtube.com/watch?v=hIZxTQP1ifo
- tutorial by Austin Seipp goes over the very unsafe, internal compiler representation dependent, implementation of the library
  - https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
- John Wiegley : application of reflection with QuickCheck
  - http://newartisans.com/2017/02/a-case-of-reflection/

