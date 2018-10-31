{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module RankNTypes.RNT where
{-

Rank-N types

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#arbitrary-rank-polymorphism

Contents
- Intro
- Relation to Existentials

------------------------------------------------------------------------------
Intro

Haskell '98 types are Rank-1. Equivalent:
-}
one ::             a ->            b -> a  -- implied universal quantification
two :: forall a b. a ->            b -> a
thr :: forall a  . a -> (forall b. b -> a) -- move quantification of 'b' to right of (->)
one  = const
two  = const
thr  = const
{-
Rank-2 type because can only be expressed as two levels (i.e., not merged):
'forall' appearing within left-hand side of (->) can NOT be moved up
- it forms another level or rank N
- where N is number of nested foralls that cannot be merged with a previous one
-}
rank2    ::           (forall a. a -> a) -> (forall b. b -> b)
rank2'   :: forall b. (forall a. a -> a) -> (          b -> b) -- move right one
rank2''  ::           (forall a. a -> a) ->            b -> b  -- use implied
rank2   _ = id
rank2'  _ = id
rank2'' _ = id
{-
sometimes undecidable, then us explicit types
------------------------------------------------------------------------------
Relation to Existentials

to unpack an existential type
- need polymorphic function that works on any type
  that could be stored in the existential

encoding existentials in terms of higher rank types in continuation-passing style

replace:
-}

data T a1 {- .. -} ai
  =  forall t1 {- .. -} tj
  .  (Eq t1, Eq tj, Num t1, Show tj) {- constraints .. -}
  => Constructor a1 ai t1 tj

ex :: (Int, Char)
ex  =
  let e = Constructor 1 'c' 1.0 "c"
  in case e of
    (Constructor a10 ai0 t10 tj0) -> ( if      t10 == 0   then 0   else a10
                                     , if show tj0 == "c" then 'c' else ai0
                                     )

-- with
newtype T' a1' {- .. -} ai'
  = Constructor' (forall b
                  . (forall t1' {- .. -} tj'
                     . (Eq t1', Eq tj', Num t1', Show tj') {- constraints -}
                     => a1' -> ai' -> t1' -> tj' -> b)
                  -> b)
{-
ex' =
  let e = Constructor' (\f -> f 1 'c' 1.0 "c")
  in case e of
    (Constructor' f) -> let pat1 patk = f k in _
-}
------------------------------------------------------------------------------
{-
Church-encoded Lists

Church-encoded lists use RankNTypes
http://stackoverflow.com/a/15593349/849891
-}
-- | Laws:
--
-- > runList xs cons nil == xs
-- > runList (fromList xs) f z == foldr f z xs
-- > foldr f z (toList xs) == runList xs f z
newtype ChurchList a =
    ChurchList { runList :: forall r. (a -> r -> r) -> r -> r }

-- | Make a 'ChurchList' out of a regular list.
fromList :: [a] -> ChurchList a
fromList xs = ChurchList $ \k z -> foldr k z xs

-- | Turn a 'ChurchList' into a regular list.
toList :: ChurchList a -> [a]
toList xs = runList xs (:) []

-- | The 'ChurchList' counterpart to '(:)'.
-- Unlike 'DList', whose impl uses the regular list type,
-- 'ChurchList' abstracts over it as well.
cons :: a -> ChurchList a -> ChurchList a
cons x xs = ChurchList $ \k z -> k x (runList xs k z)

-- | Append two 'ChurchList's. O(1).
-- Note : no need to materialize the lists as @[a]@.
append :: ChurchList a -> ChurchList a -> ChurchList a
append xs ys = ChurchList $ \k z -> runList xs k (runList ys k z)

-- i.e.,
nil :: ChurchList a
nil         = {- fromList []
            =    ChurchList $  \k z -> foldr k z []
            = -} ChurchList $ \_k z -> z

singleton :: a -> ChurchList a
singleton x = {- cons x nil
            =    ChurchList $ \k z -> k x (runList nil k z)
            = -} ChurchList $ \k z -> k x z

snoc :: ChurchList a -> a -> ChurchList a
snoc   xs x = {- append xs $ singleton x
            =    ChurchList $ \k z -> runList xs k (runList (singleton x) k z)
            = -} ChurchList $ \k z -> runList xs k (k x z)
