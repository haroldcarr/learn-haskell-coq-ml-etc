{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module P070_vect_insert_sort where
{-
import Data.Vector.Sized as V
import GHC.TypeLits

insert :: (KnownNat len, Ord elem)
       => elem
       -> Vector len elem
       -> Vector (len + 1) elem
insert x y0
  | V.length y0 == 0 = x `V.cons` y0
  | otherwise =
      let y  = undefined -- V.head y0
          xs = undefined -- V.tail y0
      in if x < y then x `V.cons` (y `V.cons` xs)
         else          y `V.cons` insert x xs

insSort ::  Ord elem
        => Vector n elem
        -> Vector n elem
insSort x0
  | V.length x0 == 0 = x0
  | otherwise =
      let x  = V.head x0
          xs = V.tail x0
      in let xsSorted = insSort xs in insert x xsSorted
-}
{-
insSort [1,3,2,9,7,6,4,5,8]
-}

