{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module P070_vect_insert_sort where

import           Data.List         as L
import           Data.Proxy
import qualified Data.Vector       as V0
import           Data.Vector.Sized as V
import           GHC.TypeLits

insert0
  :: KnownNat len
  => elem
  -> Vector len elem
  -> Vector (len + 1) elem
insert0 = V.cons

insert1 x v = l V.++ V.cons x r
  where (l, r) = V.splitAt v

x = V.replicate 1 :: Vector 7 Int
y = insert0 3 x
-- z = insert1 3 _

{-
insert :: (KnownNat len, Ord elem)
       => elem
       -> Vector len elem
       -> Vector (len + 1) elem
insert x y =
  let l  = V.toList y
      sl = L.insert x l
      v0 = V0.fromList sl
  in withSized v0 $ \(v :: Vector n elem) -> v

insert :: (KnownNat len, Ord elem)
       => elem
       -> Vector len elem
       -> Maybe (Vector (len + 1) elem)
insert x y =
  let l = V.toList y
      s = L.insert x l
  in case someNatVal (fromIntegral (L.length s)) of
    Nothing -> Nothing
    Just (SomeNat (Proxy :: Proxy m)) -> V.fromList s :: Maybe (Vector m elem)
-}

{-
insSort ::  Ord elem
        => Vector n elem
        -> Vector n elem
insSort x0
  | V.length x0 == 0 = x0
  | otherwise =
      let x  = V.head x0
          xs = V.tail x0
      in let xsSorted = insSort xs in insert x xsSorted


insSort [1,3,2,9,7,6,4,5,8]
-}

