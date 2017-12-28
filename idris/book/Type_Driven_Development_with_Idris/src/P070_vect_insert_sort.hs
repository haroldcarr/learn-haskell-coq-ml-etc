{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module P070_vect_insert_sort where

import qualified Data.List          as L
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector        as V0
import           Data.Vector.Sized  as V
import           GHC.TypeLits

x = V.generate id :: Vector 7 Int

insert :: forall len len1 elem. (KnownNat len, KnownNat len1, Ord elem)
       => elem
       -> Vector len elem
       -> Vector len1 elem
insert x y =
  let l  = V.toList y
      sl = L.insert x l
      v0 = V0.fromList sl
  in withSized v0 $ \(v :: Vector n elem) ->
    case sameNat (Proxy @len1) (Proxy @n) of
      Nothing   -> error "NO"
      Just Refl -> v :: Vector len1 elem

i = insert (2::Int) x :: Vector 8 Int

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

