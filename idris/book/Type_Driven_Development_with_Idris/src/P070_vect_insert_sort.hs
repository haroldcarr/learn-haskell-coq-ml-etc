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

sort
  :: forall n elem. (KnownNat n, Ord elem)
  => Vector n elem
  -> Vector n elem
sort x =
  let l  = V.toList x
      sl = L.sort l
      v0 = V0.fromList sl
  in withSized v0 $ \(v :: Vector m elem) ->
    case sameNat (Proxy @m) (Proxy @n) of
      Nothing   -> error "NO"
      Just Refl -> v :: Vector m elem


(Just x) = V.fromList [1,3,9,7,6,4,5,8] :: Maybe (Vector 8 Int)
s = sort x

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

i = insert (2::Int) s :: Vector 9 Int


