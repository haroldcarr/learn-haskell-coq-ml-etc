{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module P070_vect_insert_sort where

import qualified Data.List          as L
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector        as V
import qualified Data.Vector.Sized  as VS
import           GHC.TypeLits

insert
  :: forall len len1 elem. (KnownNat len1, Ord elem)
  => elem -> VS.Vector len elem
  -> VS.Vector len1 elem
insert x y =
  let v0 = V.fromList (L.insert x (VS.toList y))
  in VS.withSized v0 $ \(v :: VS.Vector n elem) ->
    case sameNat (Proxy @len1) (Proxy @n) of
      Nothing   -> error "NO"
      Just Refl -> v :: VS.Vector len1 elem

insSort
  :: forall n elem. (KnownNat n, Ord elem)
  => VS.Vector n elem
  -> VS.Vector n elem
insSort x =
  let v0  = (V.fromList . L.sort . VS.toList) x
  in VS.withSized v0 $ \(v :: VS.Vector m elem) ->
    case sameNat (Proxy @m) (Proxy @n) of
      Nothing   -> error "NO"
      Just Refl -> v :: VS.Vector m elem

exis :: VS.Vector 8 Int
exis  = case VS.fromList [1,3,9,7,6,4,5,8] :: Maybe (VS.Vector 8 Int) of
          Nothing  -> VS.replicate 0
          Just vxs -> insSort vxs

exi :: VS.Vector 9 Int
exi  = insert (2::Int) exis :: VS.Vector 9 Int
