{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
--{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module NS where

import           NP

data NS :: (k -> *) -> [k] -> * where
  Z :: f x -> NS f (x ': xs)
  S :: NS f xs -> NS f (x ': xs)

deriving instance All (Compose Show f) xs => Show (NS f xs)

type ExampleChoice = NS I '[Int, Bool, Char]

e0 :: ExampleChoice
e0 = Z (I 3)

e1 :: ExampleChoice
e1 = S (Z (I False))

e2 :: ExampleChoice
e2 = S (S (Z (I 'c')))

type Rep a = NS (NP I) (Code a)

class Generic a where
  type Code a :: [[*]]
  from :: a -> Rep a
  to   :: Rep a -> a

data Boolean = Tr | Fa

instance Generic Boolean where
  type Code Boolean = '[ '[] , '[] ]

  from Fa =    Z Nil
  from Tr = S (Z Nil)

  to    (Z Nil)     = Fa
  to (S (Z Nil)) = Tr
  to (S (S _))   = undefined

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)

instance Generic (BinTree a) where
  type Code (BinTree a) = '[ '[ a ] , '[ BinTree a, BinTree a ] ]

  from (Leaf x)   = Z (I x :* Nil)
  from (Node l r) = S (Z (I l :* I r :* Nil))

  to    (Z (I x :*        Nil))  = Leaf x
  to (S (Z (I l :* I r :* Nil))) = Node l r
  to (S (S _))                   = undefined
