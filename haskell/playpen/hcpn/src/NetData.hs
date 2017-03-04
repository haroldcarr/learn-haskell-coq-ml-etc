-- {-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE FlexibleInstances #-}
module NetData where

import           Data.Unique

-- the nets themselves (logical+graphical info)

data Net ts ps as =
  Net{trans  :: ts
     ,places:: ps
     ,arcs   :: as
     ,decls  :: String}
  deriving (Show,Read)

data NetObject i a =
  NetObject{nId::i,object::a}
  deriving (Show,Read)

instance Eq (NetObject Unique a) where
  no == no' = nId no == nId no'

instance Show Unique where
  showsPrec d u = showsPrec d $ hashUnique u

data Arc p t pos =
    PT p t String [pos]
  | TP t p String [pos] deriving (Show,Read)

mkArc aId a = NetObject aId $ a

data PartialArc p t pos =
    PPT p [pos]
  | PTP t [pos] deriving (Show,Read)

data Node n p a = Node{nName::n,nPos::p,nType::a}
  deriving (Show,Read)

mkNode nId nName nPos nType =
  NetObject nId $ Node nName nPos $ nType

data Place = Place{pType::String,pInit::String} deriving (Show,Read)

mkPlace pId pName pType pInit pt = mkNode pId pName pt $ Place pType pInit

data Trans = Trans{tGuard::String,tVert::Bool} deriving (Show,Read)

mkTrans tId tName tGuard pt = mkNode tId tName pt $ Trans tGuard False

data NodeType = T | P deriving (Show,Read,Eq)
