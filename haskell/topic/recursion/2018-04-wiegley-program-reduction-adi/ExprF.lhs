> {-# LANGUAGE DeriveFoldable         #-}
> {-# LANGUAGE DeriveFunctor          #-}
> {-# LANGUAGE DeriveTraversable      #-}
> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module ExprF where
>
> import           Data.Functor.Classes  (Eq1(..), Show1(..))
>
> {-# ANN module "HLint: ignore Use record patterns"           #-}
>
> data ExprF r = Const Int
>              | Var   Id
>              | Add   r r
>              | Mul   r r
>              | IfNeg r r r
>                deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

> instance Eq1 ExprF where
>   liftEq _  (Const il)       (Const ir)       = il == ir
>   liftEq _  (Const  _)        _               = False
>   liftEq _           _       (Const  _)       = False
>   liftEq _  (Var il)         (Var ir)         = il == ir
>   liftEq _  (Var  _)          _               = False
>   liftEq _           _       (Var  _)         = False
>   liftEq eq (Add l1 l2)      (Add r1 r2)      = eq l1 r1 && eq l2 r2
>   liftEq _  (Add  _  _)       _               = False
>   liftEq _           _       (Add  _  _)      = False
>   liftEq eq (Mul l1 l2)      (Mul r1 r2)      = eq l1 r1 && eq l2 r2
>   liftEq _  (Mul  _  _)       _               = False
>   liftEq _                _  (Mul  _  _)      = False
>   liftEq eq (IfNeg l1 l2 l3) (IfNeg r1 r2 r3) = eq l1 r1 && eq l2 r2 && eq l3 r3

> -- TODO
> instance Show1 ExprF where
>   liftShowsPrec _  _ _  (Const _)     = showString "Const"
>   liftShowsPrec _  _ _  (Var   _)     = showString "Var"
>   liftShowsPrec _  _ _  (Add   _ _)   = showString "Add"
>   liftShowsPrec _  _ _  (Mul   _ _)   = showString "Mul"
>   liftShowsPrec _  _ _  (IfNeg _ _ _) = showString "IfNeg"

> type Id = String
