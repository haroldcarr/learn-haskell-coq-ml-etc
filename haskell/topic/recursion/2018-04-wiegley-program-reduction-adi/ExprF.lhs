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
> import           Fix
>
> {-# ANN module "HLint: ignore Use record patterns"           #-}
>
> data Atom = AInt Int | ABool Bool deriving (Eq, Ord, Show)
>
> data ExprF r
>   = Const Atom
>   | Add r r
>   | If r r r
>   deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
>
> type Expr = Fix ExprF

------------------------------------------------------------------------------

> e1 :: Fix ExprF
> e1 = Fix (Add
>            (Fix (If (Fix (Const (ABool True)))
>                     (Fix (Add (Fix (Const (AInt 2)))
>                               (Fix (Const (AInt 3)))))
>                     (Fix (Add (Fix (Const (AInt (-2))))
>                               (Fix (Const (AInt (-3))))))))
>            (Fix (Const (AInt 3))))
>
> e2 :: Fix ExprF
> e2 = Fix (If (Fix (Const (ABool False)))  e1 (Fix (Const (AInt 4))))
