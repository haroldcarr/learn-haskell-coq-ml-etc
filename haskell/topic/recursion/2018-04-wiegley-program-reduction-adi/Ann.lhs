> {-# LANGUAGE DeriveFunctor          #-}
> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
> {-# LANGUAGE ViewPatterns           #-}
>
> module Ann where
>
> import           Control.Arrow           ((&&&))
> import qualified Text.PrettyPrint.Leijen as PP
> ------------------------------------------------------------------------------
> import           ExprF
> import           Fix
> import           RS

example: Annotating
-------------------

- use-case: storing intermediate values

Use a *cofree comonad* structure `Ann f a`
- to annotate nodes of type `f`
- with attributes of type `a`.

> -- | Annotate (f r) with attribute a
> newtype AnnF f a r = AnnF (f r, a) deriving (Functor, Show)

> -- | Annotated fixed-point type. A cofree comonad.
> type Ann f a = Fix (AnnF f a)

> -- | annotation constructor
> ann :: (f (Ann f a), a) -> Ann f a
> ann = Fix . AnnF

> -- | annotation deconstructor
> unAnn :: Ann f a -> (f (Ann f a), a)
> unAnn (unFix -> AnnF a) = a

> -- | root node annotation
> rootAnn :: Ann f a -> a
> rootAnn (unFix -> AnnF (_, a)) = a

> -- | remove root annotation
> rmRootAnn :: Ann f a -> f (Ann f a)
> rmRootAnn (unFix -> AnnF (x, _)) = x

> -- | remove annotations
> rmAllAnn :: Functor f => Ann f a -> Fix f
> rmAllAnn = cata alg where
>   alg (AnnF (x, _)) = Fix x

------------------------------------------------------------------------------

*synthesized* attributes via bottom-up traversal using cata

> synthesize
>   :: forall f a
>    . Functor f
>   => (f a -> a)
>   -> Fix f
>   -> Ann f a
> synthesize f = cata alg where
>   alg :: f (Ann f a) -> Ann f a
>   alg = ann . (id &&& f . fmap rootAnn)

example: annotating each node with the sizes of all subtrees:

> sizes :: (Functor f, Foldable f) => Fix f -> Ann f Int
> sizes = synthesize $ (+1) . sum

pprAnn $ sizes e1

((ifNeg ABool True @ 1
    then (AInt   2  @ 1 + AInt   3  @ 1) @ 3
    else (AInt (-2) @ 1 + AInt (-3) @ 1) @ 3) @ 8
 + AInt 3 @ 1) @ 10

------------------------------------------------------------------------------

*inherited* attributes : top-down from an initial value

- can still use a cata/para by using a higher-order carrier
- the bottom-up traversal happens top-down when the built function is run

> inherit
>   :: forall f a
>    . Functor f
>   => (Fix f -> a -> a)
>   -> a
>   -> Fix f
>   -> Ann f a
> inherit f root n0 = para alg n0 root where
>   alg :: f (a -> Ann f a, Fix f) -> a -> Ann f a
>   alg (funzip -> (ff, n)) p = ann (n', a)
>     where
>       a  = f (Fix n) p
>       n' = fmap ($ a) ff

example : compute the depth of all subtrees

> depths :: Functor f => Fix f -> Ann f Int
> depths = inherit (const (+1)) 0

pprAnn $ depths e1
((ifNeg ABool True @ 3
    then (AInt 2 @ 4 + AInt 3 @ 4) @ 3
    else (AInt (-2) @ 4 + AInt (-3) @ 4) @ 3) @ 2
 + AInt 3 @ 2) @ 1

note: could combine `synthesize` and `inherit` algebras and do both in one traversal

> ppr :: Expr -> PP.Doc
> ppr = cata pprAlg
>
> pprAlg :: ExprF PP.Doc -> PP.Doc
> pprAlg (Const c)  = PP.text $ show c
> pprAlg (Add x y)  = PP.parens $ x PP.<+> PP.text "+" PP.<+> y
> pprAlg (If t x y) = PP.parens $
>                       PP.text        "ifNeg" PP.<+> t
>                       PP.<+> PP.text "then"  PP.<+> x
>                       PP.<+> PP.text "else"  PP.<+> y
>
> pprAnn :: PP.Pretty a => Ann ExprF a -> PP.Doc
> pprAnn = cata alg where
>   alg (AnnF (d, a)) = pprAlg d PP.<+> PP.text "@" PP.<+> PP.pretty a

