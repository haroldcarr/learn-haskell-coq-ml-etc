> {-# LANGUAGE DeriveFunctor          #-}
> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
> {-# LANGUAGE ViewPatterns           #-}
>
> module ANN where
>
> import Control.Arrow ((&&&))
> import qualified Text.PrettyPrint.Leijen       as PP
> import ExprF

> type Expr = Fix ExprF
>
> class Functor f => Fixpoint f t | t -> f where
>   inF  :: f t -> t
>   outF :: t -> f t
>
> instance Functor f => Fixpoint f (Fix f) where
>   inF  = Fix
>   outF = unFix
>
> newtype Fix f = Fix { unFix :: f (Fix f) }
>
> cata
>   :: Functor f
>   =>        (f a ->   a)                  -- f
>   ->                        Fix f -> a
> cata  f     =            f . fmap (cata f)   . unFix
>
> para :: Fixpoint f t => (f (a, t) -> a) -> t -> a
> para alg = alg . fmap (para alg &&& id) . outF
>
> funzip :: Functor f => f (a, b) -> (f a, f b)
> funzip = fmap fst &&& fmap snd

example: Annotating
-------------------

- useful for storing intermediate values
- inspired by ideas from *attribute grammars*

Use a *cofree comonad* structure `Ann f a` to annotate nodes of
type `f` with attributes of type `a`.

> -- | Annotate (f r) with attribute a
> newtype AnnF f a r = AnnF (f r, a) deriving Functor

> -- | Annotated fixed-point type. A cofree comonad.
> type Ann f a = Fix (AnnF f a)

> -- | Attribute of the root node
> attr :: Ann f a -> a
> attr (unFix -> AnnF (_, a)) = a

------------------------------------------------------------------------------

> -- | strip attribute from root
> strip :: Ann f a -> f (Ann f a)
> strip (unFix -> AnnF (x, _)) = x

> -- | strip all attributes
> stripAll :: Functor f => Ann f a -> Fix f
> stripAll = cata alg where
>   alg (AnnF (x, _)) = Fix x

> -- | annotation constructor
> ann :: (f (Ann f a), a) -> Ann f a
> ann = Fix . AnnF

> -- | annotation deconstructor
> unAnn :: Ann f a -> (f (Ann f a), a)
> unAnn (unFix -> AnnF a) = a

------------------------------------------------------------------------------

*Synthesized* attributes are created in a bottom-up traversal using a catamorphism.

> synthesize :: forall f a. Functor f =>
>               (f a -> a) -> Fix f -> Ann f a
> synthesize f = cata alg where
>   alg :: f (Ann f a) -> Ann f a
>   alg = ann . (id &&& f . fmap attr)

For example, annotating each node with the sizes of all subtrees:

> sizes :: (Functor f, Foldable f) => Fix f -> Ann f Int
> sizes = synthesize $ (+1) . sum
>
> ~~~
 > pprAnn $ sizes e1
 ((ifNeg (1 @ 4 * a @ 4) @ 3
    then (b @ 4 + 0 @ 4) @ 3
    else (b @ 4 + 2 @ 4) @ 3) @ 2
      * 3 @ 2) @ 1
~~~


------------------------------------------------------------------------------

*Inherited* attributes are created in a top-down manner from an initial value.

- Can still use a cata/paramorphism by using a higher-order carrier
- the bottom-up traversal happens top-down when the built function is run

> inherit :: forall f a. Functor f =>
>             (Fix f -> a -> a) -> a -> Fix f -> Ann f a
> inherit f root n0 = para alg n0 root where
>   alg :: f (a -> Ann f a, Fix f) -> a -> Ann f a
>   alg (funzip -> (ff, n)) p = ann (n', a)
>     where
>       a  = f (Fix n) p
>       n' = fmap ($ a) ff

For example, the `depths` function computes the depth of all subtrees:

> depths :: Functor f => Fix f -> Ann f Int
> depths = inherit (const (+1)) 0

> ppr :: Expr -> PP.Doc
> ppr = cata pprAlg
>
> pprAlg :: ExprF PP.Doc -> PP.Doc
> pprAlg (Const c)     = PP.text $ show c
> pprAlg (Var  i)      = PP.text i
> pprAlg (Add x y)     = PP.parens $ x PP.<+> PP.text "+" PP.<+> y
> pprAlg (Mul x y)     = PP.parens $ x PP.<+> PP.text "*" PP.<+> y
> pprAlg (IfNeg t x y) = PP.parens $
>                          PP.text        "ifNeg" PP.<+> t
>                          PP.<+> PP.text "then"  PP.<+> x
>                          PP.<+> PP.text "else"  PP.<+> y
>
> pprAnn :: PP.Pretty a => Ann ExprF a -> PP.Doc
> pprAnn = cata alg where
>   alg (AnnF (d, a)) = pprAlg d PP.<+>
>                       PP.text "@" PP.<+> PP.pretty a

~~~
 > pprAnn $ depths e1
 ((ifNeg (1 @ 4 * a @ 4) @ 3
    then (b @ 4 + 0 @ 4) @ 3
    else (b @ 4 + 2 @ 4) @ 3) @ 2
      * 3 @ 2) @ 1
~~~

Note: could combine the `synthesize` and `inherit` algebras and do both in one traversal.

> e1 :: Fix ExprF
> e1 = Fix (Mul
>            (Fix (IfNeg
>                   (Fix (Mul (Fix (Const 1))
>                             (Fix (Var "a"))))
>                   (Fix (Add (Fix (Var "b"))
>                             (Fix (Const 0))))
>                   (Fix (Add (Fix (Var "b"))
>                             (Fix (Const 2))))))
>                 (Fix (Const 3)))
>
> e2 :: Fix ExprF
> e2 = Fix (IfNeg (Fix (Var "b")) e1 (Fix (Const 4)))
