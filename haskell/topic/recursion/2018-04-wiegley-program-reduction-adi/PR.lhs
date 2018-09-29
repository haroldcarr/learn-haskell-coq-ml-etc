> {-# LANGUAGE ConstraintKinds   #-}
> {-# LANGUAGE DeriveFunctor     #-}
> {-# LANGUAGE FlexibleContexts  #-}
> {-# LANGUAGE LambdaCase        #-}
> {-# LANGUAGE NoImplicitPrelude #-}
>
> module PR where
>
> import Data.Functor.Compose
> import Language.Haskell.Exts.SrcLoc
> import Universum
> import Fix

John Wiegley
Program Reduction: A Win for Recursion Schemes
http://newartisans.com/2018/04/win-for-recursion-schemes/

Nix source code parser into an AST
- pretty print
- evaluate

slight cost when using recursion schemes: management of Fix type wrappers

> data NAtom = NInt Int | NBool Bool deriving Show

places in structure where recursion is possible identified by type variable r

> data NExprF r
>   = NConstant NAtom
>   | NIf r r r
>   deriving (Functor, Show)
>
> -- Type of recursive expression trees : the  least fixed-point of the expression functor above.
> -- Recursion terminates at symbols and atomic values
> -- - any construction that doesn't reference the type variable, or only uses it conditionally.
> type NExpr = Fix NExprF

'Fix' used to express recursion
- separates info stored at each layer of AST from recursive structure of AST

use generic catamorphism to recurse over AST
- passing function that defines how each "data layer" *separately* reduces

see 'cata' definition below

> evaluate :: NExpr -> Int
> evaluate = cata $ \case
>   NConstant (NInt n)  -> n
>   NConstant (NBool b) -> if b then 1 else 0
>
>   -- 'cata' makes 'c', 't' and 'e' all Ints by the time we reach here.
>   NIf c t e -> if c /= 0 then t else e

> iff :: NExpr
> iff = Fix (NIf (Fix (NConstant (NInt 0)))
>                (Fix (NConstant (NInt 1)))
>                (Fix (NConstant (NInt 2))))
>
> ev :: Int
> ev = evaluate iff

Annotated expression trees

add location info to AST via a new fixed-point type:

> type NExprLoc = Fix (Compose ((,) SrcSpan) NExprF)

extend parser to create annotated exprs:

annotateLocation :: Parser a -> Parser (Ann SrcSpan a)
annotateLocation p = do
  begin <- getPosition
  res   <- p
  end   <- getPosition
  pure (SrcSpan begin end, res)

annotateLocation1 :: Parser (NExprF NExprLoc) -> Parser NExprLoc
annotateLocation1 = fmap (Fix . Compose) . annotateLocation

then wrap each parser for plain expressions so they become parsers for annotated expressions:

nixIf :: Parser NExprLoc
nixIf = annotateLocation1 (NIf
  <$> (reserved "if" *> nixExprLoc)
  <*> (reserved "then" *> nixToplevelForm)
  <*> (reserved "else" *> nixToplevelForm)
  <?> "if")

parsing code only module modified invasively because it used direct recursion
- requires injecting new data layer (annotation information) at each step

> ss :: SrcSpan
> ss  = SrcSpan "filename" 1 2 3 4
>
> ci0 :: NExprF r
> ci0 = NConstant (NInt 0)
>
> fcss :: g (Fix (Compose ((,) SrcSpan) g))
>      ->    Fix (Compose ((,) SrcSpan) g)
> fcss xx = Fix (Compose (ss, xx))
>
> fcssci0 :: Fix (Compose ((,) SrcSpan) NExprF)
> fcssci0 = fcss ci0
>
> ia :: NExprLoc
> ia = Fix (Compose (ss, NIf fcssci0 fcssci0 fcssci0))

for algorithms not directly recursive (i.e., reduction steps passed to cata) better way:

Abstract Definitional Interpreters (ADI) : ICFP 2017, David Darais
- how recursive code (e.g., evaluators) can be rewritten to inject new behavior
  at each layer of recursion – even changing logic of recursion in arbitrary ways

recursion schemes
- separates data from recursion
- manage layers in a general way
- ADI, same, but does it for behavior too (recursion schemes apply the idea to data)

With Conal Elliott found a way to unify the two ideas
- apply ADI approach to catamorphism-based evaluators operating on fixed-point data structures

> cata :: Functor f
>      =>       (f a ->   a)                  -- f
>      ->                       Fix f -> a
> adi  :: Functor f
>      =>       (f a ->   a)                  -- f
>      -> ((Fix f    ->   a) -> Fix f -> a)   -- g
>      ->                       Fix f -> a
> adiM :: (Traversable t, Monad m)
>      =>       (t a -> m a)                  -- f
>      -> ((Fix t    -> m a) -> Fix t -> m a) -- g
>      ->                       Fix t -> m a
> cata f   =     f .   fmap     (cata f)    . unFix
> adi  f g = g ( f .   fmap     (adi  f g)  . unFix)
> adiM f g = g ((f <=< traverse (adiM f g)) . unFix)

replacements for cata and cataM
modify the 'f' algorithm using 'g' transformation

Error reporting with context

extend original evaluator to append "stack frames" within a MonadReader
context around each evaluation step

> framedEvalExpr
>   :: MonadReader [NExprLoc] m
>   => (NExprF (m v) -> m v)
>   -> NExprLoc
>   -> m v
> framedEvalExpr eval = adi (eval . snd . getCompose) psi
>  where psi k v = withExprContext v (k v)
>
> withExprContext :: MonadReader [NExprLoc] m => NExprLoc -> m r -> m r
> withExprContext expr = local (expr :)

- takes monadic f-algebras for the original expression functor
  - those including a monad m
  - so can use Framed e m to require a MonadReader instance providing a list of frames
- transforms it into an f-algebra for location-annotated expression trees
  that reports all the locations leading up to an error whenever an exception is thrown

> -- type Framed e m = (MonadReader e m, MonadThrow m)

> em :: MonadReader [NExprLoc] m => m NExpr -> m Int
> em xxx = evaluate <$>xxx
>
> -- rttt = runReaderT (em ia) []

rest of logic happens in throwError, which queries MonadReader instance
for the current list of frames, and reports all the positions to the user
including whatever we knew about the expression trees at those points in time

This sort of abstraction also supports a better division of labor: The core evaluator, where most of the work is done, works on the simplest type: trees without any annotations, just Fix ExprF. All of the additional enrichments for things like error reporting happen outside of this logic, reducing the overall complexity by localizing each bit of functionality to its own module.

Adding tracing to any evaluator
The second application of this technique used adiM to introduce both Reader and IO effects, to produce a tracing report during evaluation showing which parts of the tree we’re working on as we go. Again, without modifying the original evaluator, or even knowing anything about which evaluator we end up extending:

tracingEvalExpr
    :: (MonadIO m,
        MonadIO n, Alternative n)
    => (NExprF (m v) -> m v)
    -> NExpr -> n (m v)
tracingEvalExpr eval =
  flip runReaderT (0 :: Int)
    . adiM (pure <$> eval) psi
 where
  psi k v = do
    depth <- ask
    guard (depth < 200)
    local succ $ do
      action <- k v
      return $ do
        liftIO $ putStrLn $ "eval: "
          ++ replicate (depth * 2) ' ' ++ show v
        res <- action
        liftIO $ putStrLn $ "eval: "
          ++ replicate (depth * 2) ' ' ++ "."
        return res
Reducing programs to test cases
Finally we come to the motivation for this article. Although the above expressiveness and flexibility was enough to convince me of the potential in the recursion schemes approach, I still hadn’t found its “killer app”: something that recursion schemes is able to make so much easier that it’s well worth whatever boilerplate the technique induces. But yesterday I think I found that example.

A problem with evaluating lazy functional languages, like nix or Haskell, is that it allows for self-referential structures by way of tying the knot. This is great for writing compact and elegant code, but extremely tricky if you happen to get the evaluation semantics wrong, which was the case in hnix yesterday. It ended up that somewhere deep within the evaluation of nixpkgs, I ended up forcing a thunk that I was already in the process of forcing. This means that somewhere in the evaluation hnix was either being too eager, or I’d gotten the scoping wrong and a self-reference was occurring where it shouldn’t have.

Either way, it ended up proving very difficult to delve deep into the thousands of lines of highly recursive, lazy, higher-order code. How was I going to find the root cause of the problem?

It occurred to me that even though the expression tree involved were massive, I’d only evaluated a fraction of it before encountering the bug. So why can’t I just output that fragment along with the failure, to make it easier to see what was actually involved in producing the problem? The algorithm seemed simple enough:

Start with an unadorned expression tree.

Annotate it with mutable booleans at every layer, to indicate whether we’ve forced the evaluation of that layer or not (i.e., whether it contributed to the final outcome).

Strip away from the tree anything that was never referenced.

Further compact the tree based on the logical consequences of step 3. For example, if we force the true branch of an if, but never the false, we can replace the if statement expression with just the true branch.

The tree that results from this winnowing should yield exactly the same behavior, but be potentially much smaller and simpler. After all, there are over 1.2 million lines of code already in nixpkgs, and it’s hard to know due to the lazy nature of Nix how much of it we actually touched during evaluation. There has to be a better way!

At first I thought this would be a typical hard problem: That is, easy to imagine a solution for, but many long hours of elbow grease to make it happen. I logged a bug in the tracker describing the idea, wondering how many days it would take to realize, and how much the code would have to change to make it possible.

Two hours later, it was working: thanks to both recursion schemes and abstract definitional interpreters.

This was accomplished by first defining the type of boolean-flagged trees, which extend whatever other kind of tree (given by the functor f) we might be working with:

newtype FlaggedF f r
  = FlaggedF { flagged :: (IORef Bool, f r) }
  deriving (Functor, Foldable, Traversable)

type Flagged f = Fix (FlaggedF f)
Then a function in IO that takes a given expression tree, and enriches it with all the booleans set to False, to mean unvisited:

flagExprLoc :: (MonadIO n, Traversable f)
            => Fix f -> n (Flagged f)
flagExprLoc = cataM $ \x -> do
  flag <- liftIO $ newIORef False
  pure $ Fix $ FlaggedF (flag, x)
Of course we also need a way to strip away the annotations later. cata makes this one really easy:

stripFlags :: Functor f => Flagged f -> Fix f
stripFlags = cata $ Fix . snd . flagged
Now using adiM we can fold in the IO monad, allowing us to toggle these IORef boolean as we evaluate. For these function we need to know the types of the trees involved, so that we can reduce this appropriately in pruneTree.

flaggedEvalExpr
    :: (Framed e m, Exception r,
       MonadCatch m, MonadIO m,
       MonadCatch n, MonadIO n)
    => (NExprF (m v) -> m v)
    -> NExprLoc
    -> n (m (NExprLoc, Either r v))
flaggedEvalExpr eval expr = do
  expr' <- flagExprLoc expr
  res <- adiM (pure <$> eval
                . snd . getCompose
                . snd . flagged) psi expr'
  return $ do
    eres   <- catch (Right <$> res) (pure . Left)
    expr'' <- pruneTree expr'
    return (fromMaybe nNull expr'', eres)
 where
  psi k v@(Fix (FlaggedF (b, _))) =
      (liftIO (writeIORef b True) *>) <$> k v
That I can insert this whole function here in a blog post is, to me, a testament to the power of the abstractions involved. This is all we needed to color the tree with the locations where we actually performed evaluation!

The last step is to cull the tree of its dead wood, by applying logical transforms wherever lack of evaluation implies a reduction in the size of the tree. This function is the only genuinely complex part and is a bit too long to include here. But it’s also a pure function, making it easier to verify and test. Here’s an excerpt of what it looks like:

pruneTree :: MonadIO n
          => Flagged NExprLocF
          -> n (Maybe NExprLoc)
pruneTree = cataM $ \(FlaggedF (b, Compose x)) -> do
  used <- liftIO $ readIORef b
  pure $ if used
         then Fix . Compose <$> traverse prune x
         else Nothing
 where
  prune = \case
    NLet binds (Just body@(Fix (Compose (Ann _ x)))) ->
      Just $ case mapMaybe pruneBinding binds of
        [] -> x
        xs -> NLet xs body

    -- These are the only short-circuiting binary operators
    NBinary NAnd (Just (Fix (Compose (Ann _ larg)))) _
      -> Just larg
    NBinary NOr  (Just (Fix (Compose (Ann _ larg)))) _
      -> Just larg

    -- If the function was never called, it means its
    -- argument was in a thunk that was forced elsewhere.
    NBinary NApp Nothing (Just _) -> Nothing

    -- If the scope of a with was never referenced,
    -- it's not needed
    NWith Nothing (Just (Fix (Compose (Ann _ body))))
      -> Just body

    -- ... more logic here ...

    NIf _ Nothing (Just (Fix (Compose (Ann _ f)))) -> Just f
    NIf _ (Just (Fix (Compose (Ann _ t)))) Nothing -> Just t

    -- Let the semantics of the Maybe monad handle the rest
    x -> sequence x
And that’s pretty much it. Now I can input a program like this:

let x = { z = 80 + 20; w = 123; };
    y = "Hello";
    z = "Goodbye";
in assert 1 == 1; if x.z == 100 then y else 3
And automatically reduce it to a smaller program producing the same result:

let
  x = {
    z = 80 + 20;
    w = null;
  };
  y = "Hello";
in y

It even works for my huge thunk recursion problem in nixpkgs, reducing that large amount of code spread across many files, to a single file containing less than 10k lines of code with very few imports remaining (those that could not be statically determined).

Conclusion

recursion schemes
- CONS: boilerplate to define and work with types
 - but hide via bidirectional pattern synonyms (see below)
- PROS: see above

{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad.Free

data TeletypeF r
  = GetF (String -> r)
  | PutF String r
  deriving Functor

type Teletype = Free TeletypeF

pattern Get :: (String -> Teletype a) -> Teletype a
pattern Get x = Free (GetF x)

pattern Put :: String  -> Teletype a  -> Teletype a
pattern Put s r = Free (PutF s r)

-- Note : the 'Free' constructor is not seen in any of the code below.
main :: IO ()
main = iterM phi prog
 where
  prog =
    Get $ \x ->
    Get $ \y ->
      Put x (Put y (pure ()))
  phi  = \case
    GetF k -> k =<< readLn
    PutF s k -> putStrLn s >> k

recommendation: use recursion schemes (instead of traditional recursive types)
