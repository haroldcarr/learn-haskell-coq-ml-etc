{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}

module SimpleReflect where

import Debug.SimpleReflect

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- e0 = [f,g] <*> [] :: [Expr]
e1,e2,e3,e4 :: [Expr]
e1 = [f,g] <*> [a]
e2 = [f,g] <*> [a,b]
e3 = [f,g] <*> [a,b,c]
e4 = [f,g] <*> [a,b,c,d]

-- x0 = [f,g,h] <*> []  :: _ Expr
x1,x2,x3,x4 :: [Expr]
x1 = [f,g,h] <*> [a]
x2 = [f,g,h] <*> [a,b]
x3 = [f,g,h] <*> [a,b,c]
x4 = [f,g,h] <*> [a,b,c,d]

fl,fl1,fr,fr1 :: Expr
fl  = foldl  @[] (\as a' -> f a' <> as) mempty [a, b, c]
fl1 = foldl1 @[] (\as a' -> f a' <> as)        [a, b, c]
fr  = foldr  @[] (\a' as -> f a' <> as) mempty [a, b, c]
fr1 = foldr1 @[] (\a' as -> f a' <> as)        [a, b, c]

{-
import Debug.SimpleReflect
:set -XTypeApplications
:t foldMap
:t foldMap @[]
:t foldMap @[] @Expr
:t foldMap @[] @Expr f
:t foldMap @[] @Expr f [a, b, c]
foldMap @[] @Expr f [a, b, c]
-}
