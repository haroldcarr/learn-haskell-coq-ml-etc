{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module X_3 where

import           Data.List
import           Data.Map.Strict as Map
import           Debug.Trace
import           Test.HUnit            (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util       as U (tt)
------------------------------------------------------------------------------
-- Works with either import (one at a time, of course)
-- import X_1
import X_2

-- | preserves names
--   rewrites let bindings
desugarTop :: ExpLet a -> ExpAnn a
desugarTop = desugar Map.empty
desugar :: Map.Map String Int -> ExpLet a -> ExpAnn a
desugar env expr = case expr of
  LitLet a          -> LitAnn a
  VarLet name       -> VarAnn name (env Map.! name)
  AbsLet name expr' -> let env'  = Map.map succ env
                           env'' = Map.insert name 0 env'
                       in AbsAnn name (desugar env'' expr')
  AppLet f x        -> AppAnn (desugar env f) (desugar env x)
  LetLet n v expr'  -> desugar env (AppLet (AbsLet n expr') v)
  _                 -> error "impossible?"

desugarTop' :: ExpLet a -> ExpAnn a
desugarTop' = desugar' []
desugar' :: [String] -> ExpLet a -> ExpAnn a
desugar' env expr = case expr of
  LitLet a          -> LitAnn a
  VarLet name       -> VarAnn name (fromJust (name `elemIndex` reverse env))
  AbsLet name expr' -> AbsAnn name (desugar' (name:env) expr')
  AppLet f x        -> AppAnn (desugar' env f) (desugar' env x)
  LetLet n v expr'  -> desugar' env (AppLet (AbsLet n expr') v)
  _                 -> error "impossible?"

fromJust :: Num p => Maybe p -> p
fromJust Nothing = -1
fromJust (Just i) = i

-- | removes names
anonymize :: ExpAnn a -> ExpUD a
anonymize expr = case expr of
  LitAnn a   -> LitUD a
  VarAnn _ i -> VarUD i
  AbsAnn _ e -> AbsUD (anonymize e)
  AppAnn f x -> AppUD (anonymize f) (anonymize x)
  _          -> error "impossible?"

-- | operates on undecorated expressions
eval :: Show a => [a] -> ExpUD a -> a
eval env expr = case expr of
  LitUD a   -> a
  VarUD i   -> trace (show env ++ " " ++ show i) (env !! i)
  AbsUD f   -> eval env f
  AppUD f x -> let x' = eval env x
               in eval (x':env) f
  _         -> error "impossible?"

------------------------------------------------------------------------------
-- summary
-- - composable compiler passes : smaller, easier to write and to think about

------------------------------------------------------------------------------

tidentity :: [Test]
tidentity = U.tt "tidentity"  --           |identity               |
  [ eval [] (anonymize (desugarTop (AppLet (AbsLet "i" (VarLet "i"))   (LitLet 1))))
  , eval [] (anonymize             (AppAnn (AbsAnn "i" (VarAnn "i" 0)) (LitAnn 1)))
  , eval []                        (AppUD  (AbsUD      (VarUD      0)) (LitUD  1))

  -- wrapped with an ignored Let value
  , eval [] (anonymize (desugarTop
                          (LetLet "x"
                                   (LitLet 1)
                                   (AppLet (AbsLet "i" (VarLet "i")) (VarLet "x")))))
  -- wrapped with an ignored "desugared" Let value
  , eval [] (anonymize (desugarTop
                            (ExpX ( "x"
                                  , LitLet 1
                                  , AppLet (AbsLet "i" (VarLet "i")) (VarLet "x")
                                  ))))
  ]
  1

{-# ANN konstH "HLint: ignore Use const"        #-}
{-# ANN konstH "HLint: ignore Redundant lambda" #-}
{-# ANN konstH' "HLint: ignore Use id" #-}
{-# ANN konstH' "HLint: ignore Redundant lambda" #-}
konstH :: a -> b -> a
konstH x = \_ -> x
konstH' :: a -> b -> b
konstH' _ = \y -> y

konst :: ExpLet a
konst  = AbsLet "x" (AbsLet "y" (VarLet "x"))
konst3Xyz,konst3xYz,konst3xyZ,konst3xyX :: ExpLet a
konst3Xyz = AbsLet "x" (AbsLet "y" (AbsLet "z" (VarLet "x")))
konst3xYz = AbsLet "x" (AbsLet "y" (AbsLet "z" (VarLet "y")))
konst3xyZ = AbsLet "x" (AbsLet "y" (AbsLet "z" (VarLet "z")))
konst3xyX = AbsLet "x" (AbsLet "y" (AbsLet "x" (VarLet "x")))

konst' :: ExpLet a
konst'  = AbsLet "y" (AbsLet "x" (VarLet "x"))

tkonst :: [Test]
tkonst = U.tt "tkonst"
  [ eval [] (anonymize (desugarTop
                        (AppLet (AppLet konst
                                 (LitLet 1))
                          (LitLet 2))))
  , eval [] (anonymize (desugarTop
                        (AppLet (AppLet (AbsLet "x" (AbsLet "y" (VarLet "x")))
                                 (LitLet 1))
                          (LitLet 2))))
  , eval [] (anonymize  (AppAnn (AppAnn (AbsAnn "x" (AbsAnn "y" (VarAnn "x" 1)))
                                 (LitAnn 1))
                          (LitAnn 2)))
  , eval []             (AppUD  (AppUD  (AbsUD      (AbsUD      (VarUD      1)))
                                 (LitUD  1))
                          (LitUD  2))
  ]
  2

tkonst3 :: [Test]
tkonst3 = U.tt "tkonst3"
  [ eval [] (anonymize
             (desugarTop'
               (AppLet
                 (AppLet
                   (AppLet konst3Xyz
                     (LitLet 1))
                   (LitLet 2))
                 (LitLet 3))))
  , eval [] (anonymize
             (desugarTop'
               (AppLet
                 (AppLet
                   (AppLet konst3xYz
                     (LitLet 2))
                   (LitLet 1))
                 (LitLet 3))))
  , eval [] (anonymize
             (desugarTop'
               (AppLet
                 (AppLet
                   (AppLet konst3xyZ
                     (LitLet 3))
                   (LitLet 2))
                 (LitLet 1))))
    -- not sure about this one:
  , eval [] (anonymize
             (desugarTop'
               (AppLet
                 (AppLet
                   (AppLet konst3xyX
                     (LitLet 1))
                   (LitLet 2))
                 (LitLet 3))))
  ]
  1

test :: IO Counts
test  =
  runTestTT $ TestList $
  tidentity ++ tkonst ++ tkonst3
