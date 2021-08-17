{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Z_FullPoly.Core where

import           Protolude
import           Z_FullPoly.Syntax

------------------------   EVALUATION  ------------------------

isVal :: Context -> Term -> Bool
isVal ctx = \case
  TmTrue                       -> True
  TmFalse                      -> True
  TmAbs {}                     -> True
  TmPack _ v1 _ | isVal ctx v1 -> True
  TmTAbs {}                    -> True
  _                            -> False

eval1 :: Context -> Term -> Either Err Term
eval1 ctx = \case
  TmApp (TmAbs _x _tyT11 t12) v2 | isVal ctx v2 ->
    pure $ termSubstTop v2 t12
  TmApp v1 t2 | isVal ctx v1 -> do
    t2' <- eval1 ctx t2
    pure $ TmApp v1 t2'
  TmApp t1 t2 -> do
     t1' <- eval1 ctx t1
     pure $ TmApp t1' t2
  TmIf TmTrue  t2 _t3 ->
    pure t2
  TmIf TmFalse _t2 t3 ->
    pure t3
  TmIf t1 t2 t3 -> do
    t1' <- eval1 ctx t1
    pure $ TmIf t1' t2 t3
  TmUnpack _ _ (TmPack tyT11 v12 _) t2 | isVal ctx v12 ->
    pure $ tyTermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2)
  TmUnpack tyX x t1 t2 -> do
    t1' <- eval1 ctx t1
    pure $ TmUnpack tyX x t1' t2
  TmPack tyT1 t2 tyT3 -> do
    t2' <- eval1 ctx t2
    pure $ TmPack tyT1 t2' tyT3
  TmVar _ n _ -> getBinding ctx n >>= \case
      TmAbbBind t _ -> pure t
      _             -> Left NoRuleApplies
  TmTApp (TmTAbs _x t11) tyT2 ->
    pure $ tyTermSubstTop tyT2 t11
  TmTApp t1 tyT2 -> do
    t1' <- eval1 ctx t1
    pure $ TmTApp t1' tyT2
  _ ->
    Left NoRuleApplies

eval :: Context -> Term -> Either Err Term
eval ctx t = case eval1 ctx t of
  Right t'                    -> eval ctx t'
  Left NoRuleApplies          -> pure t
  l@Left {}                   -> l

------------------------------------------------------------------------------

isTyAbbUnsafe :: Context -> Int -> Bool
isTyAbbUnsafe ctx i = case getBinding ctx i of
  Left e              -> panic $ show e
  Right (TyAbbBind _) -> True
  Right _             -> False

isTyAbb :: Context -> Int -> Either Err Bool
isTyAbb ctx i = getBinding ctx i >>= \case
  TyAbbBind _ -> pure True
  _           -> pure False

getTyAbb :: Context -> Int -> Either Err Ty
getTyAbb ctx i = getBinding ctx i >>= \case
  TyAbbBind tyT -> pure tyT
  _             -> Left NoRuleApplies

computeTy :: Context -> Ty -> Either Err Ty
computeTy ctx tyT = case tyT of
  TyVar _ i _ -> case isTyAbb ctx i of
    Right b   -> if b then getTyAbb ctx i else Left NoRuleApplies
    Left  l   -> Left l
  _           -> Left NoRuleApplies

simplifyTy :: Context -> Ty -> Either Err Ty
simplifyTy ctx tyT = case computeTy ctx tyT of
  Right tyT'         -> simplifyTy ctx tyT'
  Left NoRuleApplies -> pure tyT
  l                  -> l

tyEqv :: Context -> Ty -> Ty -> Either Err Bool
tyEqv ctx tyS0 tyT0 = do
  tyS <- simplifyTy ctx tyS0
  tyT <- simplifyTy ctx tyT0
  case (tyS,tyT) of
    (TyVar _ i _, _) | isTyAbbUnsafe ctx i -> do
      tyAbb <- getTyAbb ctx i
      tyEqv ctx tyAbb tyT

    (_, TyVar _ i _) | isTyAbbUnsafe ctx i -> do
      tyAbb <- getTyAbb ctx i
      tyEqv ctx tyS tyAbb
    (TyVar _ i _, TyVar _ j _) -> pure (i==j)
    (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> do
      bl <- tyEqv ctx tyS1 tyT1
      br <- tyEqv ctx tyS2 tyT2
      pure (bl == br)
    (TyBool,TyBool) ->
      pure True
    (TySome tyX1 tyS2, TySome _ tyT2) ->
      tyEqv (addName ctx tyX1) tyS2 tyT2

    (TyAll tyX1 tyS2, TyAll _ tyT2) ->
      tyEqv (addName ctx tyX1) tyS2 tyT2
    _ -> pure False

------------------------   TYPING  ------------------------

typeOf :: Context -> Term -> Either Err Ty
typeOf ctx = \case
  TmVar _ i _ -> getTypeFromContext ctx i

  TmAbs x tyT1 t2 -> do
    let ctx' = addBinding ctx x (VarBind tyT1)
    tyT2 <- typeOf ctx' t2
    pure $ TyArr tyT1 (typeShift (-1) tyT2)
  TmApp t1 t2 -> do
    tyT1 <- typeOf ctx t1
    tyT2 <- typeOf ctx t2
    simplifyTy ctx tyT1 >>= \case
      TyArr tyT11 tyT12 -> do
        teq <- tyEqv ctx tyT2 tyT11
        if teq then pure tyT12 else Left ParameterTypeMismatch
      _ -> Left ArrowTypeExpected
  TmTrue ->
    pure TyBool
  TmFalse ->
    pure TyBool

  TmIf t1 t2 t3 -> do
    tyT1 <- typeOf ctx t1
    tyT1IsBool <- tyEqv ctx tyT1 TyBool
    if tyT1IsBool
      then do
        tyT2 <- typeOf ctx t2
        tyT3 <- typeOf ctx t3
        tyT2EqTyT3 <- tyEqv ctx tyT2 tyT3
        if tyT2EqTyT3 then pure tyT2 else Left ArmsOfConditionalHaveDifferentTypes
      else
        Left GuardOfConditionalNotABoolean
  TmPack tyT1 t2 tyT -> simplifyTy ctx tyT >>= \case
    TySome _tyY tyT2 -> do
      tyU <- typeOf ctx t2
      let tyU' = typeSubstTop tyT1 tyT2
      teq <- tyEqv ctx tyU tyU'
      if teq then pure tyT else Left DoesNotMatchDeclaredType
    _ -> Left ExistentialTypeExpected
  TmUnpack tyX x t1 t2 -> do
    tyT1 <- typeOf ctx t1
    simplifyTy ctx tyT1 >>= \case
      TySome _tyY tyT11 -> do
        let ctx'  = addBinding ctx tyX TyVarBind
        let ctx'' = addBinding ctx' x (VarBind tyT11)
        tyT2 <- typeOf ctx'' t2
        pure $ typeShift (-2) tyT2
      _ -> Left ExistentialTypeExpected
  TmTAbs tyX t2 -> do
    let ctx' = addBinding ctx tyX TyVarBind
    tyT2 <- typeOf ctx' t2
    pure $ TyAll tyX tyT2
  TmTApp t1 tyT2 -> do
    tyT1 <- typeOf ctx t1
    simplifyTy ctx tyT1 >>= \case
      TyAll _ tyT12 -> pure $ typeSubstTop tyT2 tyT12
      _             -> Left UniversalTypeExpected

evalBinding :: Context -> Binding -> Either Err Binding
evalBinding ctx = \case
  TmAbbBind t tyT -> eval ctx t >>= \t' -> pure $ TmAbbBind t' tyT
  bind            -> pure bind
