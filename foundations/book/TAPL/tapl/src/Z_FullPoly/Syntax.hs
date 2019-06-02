{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Z_FullPoly.Syntax where

import qualified Prelude
import           Protolude

type Var = Text

data Err
  = NoRuleApplies
  | VarLookupFailure   Int Int
  | UnboundVar         Var
  | NoTypeRecorded     Var
  | WrongKindOfBinding Var
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Datatypes

data Ty
  = TyVar   Var Int Int
  | TyArr   Ty  Ty
  | TyBool
  | TySome  Var Ty
  | TyAll   Var Ty
  deriving (Eq, Show)

data Term
  = TmVar    Var  Int  Int
  | TmAbs    Var  Ty   Term
  | TmApp    Term Term
  | TmTrue
  | TmFalse
  | TmIf     Term Term Term
  | TmPack   Ty   Term Ty
  | TmUnpack Var  Var  Term Term
  | TmTAbs   Var  Term
  | TmTApp   Term Ty
  deriving (Eq, Show)

data Binding
  = NameBind
  | TyVarBind
  | VarBind   Ty
  | TyAbbBind Ty
  | TmAbbBind Term (Maybe Ty)
  deriving (Eq, Show)

type Context = [(Var, Binding)]

data Command
  = Eval     Term
  | Bind     Var  Binding
  | SomeBind Var  Var     Term
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Context management

emptyContext :: Context
emptyContext = []

ctxLength :: Context -> Int
ctxLength = length

addBinding :: Context -> Var -> Binding -> Context
addBinding ctx x bind = (x,bind):ctx

addName :: [(Var, Binding)] -> Var -> Context
addName ctx x = addBinding ctx x NameBind

isNameBound :: Context -> Var -> Bool
isNameBound ctx x =
  case ctx of
    []           -> False
    ((y,_):rest) -> y==x || isNameBound rest x
{-
pickFreshName ctx x =
  if isNamebound ctx x then pickFreshName ctx (x<>"'")
  else ((x,NameBind):ctx), x
-}
indexToName :: Context -> Int -> Either Err Var
indexToName ctx x =
  if x < ctxLength ctx then pure $ fst $ ctx Prelude.!! x
  else Left $ VarLookupFailure x (ctxLength ctx)

nameToIndex :: Context -> Text -> Either Err Int
nameToIndex ctx x =
  case ctx of
    []           -> Left $ UnboundVar x
    ((y,_):rest) -> if y==x then pure 0 else (+1) <$> nameToIndex rest x

----------------------------------------------------------------------
-- Shifting

tyMap :: (Int -> Var -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tyMap onVar = walk
 where
  walk c = \case
    TyVar   v   x    n -> onVar c v x n
    TyBool             -> TyBool
    TyArr  tyT1 tyT2   -> TyArr  (walk c tyT1) (walk c tyT2)
    TySome tyX  tyT2   -> TySome tyX           (walk (c+1) tyT2)
    TyAll  tyX  tyT2   -> TyAll  tyX           (walk (c+1) tyT2)

tmMap :: (Int -> Var -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmMap onVar onType = walk
 where
  walk c = \case
    TmVar     v   x    n       -> onVar c v x n
    TmAbs    x    tyT1 t2      -> TmAbs x  (onType c tyT1) (walk (c+1) t2)
    TmApp    t1   t2           -> TmApp    (walk c t1)     (walk c t2)
    TmTrue                     -> TmTrue
    TmFalse                    -> TmFalse
    TmIf     t1   t2   t3      -> TmIf     (walk c t1)     (walk c t2) (walk c t3)
    TmPack   tyT1 t2   tyT3    -> TmPack   (onType c tyT1) (walk c t2) (onType c tyT3)
    TmUnpack tyX  x    t1   t2 -> TmUnpack tyX             x           (walk c t1) (walk (c+2) t2)
    TmTAbs   tyX  t2           -> TmTAbs   tyX             (walk (c+1) t2)
    TmTApp   t1   tyT2         -> TmTApp   (walk c t1)     (onType c tyT2)

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
  tyMap
    (\c v x n -> if x>=c then TyVar v (x+d) (n+d) else TyVar v x (n+d))

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
  tmMap
    (\c v x n -> if x>=c then TmVar v (x+d) (n+d)
                 else         TmVar v x    (n+d))
    (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

bindingShift :: Int -> Binding -> Binding
bindingShift d = \case
  NameBind              -> NameBind
  TyVarBind             -> TyVarBind
  TyAbbBind tyT         -> TyAbbBind (typeShift d tyT)
  VarBind   tyT         -> VarBind   (typeShift d tyT)
  TmAbbBind t   tyT_opt -> TmAbbBind (termShift d t)   (typeShift d <$> tyT_opt)
    {-
    let tyT_opt' = case tyT_opt of
                     Nothing  -> Nothing
                     Just tyT -> Just (typeShift d tyT)
     in TmAbbBind (termShift d t) tyT_opt'
    -}

----------------------------------------------------------------------
-- Substitution

termSubst :: Int -> Term -> Term -> Term
termSubst j0 s =
  tmMap
    (\j v x n -> if x==j then termShift j s else TmVar v x n)
    (\_j tyT -> tyT)
    j0

termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS =
  tyMap
    (\j v x n -> if x==j then typeShift j tyS else TyVar v x n)

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT =
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

tyTermSubst :: Ty -> Int -> Term -> Term
tyTermSubst tyS =
  tmMap (\_c v x n -> TmVar v x n)
        (typeSubst tyS)

tyTermSubstTop :: Ty -> Term -> Term
tyTermSubstTop tyS t =
  termShift (-1) (tyTermSubst (typeShift 1 tyS) 0 t)

----------------------------------------------------------------------
-- Context management (continued)

getBinding :: Context -> Int -> Either Err Binding
getBinding ctx i =
  if i < ctxLength ctx
    then let (_,bind) = ctx Prelude.!! i
          in pure $ bindingShift (i+1) bind
  else
    Left $ VarLookupFailure i (ctxLength ctx)

getTypeFromContext :: Context -> Int -> Either Err Ty
getTypeFromContext ctx i = getBinding ctx i >>= \case
    VarBind tyT            -> pure tyT
    TmAbbBind _ (Just tyT) -> pure tyT
    TmAbbBind _ Nothing    -> do
      n <- indexToName ctx i
      Left $ NoTypeRecorded n
    _                      -> do
      n <- indexToName ctx i
      Left $ WrongKindOfBinding n
