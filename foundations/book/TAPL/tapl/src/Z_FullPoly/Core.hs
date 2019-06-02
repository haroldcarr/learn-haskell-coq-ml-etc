{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Z_FullPoly.Core where

import           Protolude
import           Z_FullPoly.Syntax

------------------------   EVALUATION  ------------------------

data NoRuleApplies = NoRuleApplies deriving (Eq, Show)

isVal :: Context -> Term -> Bool
isVal ctx = \case
  TmTrue                       -> True
  TmFalse                      -> True
  TmAbs {}                     -> True
  TmPack _ v1 _ | isVal ctx v1 -> True
  TmTAbs {}                    -> True
  _                            -> False

eval1 :: Context -> Term -> Either NoRuleApplies Term
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
  TmVar _ n _ ->
    case getBinding ctx n of
      TmAbbBind t _ -> pure t
      _ -> Left NoRuleApplies
  TmTApp (TmTAbs _x t11) tyT2 ->
    pure $ tyTermSubstTop tyT2 t11
  TmTApp t1 tyT2 -> do
    t1' <- eval1 ctx t1
    pure $ TmTApp t1' tyT2
  _ ->
    Left NoRuleApplies

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
  Left NoRuleApplies -> t
  Right t'           -> eval ctx t'

{-
let istyabb ctx i =
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let gettyabb ctx i =
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT'
  with NoRuleApplies -> tyT

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyString,TyString) -> true
  | (TyUnit,TyUnit) -> true
  | (TyId(b1),TyId(b2)) -> b1=b2
  | (TyFloat,TyFloat) -> true
  | (TyVar(i,_), _) when istyabb ctx i ->
      tyeqv ctx (gettyabb ctx i) tyT
  | (_, TyVar(i,_)) when istyabb ctx i ->
      tyeqv ctx tyS (gettyabb ctx i)
  | (TyVar(i,_),TyVar(j,_)) -> i=j
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyBool,TyBool) -> true
  | (TyNat,TyNat) -> true
  | (TySome(tyX1,tyS2),TySome(_,tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       tyeqv ctx1 tyS2 tyT2
  | (TyRecord(fields1),TyRecord(fields2)) ->
       List.length fields1 = List.length fields2
       &&
       List.for_all
         (fun (li2,tyTi2) ->
            try let (tyTi1) = List.assoc li2 fields1 in
                tyeqv ctx tyTi1 tyTi2
            with Not_found -> false)
         fields2
  | (TyAll(tyX1,tyS2),TyAll(_,tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       tyeqv ctx1 tyS2 tyT2
  | _ -> false

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
    TmInert(fi,tyT) ->
      tyT
  | TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match simplifyty ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if tyeqv ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmLet(fi,x,t1,t2) ->
     let tyT1 = typeof ctx t1 in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in
     typeShift (-1) (typeof ctx' t2)
  | TmFix(fi, t1) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
           TyArr(tyT11,tyT12) ->
             if tyeqv ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmString _ -> TyString
  | TmUnit(fi) -> TyUnit
  | TmAscribe(fi,t1,tyT) ->
     if tyeqv ctx (typeof ctx t1) tyT then
       tyT
     else
       error fi "body of as-term does not have the expected type"
  | TmRecord(fi, fields) ->
      let fieldtys =
        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")
  | TmTrue(fi) ->
      TyBool
  | TmFalse(fi) ->
      TyBool
  | TmIf(fi,t1,t2,t3) ->
     if tyeqv ctx (typeof ctx t1) TyBool then
       let tyT2 = typeof ctx t2 in
       if tyeqv ctx tyT2 (typeof ctx t3) then tyT2
       else error fi "arms of conditional have different types"
     else error fi "guard of conditional not a boolean"
  | TmFloat _ -> TyFloat
  | TmTimesfloat(fi,t1,t2) ->
      if tyeqv ctx (typeof ctx t1) TyFloat
      && tyeqv ctx (typeof ctx t2) TyFloat then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmPack(fi,tyT1,t2,tyT) ->
      (match simplifyty ctx tyT with
          TySome(tyY,tyT2) ->
            let tyU = typeof ctx t2 in
            let tyU' = typeSubstTop tyT1 tyT2 in
            if tyeqv ctx tyU tyU' then tyT
            else error fi "doesn't match declared type"
        | _ -> error fi "existential type expected")
  | TmUnpack(fi,tyX,x,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
          TySome(tyY,tyT11) ->
            let ctx' = addbinding ctx tyX TyVarBind in
            let ctx'' = addbinding ctx' x (VarBind tyT11) in
            let tyT2 = typeof ctx'' t2 in
            typeShift (-2) tyT2
        | _ -> error fi "existential type expected")
  | TmTAbs(fi,tyX,t2) ->
      let ctx = addbinding ctx tyX TyVarBind in
      let tyT2 = typeof ctx t2 in
      TyAll(tyX,tyT2)
  | TmTApp(fi,t1,tyT2) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
           TyAll(_,tyT12) -> typeSubstTop tyT2 tyT12
         | _ -> error fi "universal type expected")

let evalbinding ctx b = match b with
    TmAbbBind(t,tyT) ->
      let t' = eval ctx t in
      TmAbbBind(t',tyT)
  | bind -> bind
-}
