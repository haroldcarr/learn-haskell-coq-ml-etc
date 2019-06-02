{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Z_FullPoly.Syntax where

import qualified Prelude
import           Protolude

type Var = Text

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
indexToName :: Context -> Int -> Var
indexToName ctx x =
  if x < ctxLength ctx then fst $ ctx Prelude.!! x
  else panic $
    "Variable lookup failure: offset: " <> show x <> " , ctx size: " <> show (ctxLength ctx)

nameToIndex :: Context -> Text -> Int
nameToIndex ctx x =
  case ctx of
    []           -> panic ("Identifier " <> x <> " is unbound")
    ((y,_):rest) -> if y==x then 0 else 1 + nameToIndex rest x

----------------------------------------------------------------------
-- Shifting

tyMap :: Num a => (a -> Int -> Int -> Ty) -> a -> Ty -> Ty
tyMap onVar = walk
 where
  walk c = \case
    TyVar  _v   x    n -> onVar c {-v-} x n
    TyBool             -> TyBool
    TyArr  tyT1 tyT2   -> TyArr  (walk c tyT1) (walk c tyT2)
    TySome tyX  tyT2   -> TySome tyX           (walk (c+1) tyT2)
    TyAll  tyX  tyT2   -> TyAll  tyX           (walk (c+1) tyT2)

tmMap :: Num t => (t -> Int -> Int -> Term) -> (t -> Ty -> Ty) -> t -> Term -> Term
tmMap onVar onType = walk
 where
  walk c = \case
    TmVar    _v   x    n       -> onVar c {-v-} x n
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
    (\c x n -> if x>=c then TyVar "TODO" (x+d) (n+d) else TyVar "TODO" x (n+d))

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
  tmMap
    (\c x n -> if x>=c then TmVar "TODO" (x+d) (n+d)
               else         TmVar "TODO"  x    (n+d))
    (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

bindingshift :: Int -> Binding -> Binding
bindingshift d = \case
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
    (\j x n -> if x==j then termShift j s else TmVar "TODO" x n)
    (\_j tyT -> tyT)
    j0

termSubstTop :: Term -> Term -> Term
termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS =
  tyMap
    (\j x n -> if x==j then typeShift j tyS else TyVar "TODO" x n)

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT =
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

tyTermSubst :: Ty -> Int -> Term -> Term
tyTermSubst tyS =
  tmMap (\_c x n -> TmVar "TODO" x n)
        (typeSubst tyS)

tyTermSubstTop :: Ty -> Term -> Term
tyTermSubstTop tyS t =
  termShift (-1) (tyTermSubst (typeShift 1 tyS) 0 t)
{-
(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bindingshift (i+1) bind
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
 let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
         VarBind(tyT) -> tyT
     | TmAbbBind(_,Some(tyT)) -> tyT
     | TmAbbBind(_,None) -> error fi ("No type recorded for variable "
                                        ^ (index2name fi ctx i))
     | _ -> error fi
       ("getTypeFromContext: Wrong kind of binding for variable "
         ^ (index2name fi ctx i))
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmInert(fi,_) -> fi
  | TmVar(fi,_,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmLet(fi,_,_,_) -> fi
  | TmFix(fi,_) -> fi
  | TmString(fi,_) -> fi
  | TmUnit(fi) -> fi
  | TmAscribe(fi,_,_) -> fi
  | TmProj(fi,_,_) -> fi
  | TmRecord(fi,_) -> fi
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmFloat(fi,_) -> fi
  | TmTimesfloat(fi,_,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi
  | TmPack(fi,_,_,_) -> fi
  | TmUnpack(fi,_,_,_,_) -> fi
  | TmTAbs(fi,_,_) -> fi
  | TmTApp(fi,_, _) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details.
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t =
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printty_Type outer ctx tyT = match tyT with
    TyAll(tyX,tyT2) ->
      let (ctx1,tyX) = (pickfreshname ctx tyX) in
      obox(); pr "All "; pr tyX; pr ".";
      print_space ();
      printty_Type outer ctx1 tyT2;
      cbox()
  | tyT -> printty_ArrowType outer ctx tyT

and printty_ArrowType outer ctx  tyT = match tyT with
    TyArr(tyT1,tyT2) ->
      obox0();
      printty_AType false ctx tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TyId(b) -> pr b
  | TyString -> pr "String"
  | TyUnit -> pr "Unit"
  | TyRecord(fields) ->
        let pf i (li,tyTi) =
          if (li <> ((string_of_int i))) then (pr li; pr ":");
          printty_Type false ctx tyTi
        in let rec p i l = match l with
            [] -> ()
          | [f] -> pf i f
          | f::rest ->
              pf i f; pr","; if outer then print_space() else break();
              p (i+1) rest
        in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TyBool -> pr "Bool"
  | TyFloat -> pr "Float"
  | TyNat -> pr "Nat"
  | TySome(tyX,tyT2) ->
      let (ctx1,tyX) = pickfreshname ctx tyX in
      obox();
      pr "{Some "; pr tyX; pr ",";
      if outer then print_space() else break();
      printty_Type false ctx1 tyT2; pr "}";
      cbox()
  | tyT -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT

let rec printtm_Term outer ctx t = match t with
    TmAbs(fi,x,tyT1,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
         obox(); pr "lambda ";
         pr x'; pr ":"; printty_Type false ctx tyT1; pr ".";
         if (small t2) && not outer then break() else print_space();
         printtm_Term outer ctx' t2;
         cbox())
  | TmLet(fi, x, t1, t2) ->
       obox0();
       pr "let "; pr x; pr " = ";
       printtm_Term false ctx t1;
       print_space(); pr "in"; print_space();
       printtm_Term false (addname ctx x) t2;
       cbox()
  | TmFix(fi, t1) ->
       obox();
       pr "fix ";
       printtm_Term false ctx t1;
       cbox()
  | TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr "then ";
       printtm_Term false ctx t2;
       print_space();
       pr "else ";
       printtm_Term false ctx t3;
       cbox()
  | TmUnpack(fi,tyX,x,t1,t2) ->
      (let (ctx',tyX) = (pickfreshname ctx tyX) in
      let (ctx',x) = (pickfreshname ctx' x) in
      obox(); pr "let {"; pr tyX; pr ","; pr x; pr "} ="; print_space();
      printtm_Term false ctx t1; pr " in ";
      printtm_Term outer ctx' t2; cbox())
  | TmTAbs(fi,x,t) ->
      (let (ctx1,x) = (pickfreshname ctx x) in
            obox(); pr "lambda "; pr x; pr ".";
            if (small t) && not outer then break() else print_space();
            printtm_Term outer ctx1 t;
            cbox())
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | TmTimesfloat(_,t1,t2) ->
       pr "timesfloat "; printtm_ATerm false ctx t2;
       pr " "; printtm_ATerm false ctx t2
  | TmPred(_,t1) ->
       pr "pred "; printtm_ATerm false ctx t1
  | TmIsZero(_,t1) ->
       pr "iszero "; printtm_ATerm false ctx t1
  | TmTApp(fi,t,tyS) ->
      obox0();
      printtm_AppTerm false ctx t;
      print_space();
      pr "["; printty_Type false ctx tyS; pr "]";
      cbox()
  | t -> printtm_PathTerm outer ctx t

and printtm_AscribeTerm outer ctx t = match t with
    TmAscribe(_,t1,tyT1) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space(); pr "as ";
      printty_Type false ctx tyT1;
      cbox()
  | t -> printtm_ATerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(_, t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr l
  | t -> printtm_AscribeTerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmInert(_,tyT) -> pr "inert["; printty_Type false ctx tyT; pr "]"
  | TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmString(_,s) -> pr ("\"" ^ s ^ "\"")
  | TmUnit(_) -> pr "unit"
  | TmRecord(fi, fields) ->
       let pf i (li,ti) =
         if (li <> ((string_of_int i))) then (pr li; pr "=");
         printtm_Term false ctx ti
       in let rec p i l = match l with
           [] -> ()
         | [f] -> pf i f
         | f::rest ->
             pf i f; pr","; if outer then print_space() else break();
             p (i+1) rest
       in pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmFloat(_,s) -> pr (string_of_float s)
  | TmZero(fi) ->
       pr "0"
  | TmSucc(_,t1) ->
     let rec f n t = match t with
         TmZero(_) -> pr (string_of_int n)
       | TmSucc(_,s) -> f (n+1) s
       | _ -> (pr "(succ "; printtm_ATerm false ctx t1; pr ")")
     in f 1 t1
  | TmPack(fi,tyT1,t2,tyT3) ->
      obox(); pr "{*"; printty_Type false ctx tyT1;
      pr ","; if outer then print_space() else break();
      printtm_Term false ctx t2;
      pr "}"; print_space(); pr "as ";
      printty_Type outer ctx tyT3;
      cbox()
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t

let prbinding ctx b = match b with
    NameBind -> ()
  | TyVarBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT
  | TmAbbBind(t,tyT) -> pr "= "; printtm ctx t

-}
