{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module JavaLight.JavaletteLight where

import           Language.LBNF.Compiletime
import           Language.LBNF             (bnfc, dumpCode, lbnf)

bnfc [lbnf|

Fun.      Prog     ::= Typ Ident "(" ")" "{" [Stm] "}" ;

SDecl.    Stm      ::= Typ Ident ";"  ;
SAss.     Stm      ::= Ident "=" Expr ";"  ;
SIncr.    Stm      ::= Ident "++" ";"  ;
SWhile.   Stm      ::= "while" "(" Expr ")" "{" [Stm] "}" ;
SFunApp.  Stm      ::= Ident "(" [Expr] ")" ";" ;
_.        Stm      ::= Stm ";" ;

ELt.      Expr0     ::= Expr1 "<" Expr1 ;
EPlus.    Expr1     ::= Expr1 "+" Expr2 ;
ETimes.   Expr2     ::= Expr2 "*" Expr3 ;
EVar.     Expr3     ::= Ident ;
EInt.     Expr3     ::= Integer ;
EDouble.  Expr3     ::= Double ;
$.        Expr3     ::= "$" Ident;
[].       [Stm]    ::= ;
(:).      [Stm]    ::= Stm [Stm] ;

separator Expr "," ;




_.  Expr      ::= Expr0 ;
_.  Expr0     ::= Expr1 ;
_.  Expr1     ::= Expr2 ;
_.  Expr2     ::= Expr3 ;
_.  Expr3     ::= "(" Expr ")" ;

TInt.     Typ  ::= "int" ;
TDouble.  Typ  ::= "double" ;
|]


