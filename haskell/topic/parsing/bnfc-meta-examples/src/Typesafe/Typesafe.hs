{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Typesafe.Typesafe where

import           Language.LBNF

bnfc [lbnf|

antiquote "[" ":" ":]" ;


Fun.      Prog     ::=  Ident "(" ")" "{" [Stm] "}" ;

SBAss.    Stm      ::= BVar ":=" BExp ";"  ;
SAss.     Stm      ::= NVar "=" Exp ";"  ;

SIncr.    Stm      ::= NVar "++" ";"  ;
SWhile.   Stm      ::= "while" "(" BExp ")" "{" [Stm] "}" ;
SBlock.   Stm      ::= "{" [Stm] "}" ;



EAnd.     BExp0     ::= BExp "&" BExp1 ;
ENeg.     BExp1     ::= "!" BExp1 ;
ELt.      BExp1     ::= Exp "<" Exp ;
ETruth.   BExp1     ::= Truth ;
EBVar.    BExp1     ::= BVar ;

EPlus.    Exp0     ::= Exp0 "+" Exp1 ;
ETimes.   Exp1     ::= Exp1 "*" Exp2 ;
EInt.     Exp2     ::= Integer ;
EVar.     Exp2     ::= NVar ;


TTrue.    Truth    ::= "True" ;
TFalse.   Truth    ::= "False" ;

[].       [Stm]    ::= ;
(:).      [Stm]    ::= Stm [Stm] ;

internal BVar. BVar ::= Ident ;
$ .             BVar ::= Ident ;

internal NVar. NVar ::= Ident ;
$.             NVar ::= Ident ;



-- coercions

_.        Stm      ::= Stm ";" ;

_.  Exp      ::= Exp0 ;
_.  Exp0     ::= Exp1 ;
_.  Exp1     ::= Exp2 ;
_.  Exp2     ::= "[" Exp "]" ;
-- _.  Exp2     ::= Exp3 ;


_.  BExp      ::= BExp0 ;
_.  BExp0     ::= BExp1 ;
_.  BExp1     ::= "(" BExp ")" ;



-- pragmas

comment "/*" "*/" ;
comment "//" ;

entrypoints Prog, Stm, Exp, BExp ;
  |]




