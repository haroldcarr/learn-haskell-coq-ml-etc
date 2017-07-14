#lang racket

(require redex)

(define-language PCF
  (e ::=
     x
     (lambda (x) e)
     (e e)
     (if e e e)
     (e + e)
     (e ++ e)
     t
     )
  (t ::=
     ;; booleans
     tt
     ff
     ;; arithmetic
     n
     ;; strings
     s
     )
  (n ::=
     integer)
  (s ::=
     string)
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (lambda (x) e #:refers-to x))

(define PCF? (redex-match? PCF e))

(define-extended-language PCF-eval PCF
  (e ::=
     ....
     (err s (e ... )) ;; e includes err ; which I do not want in this case
     )

  (E-name ::=
     hole
     (E-name e)
     (if E-name e e)
     (E-name + e)
     (E-name ++ e)
     (v + E-name)
     (v ++ E-name)
     )
  (v ::=
     n
     s
     tt
     ff
     (lambda (x) e)))

(define PCF-eval? (redex-match? PCF-eval E-name))

(define ->name
  (reduction-relation
   PCF-eval
   #:domain e
   (--> (in-hole E-name ((lambda (x) e_1) e_2))
        (in-hole E-name (substitute e_1 x e_2))
        beta-name)
   (--> (in-hole E-name (t_1 e_2))
        (in-hole E-name (err "terminal in function position" (t_1)))
        beta-name-bad)
   (--> (in-hole E-name (if tt e_1 e_2))
        (in-hole E-name e_1)
        if-tt)
   (--> (in-hole E-name (if ff e_1 e_2))
        (in-hole E-name e_2)
        if-ff)
   (--> (in-hole E-name (if t_1 e_1 e_2))
        (err "non boolean terminal in if/test position" (t_1)))
   (--> (in-hole E-name (n_1 + n_2))
        (in-hole E-name ,(+ (term n_1) (term n_2)))
        plus)
   (--> (in-hole E-name (t_1 + t_2))
        (err "non number to plus" (t_1 t_2))
        plus-err)
   (--> (in-hole E-name (s_1 ++ s_2))
        (in-hole E-name ,(string-append (term s_1) (term s_2)))
        string-append)
   (--> (in-hole E-name (t_1 ++ t_2))
        (err "non string to string append" (t_1 t_2))
        string-append-err)
  ))


(test--> ->name (term (if 0 1 2))
         '(err "non boolean terminal in if/test position" (0)))
(test--> ->name (term (1 + "s"))
         '(err "non number to plus" (1 "s")))
(test--> ->name (term ("s" ++ 1))
         '(err "non string to string append" ("s" 1)))
(test--> ->name (term (1 ++ "s"))
         '(err "non string to string append" (1 "s")))
(test--> ->name (term ("foo" 3))
         '(err "terminal in function position" ("foo")))
(test--> ->name (term ((lambda (x) x) 3))
         3)

(test-results)
