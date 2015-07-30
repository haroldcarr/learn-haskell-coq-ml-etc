#lang racket
(require redex)
(require "common.rkt")
(require "tc-common.rkt")

;;
;; 7 Lab Type Checking
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 7.
;; Extend TLambda with syntax for the following:
;; - additional numeric operators, say, multiplication, subtraction, and division;
;; - let expressions;
;; - Boolean constants plus strict and and or operators as well as a branching construct;
;; - lists, specifically constructors and selectors (de-constructors);
;; - explicitly recursive function definitions.

(define lambda-tc? (redex-match? TLambda-tc e))

(define e1
  (term (lambda ((x int) (f (int -> int))) (+ (f (f x)) (f x)))))
(define e2
  (term (lambda ((x int) (f ((int -> int) -> int))) (f x))))
(define e3
  (term (lambda ((x int) (x (int -> int))) x)))

(module+ test
  (test-equal (judgment-holds (⊢ () ,e1 (int (int -> int) -> int))) #true)
  (test-equal (judgment-holds (⊢ () ,e2 t)) #false)
  (displayln  (judgment-holds (⊢ () ,e1 t) t))
  (displayln  (judgment-holds (⊢ () ,e2 t) t))
  (test-equal (judgment-holds (⊢ () #t bool)) #true)
  (test-equal (judgment-holds (⊢ () (anD #t #t) bool)) #true)
  )

(define-extended-language Standard-tc TLambda-tc
  (e ::= .... b anD oR)
  (b ::= #t #f)
  (t ::= .... bool)
  (v ::= n b anD oR + (lambda ((x t) ...) e))
  (E ::=
     hole
     (v ... E e ...)))

(define s->βv-tc
  (reduction-relation
   Standard-tc
   (--> (in-hole E ((lambda ((x_1 t_1) ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e))
        β)
   (--> (in-hole E (+ n_1 n_2))
        (in-hole E ,(+ (term n_1) (term n_2)))
        +)
   (--> (in-hole E (anD b_1 b_2))
        (in-hole E ,(and (term b_1) (term b_2)))
        anD)
   ))

(define-metafunction Standard-tc
  eval-value-tc : e -> v or closure
  [(eval-value-tc e) any_1 (where any_1 (run-value-tc e))])

(define-metafunction Standard-tc
  run-value-tc : e -> v or closure
  [(run-value-tc n) n]
  [(run-value-tc v) closure]
  [(run-value-tc e)
   (run-value-tc e_again)
   ; (v) means that we expect s->βv to be a function 
   (where (e_again) ,(apply-reduction-relation s->βv-tc (term e)))])


(define-judgment-form Standard-tc
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e t)
  [----------------------- "number"
   (⊢ Γ n int)]

  [----------------------- "boolean"
   (⊢ Γ b bool)]

  [----------------------- "+"
   (⊢ Γ + (int int -> int))]

  [----------------------- "anD"
   (⊢ Γ anD (bool bool -> bool))]

  [----------------------- "variable"
   (⊢ Γ x (lookup Γ x))]

  [(⊢ (extend Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (⊢ Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]

  [(⊢ Γ e_1 (t_2 ... -> t))
   (⊢ Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (⊢ Γ (e_1 e_2 ...) t)])

(module+ test
  (test-results))
