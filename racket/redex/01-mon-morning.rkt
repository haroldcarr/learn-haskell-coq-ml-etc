#lang racket

;; monday morning

(require redex)

(define-language Λ
  (e ::=
     x
     (λ (x) e)
     (e e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ (x) e #:refers-to x))

(define term1 (term (λ (x) y)))

(default-language Λ)

(define-extended-language Lambda-calculus Λ
  (C ::=
     hole
     (lambda (x) C)
     (C e)
     (e C)))

(define ->name
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole C ((λ (x) e_1) e_2))
        (in-hole C (substitute e_1 x e_2))
        beta-name)))

(define ->value
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole C ((λ (x) e_1) (λ (x_y) e_2)))
        (in-hole C (substitute e_1 x (λ (x_y) e_2)))
        beta-value)))

(apply-reduction-relation ->name  (term ((λ (x) y) x)))
(apply-reduction-relation ->value (term ((λ (x) y) x)))

(apply-reduction-relation
 ->name
 (term ((λ (x) ((λ (z) z) a)) x)))

(traces
 ->value
 (term ((λ (x) ((λ (z) z) a)) x)))
