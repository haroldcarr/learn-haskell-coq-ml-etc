#lang racket
(require redex)
(require "common.rkt")
(require "tc-common.rkt")

;;
;; 7 Lab Type Checking
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 6.
;; Develop a reduction system for which the trace expression from the
;; lecture preserves types
#|
(module+ test
  (traces ->
          (term (((lambda ((x (int -> int))) x) (lambda ((x int)) x)) 1))
          #:pred (lambda (e)
                   (judgment-holds (⊢ () ,e int)))))
|#

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
  (displayln  (judgment-holds (⊢ () ,e2 t) t)))

(define-extended-language Standard-tc TLambda-tc
  (v ::= n + (lambda ((x t) ...) e))
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


(define-judgment-form TLambda-tc
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e t)
  [----------------------- "number"
   (⊢ Γ n int)]

  [----------------------- "+"
   (⊢ Γ + (int int -> int))]

  [----------------------- "variable"
   (⊢ Γ x (lookup Γ x))]

  [(⊢ (extend Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (⊢ Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]

  [(⊢ Γ e_1 (t_2 ... -> t))
   (⊢ Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (⊢ Γ (e_1 e_2 ...) t)])

; this now works
#;
(module+ test
  (traces s->βv-tc
          (term (((lambda ((x (int -> int))) x) (lambda ((x int)) x)) 1))
          #:pred (lambda (e)
                   (judgment-holds (⊢ () ,e int)))))

; this works too
#;
(module+ test
  (traces s->βv-tc
          (term (+ 1 (+ (+ 2 3) (+ 4 5))))
          #:pred (lambda (e)
                   (judgment-holds (⊢ () ,e int)))))

(module+ test
  (test-results))
