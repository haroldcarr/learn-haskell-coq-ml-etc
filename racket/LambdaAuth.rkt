#lang racket

(require redex)

(define-language λα
  ;; types
  (τ ::=
     1         ;; unit type
     α         ;; variable types
     (τ -> τ)  ;; function type
     (+ τ τ)   ;; sum type
     (* τ τ)   ;; product type
     (μ α τ)   ;; recursive type
     (auth τ)  ;; authenticated type
     )
  ;; values
  (v ::=
     unit                          ;; unit value
     x                             ;; variable value
     (lambda (x) e)            ;; function value           ;; ** 04-reductions-...
     (rec x (lambda (x) e))    ;; recursive function value ;; ** 04-reductions-...
     (inj1 v)                      ;; sum value
     (inj2 v)                      ;; sum value
     (pair v v)                         ;; product value
     (roll v)                      ;; introduce 1 level of recursive value
     )
  ;; expressions
  (e ::=
     v                       ;; values
     (let ((x e)) e)         ;; local binding
     (v v)               ;; call-by-value application
     (case v v v)            ;; elimination of sum type
     (prj1 v)                ;; projection of product
     (prj2 v)                ;; projection of product
     (unroll v)              ;; eliminate 1 level of recursion
     (auth v)                ;; create authenticate value
     (unauth v)              ;; get value from authenticated value
     )
  ;; variables
  (x ::= variable-not-otherwise-mentioned)
)

(define λα-τ? (redex-match? λα τ))
(define λα-e? (redex-match? λα e))

(module+ test
  ;; types
  (test-equal (λα-τ? (term (1 -> 1))) #t)
  ;; values
  (test-equal (λα-e? (term unit)) #t)
  (test-equal (λα-e? (term z)) #t)
  (test-equal (λα-e? (term (lambda (w) z))) #t)
  (test-equal (λα-e? (term (inj1 unit))) #t)
  (test-equal (λα-e? (term (inj2 z))) #t)
  (test-equal (λα-e? (term (unit unit))) #t)
  (test-equal (λα-e? (term (roll z))) #t)
  ;; expressions
  (test-equal (λα-e? (term (let ((z unit)) (inj1 z)))) #t)
  (test-equal (λα-e? (term (w z))) #t)
  (test-equal (λα-e? (term (case unit (lambda (w) w) (lambda (z) z)))) #t)
  )

(define-language nats
  (n ::= z (s n)))

(define-judgment-form nats
  #:mode (sum I I O)
  #:contract (sum n n n)

  [---------- "zero"
   (sum z n n)]

  [(sum n_1 n_2 n_3)
   ----------------- "add1"
   (sum (s n_1) n_2 (s n_3))])

(module+ test
  (test-equal (judgment-holds (sum (s (s z)) (s z) (s (s (s z))))) #t)
  (test-equal (judgment-holds (sum (s (s z)) (s z) (s (s (s n))))) #t)
  (test-equal (judgment-holds (sum (s (s z)) (s z) (s (s (s (s n)))))) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-results))
