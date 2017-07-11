#lang racket

(require redex)

(define-language λα
  ;; types
  (τ ::=
     1                      ;; unit type
     α                      ;; variable types
     (τ -> τ)               ;; function type
     (+ τ τ)                ;; sum type
     (* τ τ)                ;; product type
     (μ α τ)                ;; recursive type
     (auth τ)               ;; authenticated type
     )
  ;; values
  (v ::=
     unit                   ;; unit value
     x                      ;; variable value
     (lambda (x) e)         ;; function value           ;; ** 04-reductions-...
     (rec x (lambda (x) e)) ;; recursive function value ;; ** 04-reductions-...
     (inj1 v)               ;; sum value
     (inj2 v)               ;; sum value
     (prod v v)             ;; product value
     (roll v)               ;; introduce 1 level of recursive value
     )
  ;; expressions
  (e ::=
     v                      ;; values
     (let ((x e)) e)        ;; local binding
     (v v)                  ;; call-by-value application
     (case v v v)           ;; elimination of sum type
     (prj1 v)               ;; projection of product
     (prj2 v)               ;; projection of product
     (unroll v)             ;; eliminate 1 level of recursion
     (auth v)               ;; create authenticate value
     (unauth v)             ;; get value from authenticated value
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
#;
(define-judgment-form λα
  #:mode (α I I I O)
  [(α x e v ,(subst (((v x)) e)))])
#;
(module+ test
  (test-equal
      (judgment-holds (α x x unit unit))
    #t)
  )

(define-judgment-form λα
  #:mode (δ I I O)
  [(δ prj1 (prod v_1 v_2) v_1)]
  [(δ prj2 (prod v_1 v_2) v_2)])

(module+ test
  (test-equal (judgment-holds (δ prj1
                                 (prod (prod unit unit) unit)
                                 (prod unit unit)))
              #t)
  (test-equal (judgment-holds (δ prj2
                                 (prod (prod unit unit) unit)
                                 unit))
              #t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTIL

;; -----------------------------------------------------------------------------
;; (in x x_1 ...) is x a member of (x_1 ...)?

(module+ test
  (test-equal (term (in x (y z x y z))) #true)
  (test-equal (term (in x ())) #false)
  (test-equal (term (in x (y z w))) #false))

(define-metafunction λα
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #true]
  [(in x (x_1 ...)) #false])

;; -----------------------------------------------------------------------------
;; (=α e_1 e_2) determines whether e_1 and e_2 are α equivalent

(module+ test
  (test-equal (term (=α (lambda (x) x) (lambda (y) y))) #true)
  (test-equal (term (=α (lambda (x) (x 1)) (lambda (y) (y 1)))) #true)
  (test-equal (term (=α (lambda (x) x) (lambda (y) z))) #false))

(define-metafunction λα
  =α : any any -> boolean
  [(=α any_1 any_2) ,(equal? (term (sd any_1)) (term (sd any_2)))])

;; a Racket definition for use in Racket positions 
(define (=α/racket x y) (term (=α ,x ,y)))

;; (sd e) computes the static distance version of e
(define-extended-language SD λα
  (e ::= .... (K n))
  (n ::= natural))

(define SD? (redex-match? SD e))

(module+ test
  (define sd1 (term (K 1)))
  (define sd2 (term 1))

  (test-equal (SD? sd1) #true))

(define-metafunction SD
  sd : any -> any
  [(sd any_1) (sd/a any_1 ())])

(module+ test
  (test-equal (term (sd/a x ())) (term x))
  (test-equal (term (sd/a x ((y) (z) (x)))) (term (K 2 0)))
  (test-equal (term (sd/a ((lambda (x) x) (lambda (y) y)) ()))
              (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
  (test-equal (term (sd/a (lambda (x) (x (lambda (y) y))) ()))
              (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
  (test-equal (term (sd/a (lambda (z x) (x (lambda (y) z))) ()))
              (term (lambda () ((K 0 1) (lambda () (K 1 0)))))))

(define-metafunction SD
  sd/a : any ((x ...) ...) -> any
  [(sd/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ;; bound variable
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  [(sd/a (lambda (x ...) any_1) (any_rest ...))
   (lambda () (sd/a any_1 ((x ...) any_rest ...)))]
  [(sd/a (any_fun any_arg ...) (any_rib ...))
   ((sd/a any_fun (any_rib ...)) (sd/a any_arg (any_rib ...)) ...)]
  [(sd/a any_1 any)
   ;; free variable, constant, etc
   any_1])

;; -----------------------------------------------------------------------------
;; (subst ([e x] ...) e_*) substitutes e ... for x ... in e_* (hygienically)

(module+ test
  (test-equal (term (subst ([1 x][2 y]) x)) 1)
  (test-equal (term (subst ([1 x][2 y]) y)) 2)
  (test-equal (term (subst ([1 x][2 y]) z)) (term z))
  (test-equal (term (subst ([1 x][2 y]) (lambda (z w) (x y))))
              (term (lambda (z w) (1 2))))
  (test-equal (term (subst ([1 x][2 y]) (lambda (z w) (lambda (x) (x y)))))
              (term (lambda (z w) (lambda (x) (x 2))))
              #:equiv =α/racket)
  (test-equal (term (subst ((2 x)) ((lambda (x) (1 x)) x)))
              (term ((lambda (x) (1 x)) 2))
              #:equiv =α/racket))

(define-metafunction λα
  subst : ((any x) ...) any -> any
  [(subst [(any_1 x_1) ... (any_x x) (any_2 x_2) ...] x) any_x]
  [(subst [(any_1 x_1) ... ] x) x]
  [(subst [(any_1 x_1) ... ] (lambda (x ...) any_body))
   (lambda (x_new ...)
     (subst ((any_1 x_1) ...)
            (subst-raw ((x_new x) ...) any_body)))
   (where  (x_new ...)  ,(variables-not-in (term any_body) (term (x ...)))) ]
  [(subst [(any_1 x_1) ... ] (any ...)) ((subst [(any_1 x_1) ... ] any) ...)]
  [(subst [(any_1 x_1) ... ] any_*) any_*])

(define-metafunction λα
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((x_n1 x_o1) ... (x_new x) (x_n2 x_o2) ...) x) x_new]
  [(subst-raw ((x_n1 x_o1) ... ) x) x]
  [(subst-raw ((x_n1 x_o1) ... ) (lambda (x ...) any))
   (lambda (x ...) (subst-raw ((x_n1 x_o1) ... ) any))]
  [(subst-raw [(any_1 x_1) ... ] (any ...))
   ((subst-raw [(any_1 x_1) ... ] any) ...)]
  [(subst-raw [(any_1 x_1) ... ] any_*) any_*])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-results))
