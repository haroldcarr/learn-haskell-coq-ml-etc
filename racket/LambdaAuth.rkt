#lang racket

(require redex)
(require openssl/sha1)

;; ------------------------------------------------------------------------------
;; syntax

(define-language λα-syntax
  ;; Program
  (P ::= (prog Π e))
  ;; proof streams
  (Π ::= (π (h ... )))
  (h ::= string)
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
     (λ (x) e)              ;; function value
     (rec x (λ (x) e))      ;; recursive function value
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
                            ;; administrative normal form
     (case v v v)           ;; elimination of sum type
     (prj1 v)               ;; projection of product
     (prj2 v)               ;; projection of product
     (unroll v)             ;; eliminate 1 level of recursion
     (auth v)               ;; create authenticate value
     (unauth v)             ;; get value from authenticated value
     )
  ;; variables
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x)
  (λ (x) e #:refers to x)
  (rec x_1 (λ (x_2) e #:refers-to x_2)) ;; AND to x_1
)

(define λα-syntax-τ? (redex-match? λα-syntax τ))
(define λα-syntax-e? (redex-match? λα-syntax e))
(define λα-syntax-P? (redex-match? λα-syntax P))

(define p-rec-1 (term (prog (π ()) ((rec x (λ (y) y))
                                    unit))))
(define p-rec-2 (term (prog (π ()) ((rec x (λ (y) (x y)))
                                    unit))))
(define p1 (term (prog (π ()) (let [(f (λ (z) (prod (prod z z) z)))]
                                (f unit)))))
(define p2 (term (prog (π ()) (let [(f (λ (z) (prod (prod z z) z)))]
                                (let [(c (case (inj1 unit) f f))]
                                  (prj1 c))))))

(module+ test
  ;; types
  (test-equal (λα-syntax-τ? (term (1 -> 1))) #t)
  ;; values
  (test-equal (λα-syntax-e? (term unit)) #t)
  (test-equal (λα-syntax-e? (term z)) #t)
  (test-equal (λα-syntax-e? (term (λ (w) z))) #t)
  (test-equal (λα-syntax-e? (term (rec x (λ (x) e)))) #t)
  (test-equal (λα-syntax-e? (term (inj1 unit))) #t)
  (test-equal (λα-syntax-e? (term (inj2 z))) #t)
  (test-equal (λα-syntax-e? (term (unit unit))) #t)
  (test-equal (λα-syntax-e? (term (roll z))) #t)
  ;; expressions
  (test-equal (λα-syntax-e? (term (let ((z unit)) (inj1 z)))) #t)
  (test-equal (λα-syntax-e? (term (w z))) #t)
  (test-equal (λα-syntax-e? (term (case unit (λ (w) w) (λ (z) z)))) #t)
  ;; programs
  (test-equal (λα-syntax-P? p-rec-1) #t)
  (test-equal (λα-syntax-P? p-rec-2) #t)
  (test-equal (λα-syntax-P? p1) #t)
  (test-equal (λα-syntax-P? p2) #t)
  )

;; ------------------------------------------------------------------------------
;; evaluation

(define-extended-language λα λα-syntax
  (E ::=
     hole
     (let ((x E)) e)
     ))

(define λα-E? (redex-match? λα E))

(module+ test
  (test-equal (λα-E? (term hole)) #t)
  (test-equal (λα-E? (term (let ((x hole)) x))) #t)
  )

(define -->βv
  (reduction-relation
   λα
   #:domain P
   (--> (prog Π (in-hole E ((λ (x_1) e_1) v_1)))
        (prog Π (in-hole E (subst ([v_1 x_1]) e_1)))
        "βv-apply")
   (--> (prog Π (in-hole E ((rec x_1 (λ (x_2) e_1))
                            v_1)))
        (prog Π (in-hole E ((λ (x_2) e_2)
                            v_1)))
        (where e_2 (subst [((rec x_1 (λ (x_2) e_1))
                            x_1)]
                          e_1))
        "βv-rec")
   (--> (prog Π (in-hole E (let ((x_1 v_1)) e_2)))
        (prog Π (in-hole E (subst ([v_1 x_1]) e_2)))
        "βv-let")
   (--> (prog Π (in-hole E (case (inj1 v_1) v_2 v_3)))
        (prog Π (in-hole E (v_2 v_1)))
        "βv-case-inj1")
   (--> (prog Π (in-hole E (case (inj2 v_1) v_2 v_3)))
        (prog Π (in-hole E (v_3 v_1)))
        "βv-case-inj2")
   (--> (prog Π (in-hole E (prj1 (prod v_1 v_2))))
        (prog Π (in-hole E v_1))
        "βv-prj1")
   (--> (prog Π (in-hole E (prj2 (prod v_1 v_2))))
        (prog Π (in-hole E v_2))
        "βv-prj2")
   (--> (prog Π (in-hole E (unroll (roll v_1))))
        (prog Π (in-hole E v_1))
        "βv-unroll-roll")
   ))

(module+ test
  (test--> -->βv
           (term (prog (π ()) ((λ (x) x) unit)))
           (term (prog (π ()) unit)))
  ;(traces -->βv p-rec-1)
  (test-->> -->βv
           p-rec-1
           (term (prog (π ()) unit)))
  ;(traces -->βv p-rec-2)
  #;
  (test--> -->βv
          p-rec-2
          (term (prog (π ()) ((λ (y) ((rec x (λ (y) (x y)))
                                      y))
                              unit))))
  (test-->> -->βv
            (term (prog (π ())
                        (let ((x       (prod (prod unit unit)
                                             unit)))
                          x)))
            (term (prog (π ())         (prod (prod unit unit)
                                             unit))))
  (test-->> -->βv
            (term (prog (π ())
                        (let ((x (prj1 (prod (prod unit unit)
                                             unit))))
                          x)))
            (term (prog (π ())               (prod unit unit))))
  (test--> -->βv
           (term (prog (π ()) (case (inj1 unit)
                                (λ (x) x)
                                (λ (y) y))))
           (term (prog (π ()) ((λ (x) x) unit))))
  (test--> -->βv
           (term (prog (π ()) (case (inj2 unit)
                                (λ (x) x)
                                (λ (y) y))))
           (term (prog (π ()) ((λ (y) y) unit))))
  (test-->> -->βv
            (term (prog (π ()) (case (inj1 unit)
                                 (λ (x) x)
                                 (λ (y) y))))
            (term (prog (π ()) unit)))
  (test--> -->βv
           (term (prog (π ()) (prj1 (prod (prod unit unit) unit))))
           (term (prog (π ()) (prod unit unit))))
  (test--> -->βv
           (term (prog (π ()) (prj2 (prod (prod unit unit) unit))))
           (term (prog (π ()) unit)))
  (test--> -->βv
           (term (prog (π ()) (unroll (roll unit))))
           (term (prog (π ()) unit)))
  (test-->> -->βv
            p1
            (term (prog (π ()) (prod (prod unit unit) unit))))
  (test-->> -->βv
            p2
            (term (prog (π ()) (prod unit unit))))
  )

;; =============================================================================
;; authentication

(define-extended-language λα-auth λα
  (v ::= ....
     h
     (α h v))
  )

(define -->P
  (extend-reduction-relation -->βv λα-auth
   #:domain P
   (--> (prog Π (in-hole E (auth v_1)))
        (prog Π (in-hole E (α h_1 v_1)))
        (where h_1 (hash-shallow v_1))
        "-->P-auth")
   (--> (prog (π (h ...)) (in-hole E (α h_1 v_1)))
        (prog (π (h ... v_2)) (in-hole E v_1))
        (where v_2 (shallow-projection v_1))
        "-->P-unauth")
   ))

(define-metafunction λα-auth
  hash-shallow : v -> h
  [(hash-shallow v) h
   (where h ,(hash (term (shallow-projection v))))])

(define-metafunction λα-auth
  shallow-projection : v -> h
  [(shallow-projection any) h
   (where h ,(format "~s" (term (shallow-projection-aux any))))])

(define-metafunction λα-auth
  shallow-projection-aux : v -> v
  [(shallow-projection-aux unit)
   unit]
  [(shallow-projection-aux x)
   x]
  [(shallow-projection-aux (α h v))
   h]
  [(shallow-projection-aux (λ (x) e))
   (λ (x) (shallow-projection-aux e))]
  [(shallow-projection-aux (auth v))
   (auth (shallow-projection-aux v))]
  [(shallow-projection-aux (unauth v))
   (unauth (shallow-projection-aux v))]
  [(shallow-projection-aux (prod v_1 v_2))
   (prod (shallow-projection-aux v_1) (shallow-projection-aux v_2))]
  [(shallow-projection-aux (prj1 v))
   (prj1 (shallow-projection-aux v))]
  [(shallow-projection-aux (prj2 v))
   (prj2 (shallow-projection-aux v))]
  [(shallow-projection-aux (roll v))
   (roll (shallow-projection-aux v))]
  [(shallow-projection-aux (unroll v))
   (unroll (shallow-projection-aux v))]
  [(shallow-projection-aux (rec x_1 (λ (x_2) e)))
   (rec x_1 (shallow-projection-aux (λ (x_2) e)))]
  [(shallow-projection-aux (inj1 v))
   (inj1 (shallow-projection-aux v))]
  [(shallow-projection-aux (inj2 v))
   (inj2 (shallow-projection-aux v))]
  [(shallow-projection-aux (case v_1 v_2 v_3))
   (case (shallow-projection-aux v_1)
         (shallow-projection-aux v_2)
         (shallow-projection-aux v_3))]
  [(shallow-projection-aux (let ((x e_1)) e_2))
   (let ((x (shallow-projection-aux e_1)))
     (shallow-projection-aux e_2))]
  )

(define (hash x)
  (sha1 (open-input-string x)))

(module+ test
  (test--> -->P
           (term (prog (π ()) (auth unit)))
           (term (prog (π ()) (α "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e"
                                 unit))))
  (test--> -->P
           (term (prog (π ()) (unauth (α "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e"
                                         unit))))
           (term (prog (π ("0df9eea0bad5a55395db9ec290dfcf4a883d5d3e"))
                       unit)))
  (test--> -->P
           (term (prog (π ()) (auth x)))
           (term (prog (π ()) (α "11f6ad8ec52a2984abaafd7c3b516503785c2072"
                                 x))))
  (test--> -->P
           (term (prog (π ()) (auth (λ (x) unit))))
           (term (prog (π ()) (α "238a0d5fb845295966f12fa741b8bcd8ec51d22c"
                                 (λ (x) unit)))))
  )

;; =============================================================================
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
  (test-equal (term (=α (λ (x) x) (λ (y) y))) #true)
  (test-equal (term (=α (λ (x) (x 1)) (λ (y) (y 1)))) #true)
  (test-equal (term (=α (λ (x) x) (λ (y) z))) #false))

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
  (test-equal (term (sd/a ((λ (x) x) (λ (y) y)) ()))
              (term ((λ () (K 0 0)) (λ () (K 0 0)))))
  (test-equal (term (sd/a (λ (x) (x (λ (y) y))) ()))
              (term (λ () ((K 0 0) (λ () (K 0 0))))))
  (test-equal (term (sd/a (λ (z x) (x (λ (y) z))) ()))
              (term (λ () ((K 0 1) (λ () (K 1 0)))))))

(define-metafunction SD
  sd/a : any ((x ...) ...) -> any
  [(sd/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ;; bound variable
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  [(sd/a (λ (x ...) any_1) (any_rest ...))
   (λ () (sd/a any_1 ((x ...) any_rest ...)))]
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
  (test-equal (term (subst ([1 x][2 y]) (λ (z w) (x y))))
              (term (λ (z w) (1 2))))
  (test-equal (term (subst ([1 x][2 y]) (λ (z w) (λ (x) (x y)))))
              (term (λ (z w) (λ (x) (x 2))))
              #:equiv =α/racket)
  (test-equal (term (subst ((2 x)) ((λ (x) (1 x)) x)))
              (term ((λ (x) (1 x)) 2))
              #:equiv =α/racket))

(define-metafunction λα
  subst : ((any x) ...) any -> any
  [(subst [(any_1 x_1) ... (any_x x) (any_2 x_2) ...] x) any_x]
  [(subst [(any_1 x_1) ... ] x) x]
  [(subst [(any_1 x_1) ... ] (λ (x ...) any_body))
   (λ (x_new ...)
     (subst ((any_1 x_1) ...)
            (subst-raw ((x_new x) ...) any_body)))
   (where  (x_new ...)  ,(variables-not-in (term any_body) (term (x ...)))) ]
  [(subst [(any_1 x_1) ... ] (any ...)) ((subst [(any_1 x_1) ... ] any) ...)]
  [(subst [(any_1 x_1) ... ] any_*) any_*])

(define-metafunction λα
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((x_n1 x_o1) ... (x_new x) (x_n2 x_o2) ...) x) x_new]
  [(subst-raw ((x_n1 x_o1) ... ) x) x]
  [(subst-raw ((x_n1 x_o1) ... ) (λ (x ...) any))
   (λ (x ...) (subst-raw ((x_n1 x_o1) ... ) any))]
  [(subst-raw [(any_1 x_1) ... ] (any ...))
   ((subst-raw [(any_1 x_1) ... ] any) ...)]
  [(subst-raw [(any_1 x_1) ... ] any_*) any_*])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-results))
