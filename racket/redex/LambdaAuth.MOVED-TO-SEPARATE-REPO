#lang racket

(require redex)
(require openssl/sha1)

;; ------------------------------------------------------------------------------
;; syntax

(define-language λα-syntax
  ;; Program
  (P ::= (prog Π e))
  ;; proof streams
  (Π ::= (π h ...))
  (h ::= string)
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
  (rec x_1 (λ (x_2) e #:refers-to (shadow x_1 x_2)))
)

(define λα-syntax-e? (redex-match? λα-syntax e))
(define λα-syntax-P? (redex-match? λα-syntax P))

(module+ test
  ;; examples
  (define p-rec-1 (term (prog (π) ((rec x (λ (y) y))
                                   unit))))
  (define p-rec-2 (term (prog (π) ((rec x (λ (y) (x y)))
                                   unit))))
  (define p1 (term (prog (π) (let [(f (λ (z) (prod (prod z z) z)))]
                               (f unit)))))
  (define p2 (term (prog (π) (let [(f (λ (z) (prod (prod z z) z)))]
                               (let [(c (case (inj1 unit) f f))]
                                 (prj1 c))))))
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

(default-language λα)

(define λα-E? (redex-match? λα E))

(module+ test
  (test-equal (λα-E? (term hole)) #t)
  (test-equal (λα-E? (term (let ((x hole)) x))) #t)
  )

(define -->βv
  (reduction-relation
   λα
   #:domain P
   (--> (prog Π (in-hole E ((λ (x) e) v)))
        (prog Π (in-hole E (substitute e x v)))
        "βv-apply")
   (--> (prog Π (in-hole E ((rec x_1 (λ (x_2) e_1))
                            v)))
        (prog Π (in-hole E ((λ (x_2) e_2)
                            v)))
        (where e_2 (substitute e_1 x_1 (rec x_1 (λ (x_2) e_1))))
        "βv-rec")
   (--> (prog Π (in-hole E (let ((x v)) e)))
        (prog Π (in-hole E (substitute e x v)))
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
   (--> (prog Π (in-hole E (unroll (roll v))))
        (prog Π (in-hole E v))
        "βv-unroll-roll")
   ))

(module+ test
  (test--> -->βv
           (term (prog (π) ((λ (x) x) unit)))
           (term (prog (π) unit)))
  ;(traces -->βv p-rec-1)
  (test-->> -->βv
           p-rec-1
           (term (prog (π) unit)))
  ;(traces -->βv p-rec-2)
  #;
  (test--> -->βv
          p-rec-2
          (term (prog (π ()) ((λ (y) ((rec x (λ (y) (x y)))
                                      y))
                              unit))))
  (test-->> -->βv
            (term (prog (π)
                        (let ((x       (prod (prod unit unit)
                                             unit)))
                          x)))
            (term (prog (π)            (prod (prod unit unit)
                                             unit))))
  (test-->> -->βv
            (term (prog (π)
                        (let ((x (prj1 (prod (prod unit unit)
                                             unit))))
                          x)))
            (term (prog (π)                  (prod unit unit))))
  (test--> -->βv
           (term (prog (π) (case (inj1 unit)
                             (λ (x) x)
                             (λ (y) y))))
           (term (prog (π) ((λ (x) x) unit))))
  (test--> -->βv
           (term (prog (π) (case (inj2 unit)
                             (λ (x) x)
                             (λ (y) y))))
           (term (prog (π) ((λ (y) y) unit))))
  (test-->> -->βv
            (term (prog (π) (case (inj1 unit)
                              (λ (x) x)
                              (λ (y) y))))
            (term (prog (π) unit)))
  (test--> -->βv
           (term (prog (π) (prj1 (prod (prod unit unit) unit))))
           (term (prog (π) (prod unit unit))))
  (test--> -->βv
           (term (prog (π) (prj2 (prod (prod unit unit) unit))))
           (term (prog (π) unit)))
  (test--> -->βv
           (term (prog (π) (unroll (roll unit))))
           (term (prog (π) unit)))
  (test-->> -->βv
            p1
            (term (prog (π) (prod (prod unit unit) unit))))
  (test-->> -->βv
            p2
            (term (prog (π) (prod unit unit))))
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
   (--> (prog Π (in-hole E (auth v)))
        (prog Π (in-hole E (α (hash-shallow v) v)))
        "-->P-auth")
   (--> (prog (π h ...)
              (in-hole E (unauth (α h_1 v_1))))
        (prog (π h ... (shallow-projection v_1))
              (in-hole E v_1))
        "-->P-unauth")
   ))

(define -->V
  (extend-reduction-relation -->βv λα-auth
   #:domain P
   (--> (prog Π (in-hole E (auth v)))
        (prog Π (in-hole E (hash v)))
        "-->V-auth")
   (--> (prog (π h_s0 h_1 ...)
              (in-hole E (unauth h)))
        (prog (π h_1 ...)
              (in-hole E h_s0))
        (side-condition (equal? (rhash (term h_s0))
                                (term h)))
        "-->V-unauth")
   ))

(define-metafunction λα-auth
  hash : v -> h
  [(hash v) h
   (where h ,(rhash (term v)))])

(define-metafunction λα-auth
  hash-shallow : v -> h
  [(hash-shallow v) (hash (shallow-projection v))])

(define-metafunction λα-auth
  shallow-projection : v -> h
  [(shallow-projection any) h
   (where h ,(format "~s" (term (sp any))))])

(define-metafunction λα-auth
  sp : v -> v
  [(sp unit)
   unit]
  [(sp x)
   x]
  [(sp (α h v))
   h]
  [(sp (λ (x) e))
   (λ (x) (sp e))]
  [(sp (auth v))
   (auth (sp v))]
  [(sp (unauth v))
   (unauth (sp v))]
  [(sp (prod v_1 v_2))
   (prod (sp v_1) (sp v_2))]
  [(sp (prj1 v))
   (prj1 (sp v))]
  [(sp (prj2 v))
   (prj2 (sp v))]
  [(sp (roll v))
   (roll (sp v))]
  [(sp (unroll v))
   (unroll (sp v))]
  [(sp (rec x_1 (λ (x_2) e)))
   (rec x_1 (sp (λ (x_2) e)))]
  [(sp (inj1 v))
   (inj1 (sp v))]
  [(sp (inj2 v))
   (inj2 (sp v))]
  [(sp (case v_1 v_2 v_3))
   (case (sp v_1)
         (sp v_2)
         (sp v_3))]
  [(sp (let ((x e_1)) e_2))
   (let ((x (sp e_1)))
     (sp e_2))]
  )

(define (rhash x)
  (sha1 (open-input-string x)))

(module+ test
  (test--> -->P
           (term (prog (π) (auth unit)))
           (term (prog (π) (α "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e"
                              unit))))
  (test--> -->P
           (term (prog (π) (unauth (α "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e"
                                      unit))))
           (term (prog (π "unit") unit)))
  (test--> -->V
           (term (prog (π) (auth "unit")))
           (term (prog (π) "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e")))
  (test--> -->V
           (term (prog (π "unit")
                       (unauth "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e")))
           (term (prog (π) "unit")))
  ;; -------------------------
  (test--> -->P
           (term (prog (π) (auth x)))
           (term (prog (π) (α "11f6ad8ec52a2984abaafd7c3b516503785c2072"
                              x))))
  ;; -------------------------
  (test--> -->P
           (term (prog (π) (auth (λ (x) unit))))
           (term (prog (π) (α "238a0d5fb845295966f12fa741b8bcd8ec51d22c"
                              (λ (x) unit)))))
  ;; =========================
  (define ppp
    (term (prog (π) (let ((a (auth unit)))
                      (let ((b (prod a a)))
                        (let ((c (prod a b)))
                          (prj2 c)))))))
  ;(traces -->P ppp)
  (test-->> -->P
            ppp
            (term (prog (π)
                        (prod
                         (α "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e" unit)
                         (α "0df9eea0bad5a55395db9ec290dfcf4a883d5d3e" unit)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-results))
