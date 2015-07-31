#lang racket
(require redex)
(require "close.rkt")
(require "common.rkt")

;;
;; 5 Lab Designing Reductions
;;

;; Given:

(define-extended-language Lambda-η Lambda
  (e ::= .... n)
  (n ::= natural)
  (C ::=
     hole
     (e ... C e ...)
     (lambda (x_!_ ...) C))
  (v ::=                     ; values
     n
     (lambda (x ...) e)))

(define lambda-η? (redex-match? Lambda-η e))

(define -->β
  (reduction-relation
   Lambda-η
   ;; call-by-name (because 'e' as args instead of 'v')
   (--> (in-hole C ((lambda (x_1 ..._n) e) e_1 ..._n))
        (in-hole C (subst ([e_1 x_1] ...) e))
        β)))

;; HC add tests:

(module+ test
  (test--> -->β (term  0                 )     ) ; expect nothing
  (test--> -->β (term  (lambda (x) x)    )     ) ; expect nothing
  (test--> -->β (term ((lambda (x) x) 3) )    3) ; β reduction
  (define id-id-of-3 (term ((lambda (x) x)
                            ((lambda (y) y)
                             3))))
  ;; with α equivalence there is just one result
  (test--> -->β #:equiv =α/racket
           id-id-of-3
           (term ((lambda (x) x) 3)))
  ;; withOUT α equivalence there are two results
  (test--> -->β
           id-id-of-3
           (term ((lambda (x) x) 3))
           (term ((lambda (y) y) 3)))
  ;; do all redexes
  (test-->> -->β
           id-id-of-3
           3)
  (define has-β-η (term ((lambda (a b c)
                           (lambda (x y) (z x y)))
                         1 2 3)))
  (test--> -->β #:equiv =α/racket
           has-β-η
           (term (lambda (x y) (z x y)))) ; does β
  (test-->> -->β #:equiv =α/racket
           has-β-η
           (term (lambda (x y) (z x y)))) ; ONLY does β
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.
;; Develop a βη reduction relation for Lambda-η.

(module+ test
  ;; see if new reduction handles examples from above
  (test-->> -->βη
           id-id-of-3
           3)
  ; a possible η reduction
  (test--> -->β
           (term (lambda (a b c) (z a b c)))
                   ) ; nothing, does NOT reduce using -->β
  (test--> -->βη
           (term (lambda (a b c) (z a b c)))
           (term z)) ;          DOES     reduce using -->βη
  )

(define -->βη
  (extend-reduction-relation
   -->β
   Lambda-η
   (--> (in-hole C (lambda (x ..._n) (e e_1 ..._n)))
        (in-hole C e)
        η)))

;; --------------------------------------------------
;; Find a term that contains both a β and an η redex.
;; Formulate a Redex test that validates this claim.
;; Also use trace to graphically validate the claim.

(module+ test
  (test--> -->βη #:equiv =α/racket
            has-β-η
            (term ((lambda (a b c) z) 1 2 3)) ; η reduction
            (term (lambda (x y) (z x y))))    ; β reduction
  (test--> -->βη
            (term (lambda (x y) (z x y)))     ; β reduction result from above
            (term z))
  ; do all redexes
  (test-->> -->βη
            has-β-η
            (term z))
;  (traces -->β  has-β-η)
;  (traces -->βη has-β-η)
  )

;;--------------------------------------------------
;; Develop the β and βη STANDARD REDUCTION RELATIONS.
;; Hint Look up extend-reduction-relation to save some work.

(define-extended-language Standard-η Lambda-η
  (E ::=
     hole
     (v ... E e ...)))

(module+ test
  (define t0
    (term
     ((lambda (a b) (a b))
      ((lambda (d) d) (lambda (e) e))
      ((lambda (f) f) 5))))
  (define t0-one-step
    (term
     ((lambda (a b) (a b))
      (lambda (e) e)
      ((lambda (f) f) 5))))
  ; yields only one term, leftmost-outermost
  (test--> s->vβ  #:equiv =α/racket t0 t0-one-step)
  (test--> s->vβη #:equiv =α/racket t0 t0-one-step)
  ; transitive closure drives it to final '5' value
  (test-->> s->vβ  t0 5)
  (test-->> s->vβη t0 5)
  (test-->> s->vβ  #:equiv =α/racket
           (term (lambda (a) (c a)))
           (term (lambda (a) (c a)))) ; no reduction
  (test-->> s->vβη #:equiv =α/racket
           (term (lambda (a) (c a)))
           (term c))                  ; η reduction
  (test--> s->vβη #:equiv =α/racket
           (term ((lambda (x) x) (lambda (a) (c a)))) ;; has β and η reductions possible
           (term (lambda (a) (c a)))  ; β reduction
           (term ((lambda (x) x) c))) ; η reduction
  )

(define s->vβ
  (reduction-relation
   Standard-η
   ;; by-value because 'v' as args
   (--> (in-hole E ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e)))))

(define s->vβη
  (extend-reduction-relation
   s->vβ
   Standard-η
   (--> (in-hole E (lambda (x ..._n) (e e_1 ..._n)))
        (in-hole E e))))

;; --------------------------------------------------
;; SEMANTICS
;; Use the standard reduction relations above to formulate a semantics for both variants.
;; The above test case, reformulated for the standard reduction, must fail.
;; Why?

;; Note The semantics for βη requires some experimentation.

;; Justify your non-standard definition of the run function.

(module+ test
  (test-equal (term (eval-value-η ,t0)) 5)
  (test-equal (term (eval-value-η ,t0-one-step)) 5)

  (define t1
    (term ((lambda (x) x) (lambda (x) x))))
  (test-equal (lambda-η? t1) #true)
  (test-equal (redex-match? Standard-η e t1) #true)
  (test-equal (term (eval-value-η ,t1)) 'closure)

  ;; has β and η reductions possible
  #;
  (test-equal (term (eval-value-η
                     (term ((lambda (x) x) (lambda (a) (c a))))))
              1)
  #;
  (test-equal (term (eval-value-η
                     ((lambda (x) x) (lambda (a) (1 a)))))
              1)
  )

(define-metafunction Standard-η
  eval-value-η : e -> v or closure
  [(eval-value-η e) any_1 (where any_1 (run-value-η e))])

(define-metafunction Standard-η
  run-value-η : e -> v or closure
  [(run-value-η n) n]
  [(run-value-η v) closure]
  [(run-value-η e)
   (run-value-η e_again)
   ; (v) here means s->vβη is expected to be a function (i.e., return one result)
   (where (e_again) ,(begin (display (term e)) (newline)
                            (apply-reduction-relation s->vβη (term e))))]
  [(run-value-η e) stuck])

;; NOTE: there above 'where' fails because it returns multiple results (i.e., not a function):
#;
(apply-reduction-relation  s->vβη (term ((lambda (x) x) (lambda (a) (1 a)))))

;; --------------------------------------------------
;; The βη semantics is equivalent to the β variant.
;; Formulate this theorem as a metafunction. Use redex-check to test your theorem.

;; Note Why does it make no sense to add η to this system?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.
;; Extend the by-value language with an addition operator.
;; TODO
;; Equip both βv reduction system and βv standard reduction with rules
;; that assign addition the usual semantics.
;; Finally define a semantics functions for this language.
;; Hint Your rules need to escape to Racket and use its addition operator.

(module+ test
  (test-results))
