#lang racket
(require "common.rkt")
(require redex)

;;
;; 5 Lab Designing Reductions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4.
;; Develop a βη reduction relation for Lambda-η.

(define-extended-language Lambda-η Lambda
  (e ::= .... n)
  (n ::= natural)
  (C ::=
     hole
     (e ... C e ...)
     (lambda (x_!_ ...) C))
  (v ::=
     n
     (lambda (x ...) e)))

(define -->β
  (reduction-relation
   Lambda-η
   (--> (in-hole C ((lambda (x_1 ..._n) e) e_1 ..._n))
        (in-hole C (subst ([e_1 x_1] ...) e))
        β)))

;; Exercise 4.

(define -->β-η
  (extend-reduction-relation
   -->β
   Lambda-η
   (--> (in-hole C (lambda (x ..._n) (e e_1 ..._n)))
        (in-hole C e))))

(define lambda-η? (redex-match? Lambda-η e))

(module+ test
  (test--> -->β-η #:equiv =α/racket (term ((lambda (x) x) 3)) 3)
  (test--> -->β-η #:equiv =α/racket
           (term ((lambda (x) x)
                  ((lambda (x) x) 3)))
           (term ((lambda (x) x) 3)))
  (test--> -->β-η #:equiv =α/racket
           (term ((lambda (a) (c a)) 1))
           (term (c 1)))
  ;; this fails without handling η correctly
  (test--> -->β-η #:equiv =α/racket
           (term (lambda (a) (c a)))
           (term c))
  )

;;; Semantics

(define-extended-language Standard-η Lambda-η
  (v ::= n (lambda (x ...) e))
  (E ::=
     hole
     (v ... E e ...)))

(module+ test
  (define t0
    (term
     ((lambda (x y) (x y))
      ((lambda (x) x) (lambda (x) x))
      ((lambda (x) x) 5))))
  (define t0-one-step
    (term
     ((lambda (x y) (x y))
      (lambda (x) x)
      ((lambda (x) x) 5))))

  (test--> s->βv-η t0 t0-one-step)
  ; but the transitive closure drives it to 5
  (test-->> s->βv-η t0 5)
  (test-->> s->βv-η #:equiv =α/racket
           (term (lambda (a) (c a)))
           (term c))
  (test--> s->βv-η #:equiv =α/racket
           (term ((lambda (a) (c a)) 1))
           (term (c 1)))
  (test--> s->βv-η #:equiv =α/racket
           (term ((lambda (x) x) (lambda (a) (c a)))) ;; has β and η reductions possible
           (term (lambda (a) (c a)))
           (term ((lambda (x) x) c)))
  )

(define s->βv-η
  (reduction-relation
   Standard-η
   (--> (in-hole E (lambda (x ..._n) (e e_1 ..._n)))
        (in-hole E e))
   (--> (in-hole E ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e)))))

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
   ; (v) here means s->βv-η is expected to be a function (i.e., return one result)
   (where (e_again) ,(begin (display (term e)) (newline)
                            (apply-reduction-relation s->βv-η (term e))))]
  [(run-value-η e) stuck])

;; NOTE: there above 'where' fails because it returns multiple results (i.e., not a function):
#;
(apply-reduction-relation  s->βv-η (term ((lambda (x) x) (lambda (a) (1 a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5.
;; Extend the by-value language with an addition operator.
;; TODO
