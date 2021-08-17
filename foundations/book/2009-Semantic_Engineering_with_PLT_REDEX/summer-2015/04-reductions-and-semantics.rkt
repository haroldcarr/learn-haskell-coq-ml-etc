#lang racket
(require redex)

#|
4 Reductions and Semantics

Goals
— extend languages with concepts needed for reduction relations
— developing reduction relations
— defining a semantics
— testing against a language

Note These notes deal with the λβ calculus, specifically its reduction system.

notation meaning
x        basic notion of reduction, without properties
-->x     one-step reduction, generated from x, compatible with syntactic constructions
-->>x    reduction, generated from -->x, transitive (here also reflexive)
=x       “calculus”, generated from -->x, symmetric, transitive, reflexive

4.1 Contexts, Values

The logical way of generating an equivalence (or reduction) relation
over terms uses through inductive inference rules that make the
relation compatible with all syntactic constructions.

An alternative and equivalent method is to introduce the notion of a
context and to use it to generate the reduction relation (or
equivalence) from the notion of reduction:
|#

(require "common.rkt")

(define-extended-language Lambda-calculus Lambda
  (e ::= .... n)              ; Lambda expressions plus naturals
  (n ::= natural)
  (v ::= (lambda (x ...) e))  ; values
  ; a context is an expression with one hole in lieu of a sub-expression
  (C ::=                      ; Context
     hole
     (e ... C e ...)          ; can appear in an application
     (lambda (x_!_ ...) C)))  ; can appear as the lambda body

(define Lambda/n? (redex-match? Lambda-calculus e))
(define Context? (redex-match? Lambda-calculus C))

(module+ test
  (define C1 (term ((lambda (u v)    u) hole 1)))
  (define C2 (term ((lambda (w x) hole)    0 2)))
  (define C3 (term (hole                   3 4)))
  (test-equal (Context? C1) #true)
  (test-equal (Context? C2) #true)
  (test-equal (Context? C3) #true)
  ; Filling the hole of context with an expression yields an expression:
  (define e1 (term (in-hole ,C1 1)))
  (define e2 (term (in-hole ,C2 29)))
  (define e3 (term (in-hole ,C3 (lambda (y z) z))))
;  (displayln e1)
;  (displayln e2)
  (displayln e3)

  (test-equal (Lambda/n? e1) #true)
  (test-equal (Lambda/n? e2) #true)
  (test-equal (Lambda/n? e3) #true)

  ; What does filling the hole of a context with a context yield?
  ; Answer: another context:
  (define e4 (term (in-hole ,C1 (lambda (a b) hole))))
;  (displayln e4)
  (test-equal (Context? e4) #true)
  (test-equal (Lambda/n? e4) #false)
  )

#|
4.2 Reduction Relations

Developing a reduction relation is like developing a function.
Work through examples first.
A reduction relation does not have to be a function,
meaning it may reduce one and the same term to distinct terms.
|#
; the λβ calculus, reductions only
(module+ test
  ; does the one-step reduction (i.e., test-->) reduce both β redexes?
  (test--> -->β
           #:equiv =α/racket
           (term ((lambda (o)
                    ((lambda (i) i)
                     o))
                  z))
           (term ((lambda (o) o) z))   ; reduce inner application
           (term ((lambda (i) i) z)))  ; reduce outer application

  (define ex (term ((lambda (x y) (x 1 y 2))
                    (lambda (a b c) a)
                    3)))
  ; single-step
  (test--> -->β
           #:equiv =α/racket
           ex
           (term ((lambda (a b c) a) 1 3 2)))
  ; reduce all redexes (i.e., test-->>)
  ; (can check that full reduction relation reduces all redexes)
  (test-->> -->β
            ex
            1))

;; call-by-name
(define -->β
  (reduction-relation
   Lambda-calculus
   ;; if the hole in the expression contains an application
   (--> (in-hole C ((lambda (x_1 ..._n) e_body) e_1 ..._n))
        ;; then "apply" by replacing hole with body where parms replaced by args.
        (in-hole C (subst ([e_1 x_1] ...) e_body)))))

;; call-by-value calculus changes args to lambdas from 'e' to 'v'
(define -->βv
  (reduction-relation
   Lambda-calculus
   (--> (in-hole C ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole C (subst ([v_1 x_1] ...) e)))))

;; traces visualizes reduction graphs.
;; traces for same term but CBN viz CBV (fewer intermediate terms. Why? TODO):

(module+ test
  (define e (term ((lambda (x y)
                     ((lambda (f) (f (x 1 y 2)))
                      (lambda (w) 42)))
                   ((lambda (x) x) (lambda (a b c) a))
                   3)))
  #;
  (traces -->β  e)
  #;
  (traces -->βv e)
  )

#|
4.3 Semantics

First we need a standard reduction relation.
The key is to define the path to the leftmost-outermost redex (like above, via contexts).
Here are the relevant definitions for the by-value reduction relation
("Standard" means leftmost-outermost):
|#

(define-extended-language Standard Lambda-calculus
  (v ::= n (lambda (x ...) e)) ; values are naturals and lambdas
  (E ::=
     hole
     (v ... E e ...)))         ; application exprs evaluated to values left-to-right

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
  (test--> s->βv t0 t0-one-step)
  ; but the transitive closure drives it to 5
  (test-->> s->βv t0 5))

(define s->βv
  (reduction-relation
   Standard
   (--> (in-hole E ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e)))))

#|
Note the tests-first development of the relation.
Now we can define the semantics function (i.e., program to final value):
|#

(module+ test
  (test-equal (term (eval-value ,t0)) 5)
  (test-equal (term (eval-value ,t0-one-step)) 5)

  (define t1
    (term ((lambda (x) x) (lambda (y) y))))
  (test-equal (lambda? t1) #true)
  (test-equal (redex-match? Standard e t1) #true)
  (test-equal (term (eval-value ,t1)) 'closure))

(define-metafunction Standard
  ;; given an expression, return a 'v'alue or a closure
  eval-value : e -> v or closure
  [(eval-value e) any_1 (where any_1 (run-value e))])

#|
The key is the stepper-loop (i.e., run-value):
- it applies Racket fun apply-reduction-relation repeatedly until it yields a value.
|#

(define-metafunction Standard
  run-value : e -> v or closure
  [(run-value n) n]       ; natural: just return it
  [(run-value v) closure] ; value: natural ruled out above, so must be closure
  [(run-value e)          ; application, β reduce, then recurse on result
   (run-value e_again)
   ; (v) means that we expect s->βv to be a function 
   (where (e_again) ,(apply-reduction-relation s->βv (term e)))])

#|
4.4 What are Models

Good models of programming languages are like Newtonian models of how you drive a car.
As long as your speed is within a reasonable interval, the model accurately predicts
how your car behaves.

Similarly, as long as your terms are within a reasonable subset (the model’s language),
the evaluator of the model and the evaluator of the language ought to agree.

For Racket you set up an evaluator for the language like this (the details don’t matter):
|#

(define-namespace-anchor A)

(define N (namespace-anchor->namespace A))

; Lambda.e -> Number or 'closure or exn
(define (racket-evaluator t0)
  (define result
    (with-handlers ((exn:fail? values))
      (eval t0 N)))
  (cond
    [(number? result) result]
    [(procedure? result) (term closure)]
    [else (make-exn "hello world" (current-continuation-marks))]))

;; the theorem is ('theorem:' just a convention, not magic)
;; (formulated as a meta-function):
(define-metafunction Standard
  theorem:racket=eval-value : e -> boolean
  [(theorem:racket=eval-value e)
   ,(letrec ([rr (racket-evaluator (term e))]
             [vr (term (run-value e))])
      (cond
        [(and (exn? rr) (eq? (term stuck) vr)) #true]
        [(exn? rr)                             #false]
        [(eq? (term stuck) vr)                 #false]
        [else                                  (equal? vr rr)]))])

;; test it on some terms (TODO: failures):
#'
(module+ test
  (test-equal (term (racket=eval-value ,t0))          #true)
  (test-equal (term (racket=eval-value ,t0-one-step)) #true)
  (test-equal (term (racket=eval-value ,t1))          #true))

;; random testing (ala 'QuickCheck') TODO: failures):
#;
(redex-check Standard e (term (theorem:racket=eval-value e)))

(module+ test
  (test-results))
