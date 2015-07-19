;;
;; Created       : 2015 Jan 02 (Fri) 16:15:07 by Harold Carr.
;; Last Modified : 2015 Jul 11 (Sat) 16:21:12 by Harold Carr.
;;

#lang racket
(require redex)

;;; p 207 Exercise 11.1
(and
 (equal?      `(+ ,(first      `(, (+ 12 34)))  5)
                                  '(+ 46        5))
 (equal? (term (+ ,(first (term (, (+ 12 34)))) 5))
                                  '(+ 46        5)))


;;; p 207 Specifying Abstract Syntax

(define-language bool-any-lang
  [B true
     false
     (v B B)]
  [C (v C B)
     (v B C)
     hole])

;;; p 7 Exercise 1.1

(redex-match bool-any-lang
             B
             (term true))

(redex-match bool-any-lang
             B
             (term v))

(redex-match bool-any-lang
             B
             (term (v (v false true) (v false false))))

(redex-match bool-any-lang
             B
             (term (v (false) (true))))

(redex-match bool-any-lang
             B
             (term "hello"))

;;; p 208

(define B1 (term true))
(define B2 (term false))
(define B3 (term (v true false)))
(define B4 (term (v ,B1 ,B2)))
(define B5 (term (v false ,B4)))

(define C1 (term hole))
(define C2 (term (v (v false false) hole)))
(define C3 (term (v hole true)))

;;; p 209

(redex-match bool-any-lang
             B
             (term (v false true)))

(redex-match bool-any-lang
             (in-hole C (v true B))
             (term      (v true (v true false))))

;;; p 210 Exercise 11.2

(define-language e-11-2
  [A 0
     1
     2
     (+ A A)]
  [C (+ C A)
     (+ A C)
     hole])

(redex-match e-11-2
             A
             (term (+ 1 2)))

(redex-match e-11-2
             (in-hole C (+ 1 A))
             (term      (+ 1 (+ 1 0))))

;;; p 210 Specifying Reduction Relations

(define bool-any-red
  (reduction-relation
   bool-any-lang
   (--> (in-hole C (v true B))
        (in-hole C true)
        v-true)
   (--> (in-hole C (v false B))
        (in-hole C B)
        v-false)))

;;; p 211

(redex-match bool-any-lang
             (in-hole C    (v true B))
             (term      (v (v true (v false true)) false)))

;;; p 211 Exercise 11.3

(define e-11-3
  (reduction-relation
   e-11-2
   (--> (in-hole C (+ 0 A))
        (in-hole C A)
        +_0_left)
   (--> (in-hole C (+ A 0))
        (in-hole C A)
        +_0_right)
   (--> (in-hole C (+ 1 1))
        (in-hole C 2)
        +_1)
   (--> (in-hole C (+ 1 2))
        (in-hole C 0)
        +_1_2)
   (--> (in-hole C (+ 2 1))
        (in-hole C 0)
        +_2_1)
   (--> (in-hole C (+ 2 2))
        (in-hole C 1)
        +_2_2)))

(redex-match e-11-2
             (in-hole C    (+ 0 A))
             (term      (+ (+ 0 (+ 1 0)) 2)))

(redex-match e-11-2
             (in-hole C    (+ 1 A))
             (term      (+ (+ 1 (+ 1 2)) 1)))

(redex-match e-11-2
             (in-hole C A)
             (term      (+ 1 2)))

;;; p 212 Tracing

(traces bool-any-red
        (term (v (v true false) (v true true))))

;;; p 213 Exercise 11.4

(traces e-11-3
        (term (+ 1 2)))

(traces e-11-3
        (term (+ 1 1)))

(traces e-11-3
        (term (+ (+ (+ 1 1) 2) 0)))

(traces e-11-3
        (term (+ (+ (+ 1 1) 2) (+ 1 2))))

;;; p 213 Variation on a Theme

;; p 214 holes only in leftmost-outermost position

(define-language bool-standard-lang
  [B true
     false
     (v B B)]
  [E (v E B)
     hole])

(define bool-standard-red
  (reduction-relation
   bool-standard-lang
   (--> (in-hole E (v true B))
        (in-hole E true)
        v-true)
   (--> (in-hole E (v false B))
        (in-hole E B)
        v-false)))

(traces bool-standard-red
        (term (v (v true false) (v true true))))

;; p 215 Excercise 11.5

; TODO

;; End of file.
