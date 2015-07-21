;;
;; Created       : 2015 Jul 18 (Sat) 19:17:46 by Harold Carr.
;; Last Modified : 2015 Jul 18 (Sat) 19:18:01 by Harold Carr.
;;

#lang racket
(require redex)

;;; p 217 12.1 More on Redex Grammars

(define-language iswim
  ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M))
  (o o1 o2)
  (o1 add1 sub1 iszero)
  (o2 + - * ^)
  (b number)
  ((V U W) b X (λ X M))
  (E hole (V E) (E M) (o V ... E M ...))
  ((X Y Z) variable-not-otherwise-mentioned))

;;; p 218

(redex-match iswim (in-hole E number) (term (+ 1 2)))

;; p 219 Exercise 12.1 TODO
;; p 219 Exercise 12.2 TODO

;;; p 219 12.2 Meta-functions

(define-metafunction iswim
  [(δ (iszero 0))  (λ x (λ y x))]
  [(δ (iszero b))  (λ x (λ y y))]
  [(δ (add1   b))  ,(add1 (term b))]
  [(δ (sub1   b))  ,(sub1 (term b))]
  [(δ (+ b_1 b_2)) ,(+    (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(-    (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(*    (term b_1) (term b_2))]
  [(δ (^ b_1 b_2)) ,(expt (term b_1) (term b_2))])

;; p 220

(equal? (term (δ (iszero 2)))   '(λ x (λ y y)))
(equal? (term (δ (+ 1 2)))      3)

;; p 221

(define-metafunction iswim
  ;; 1. X_1 bound, so do not continue in λ body
  [(subst (λ X_1 any_1) X_1 any_2)
   (λ X_1 any_2)]
  ;; 2. capture avoiding sub via fresh name
  [(subst (λ X_1 any_1) X_1 any_2)
   (λ X_3
     (subst (subst-var any_1 X_1 X_3) X_2 any_2))
   (where X_3 ,(variable-not-in (term (X_2 any_1 any_2))
                                (term X_1)))]
  ;; 3. replace X_1 with any_1
  [(subst X_1 X_1 any_1) any_1]
  ;; next 2: recur on tree structor of term
  [(subst (any_2 ...) X_1 any_1)
   ((subst any_2 X_1 any_1) ...)]
  [(subst any_2 X_1 any_1) any_2])

;; p 222

(equal? (term (subst (+ x (- y z)) z 5))
        '(+ x (- y 5)))

;; p 223

(define-metafunction iswim
  [(subst-var (any_1 ...) variable_1 variable_2)
   ((subst-var any_1 variable_1 variable_2) ...)]
  [(subst-var variable_1 variable_2) variable2]
  [(subst-var any_1 variable_1 variable_2) any_1])
  
(define iswim-red
  (reduction-relation
   iswim
   (--> (in-hole E ((λ X M) V))
        (in-hole E (subst M X V))
        beta-v)
   (--> (in-hole E (o b ...))
        (in-hole E (δ (o b ...)))
        δ)))

(redex-match
 iswim
 (in-hole E (λ X X))
 (term (λ X X)))

(redex-match
 iswim
 (in-hole E ((λ y (y y)) (λ x (x x))))
 (term ((λ y (y y)) (λ x (x x)))))

(traces iswim-red
        (term ((λ y (y y)) (λ x (x x)))))

(traces iswim-red
        (term (((λ x (λ y (+ x y))) 3) 4)))

;; p 225

(define iswim-standard
  (reduction-relation
   iswim
   (v ((λ X M) V) (subst M X V) beta-v)
   (v (o b ...) (δ (o b ...)) δ)
   with
   [(--> (in-hole E M) (in-hole E N)) (v M N)]))

(traces iswim-standard
        (term (((λ x (λ y (+ x y))) 3) 4)))
