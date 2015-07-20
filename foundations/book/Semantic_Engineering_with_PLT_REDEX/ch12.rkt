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


