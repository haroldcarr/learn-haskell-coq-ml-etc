#lang racket
(require redex)
(require "common.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "borrowed" from the lecture

; (subtract (x ...) x_1 ...) removes x_1 ... from (x ...)
     
(module+ test
  (test-equal (term (subtract (x y z x) x z)) (term (y))))

(define-metafunction Lambda
  subtract : (x ...) x ... -> (x ...)
  [(subtract (x ...)) (x ...)]
  [(subtract (x ...) x_1 x_2 ...)
   (subtract (subtract1 (x ...) x_1) x_2 ...)])
     
; (subtract1 (x ...) x_1) removes x_1  from (x ...)
(module+ test
  (test-equal (term (subtract1 (x y z x) x)) (term (y z))))
     
(define-metafunction Lambda
  subtract1 : (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x)
   (x_1 ... x_2new ...)
   (where (x_2new ...) (subtract1 (x_2 ...) x))
   (where #false (in x (x_1 ...)))]
  [(subtract1 (x ...) x_1) (x ...)])

; (fv e) computes the sequence of free variables of e
; a variable occurrence of x is free in e 
; if no (lambda (... x ...) ...) dominates its occurrence 
 
(define-metafunction Lambda
  fv : e -> (x ...)
  [(fv x) (x)]
  [(fv (lambda (x ...) e))
   (subtract (x_e ...) x ...)
   (where (x_e ...) (fv e))]
  [(fv (e_f e_a ...))
   (x_f ... x_a ... ...)
   (where (x_f ...) (fv e_f))
   (where ((x_a ...) ...) ((fv e_a) ...))])

(module+ test
  (test-equal (term (fv x)) (term (x)))
  (test-equal (term (fv (lambda (x) x))) (term ()))
  (test-equal (term (fv (lambda (x) (y z x)))) (term (y z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A variable x is bound in e_Lambda if x occurs in a lambda-parameter list in e_Lambda

#|
(define-metafunction Lambda
  bv : x e -> boolean
  [(bv _ x) #f]
  [(bv _ (e e_1 ...)) #f]
  [(bv x (lambda (x_1 ... x x_2 ...) _)) #t]
  [(bv _ _) #f]
  )

(module+ test
  (test-equal (term (bv x (lambda (x) x)))
              #t)
  (test-equal (term (bv y (lambda (x) x)))
              #f)
  (test-equal (term (bv x (lambda () (lambda (x) x))))
              #f)
  (test-equal (term (bv y (lambda (y x) y)))
              #t)
  (test-equal (term (bv y (lambda (x y) y)))
              #t)
  (test-equal (term (bv x (lambda (y z) x)))
              #f)
)
|#

; (add (x ...) x_1 ...) adds x_1 ... to (x ...)
     
(module+ test
  (test-equal (term (add (y z) w x)) (term (x w y z)))) ;; NOTE: order of result

(define-metafunction Lambda
  add : (x ...) x ... -> (x ...)
  [(add (x ...)) (x ...)]
  [(add (x ...) x_1 x_2 ...)
   (add (add1 (x ...) x_1) x_2 ...)])
     
; (add (x ...) x_1) adds x_1 to (x ...)
(module+ test
  (test-equal (term (add1 (x y z) w)) (term (w x y z))))
     
(define-metafunction Lambda
  add1 : (x ...) x -> (x ...)
  [(add1 (x_1 ... x x_2 ...) x)
   (x_1 ... x x_2 ...)
   (where #false (in x (x_1 ...)))
   (where #false (in x (x_2 ...)))]
  [(add1 (x ...) x_1) (x_1 x ...)])

; (fv e) computes the sequence of free variables of e
; a variable occurrence of x is free in e 
; if no (lambda (... x ...) ...) dominates its occurrence 

;;; UNFINISHED!!!
(define-metafunction Lambda
  bv : e -> (x ...)
  [(bv x) ()]
  [(bv (lambda (x ...) e))
   (add (x_e ...) x ...)
   (where (x_e ...) (bv e))]
  [(bv (e_f e_a ...))
   (x_f ... x_a ... ...)
   (where (x_f ...) (bv e_f))
   (where ((x_a ...) ...) ((bv e_a) ...))])

(module+ test
  (test-equal (term (bv x)) (term ()))
  (test-equal (term (bv (lambda (x) x))) (term (x)))
  (test-equal (term (bv (lambda (x) (y z x)))) (term (x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-extended-language Env Lambda
  (e ::= .... natural)
  (env ::= ((x e) ...))
  (e-or-f ::= e #f))

(define env? (redex-match? Env env))

(define-metafunction Env
  lookup : x env -> e-or-f
  [(lookup x ((x_1 e_1) ... (x e) (x_2 e_2) ...)) e]
  [(lookup _ _) #f]
  )

(module+ test
  (test-equal (env? (term ())) #t)
  (test-equal (env? (term ((a b)))) #t)
  (test-equal (env? (term a)) #f)
  (test-equal (env? (term ((a)))) #f)
  (test-equal (env? (term ((a b) c))) #f)
  )

(module+ test
  (test-equal (term (lookup x ())) #f)
  (test-equal (term (lookup x ((x 1)))) 1)
  (test-equal (term (lookup x ((y 1)))) #f)
  )

(define (environment? e) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define-metafunction Env
  let : env e -> e
  [(let env e)
   (subst ,(map reverse (term env)) e)
   ]
  )

(module+ test
  (test-equal (term (let () 1)) (term 1))
  (test-equal
   (term (let ((x (lambda (a b c) a))
               (y (lambda (x) x)))
           (x y y y)))
   (term
    ((lambda (x y) (x y y y))
     (lambda (a b c) a)
     (lambda (x) x))))
  )
|#

(define-metafunction Lambda
  let : ((x e) ...) e -> e
  [(let ((x e) ...) e_body)
   ((lambda (x ...) e_body) e ...)
   ]
  )

(module+ test
  (test-equal (term (let () (lambda (x) x))) (term ((lambda () (lambda (x) x)))))
  (test-equal
   (term (let ((x (lambda (a b c) a))
               (y (lambda (x) x)))
           (x y y y)))
   (term
    ((lambda (x y) (x y y y))
     (lambda (a b c) a)
     (lambda (x) x))))
  (test-equal (term
               (fv
                (let ((x (lambda (a b c) a))
                      (y (lambda (x) x)))
                  (x y y y))))
              '())
  (test-equal (term
               (bv
                (let ((x (lambda (a b c) a))
                      (y (lambda (x) x)))
                  (x y y y))))
              '(y x c b a x))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (test-results))
