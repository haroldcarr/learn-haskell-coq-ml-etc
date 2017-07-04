#lang racket

(require redex)

;; 2 Long Tutorial

;; Develop LC as a model of a PL
;; 1. calculus: a simple logic for calculating with the terms of the language e == e’
;; 2. semantics: system for determining the value of a program

;; 2.2.1 Developing a Language

;; define-language pecifies syntax trees via tree grammars

(define-language Lambda1
  (e ::= x
         (lambda (x ...) e)
         (e e ...))
  (x ::= variable-not-otherwise-mentioned))

;; use the grammar to generate instances and check them

(define e1 (term y))
(define e2 (term (lambda (y) y)))
(define e3 (term (lambda (x y) y)))
(define e4 (term (,e2 ,e3)))

;; predicate that tests Lambda1 language membership
(define lambda1? (redex-match? Lambda1 e))

;; tests
(test-equal (lambda1? e1) #true)
(test-equal (lambda1? e2) #true)
(test-equal (lambda1? e3) #true)
(test-equal (lambda1? e4) #true)

(define eb1 (term (lambda (x x) y)))
(define eb2 (term (lambda (x y) 3)))

(test-equal (lambda1? eb1) #true)  ;; HC - tutorial says this should be #false
(test-equal (lambda1? eb2) #false)

;; (test-results)

;; 2.2.2 Developing Metafunctions : functions on the terms of a specific language.

;; do not allow param sequences with repeated variables.
;; unique-vars1 consumes a sequence of xs and produces a boolean.
(define-metafunction Lambda1
  unique-vars1 : x ... -> boolean
  [(unique-vars1) #true]                              ;; terminate recursion
  [(unique-vars1 x x_1 ... x x_2 ...) #false]
  [(unique-vars1 x x_1 ...) (unique-vars1 x_1 ...)])  ;; recursion

(module+ test
  (test-equal (term (unique-vars1 x y)) #true)
  (test-equal (term (unique-vars1 x y x)) #false))

;; Lambda1 allows repeated params.  Lambda2 does NOT.
(define-language Lambda2
  (e ::=
     x
     (lambda (x_!_ ...) e) ;; x_!_ ... means x must differ from all other elements of sequence
     (e e ...))
  (x ::= variable-not-otherwise-mentioned))

(define-metafunction Lambda2
  unique-vars2 : x ... -> boolean
  [(unique-vars2) #true]
  [(unique-vars2 x x_1 ... x x_2 ...) #false]
  [(unique-vars2 x x_1 ...) (unique-vars2 x_1 ...)])

(module+ test
  (test-equal (term (subtract (x y z x) x z)) (term (y))))

; (subtract (x ...) x_1 ...) removes x_1 ... from (x ...)
(define-metafunction Lambda2
  subtract : (x ...) x ... -> (x ...)
  [(subtract (x ...)) (x ...)]
  [(subtract (x ...) x_1 x_2 ...)
   (subtract (subtract1 (x ...) x_1) x_2 ...)])

; (subtract1 (x ...) x_1) removes x_1  from (x ...)
(module+ test
  (test-equal (term (subtract1 (x y z x) x)) (term (y z))))

(define-metafunction Lambda2
  subtract1 : (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x)
   (x_1 ... x_2new ...)
   (where (x_2new ...) (subtract1 (x_2 ...) x))
   (where #false (in x (x_1 ...)))]
  [(subtract1 (x ...) x_1) (x ...)])

(define-metafunction Lambda2
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #true]
  [(in x (x_1 ...)) #false])

;; 2.2.3 Developing a Language 2

;; Scope.
;; fv: free-variables function : specifies which constructs bind and which don’t
; (fv e) computes the sequence of free variables of e
; a variable occurrence of x is free in e
; if no (lambda (... x ...) ...) dominates its occurrence

(module+ test
  (test-equal (term (fv x)) (term (x)))
  (test-equal (term (fv (lambda2 (x) x))) (term ()))
  (test-equal (term (fv (lambda2 (x) (y z x)))) (term (y z))))
 
(define-metafunction Lambda2
  fv : e -> (x ...)
  [(fv x) (x)]
  [(fv (lambda2 (x ...) e))
   (subtract (x_e ...) x ...)
   (where (x_e ...) (fv e))]
  [(fv (e_f e_a ...))
   (x_f ... x_a ... ...)
   (where (x_f ...) (fv e_f))
   (where ((x_a ...) ...) ((fv e_a) ...))])

;; Specify an α equivalence relation:
;; eliminates variables from phrases
;; replaces them with arrows to their declarations
;; (i.e., "static-distance" aka de Bruijn index)

; (sd e) computes the static distance version of e
(define-extended-language SD Lambda2
  (e ::= .... (K n n)) ;; DIFF: Robby
  (n ::= natural))

(define sd1 (term (K 1 1))) ;; DIFF: Robby
;; (define sd2 (term 1))

(define SD? (redex-match? SD e))

(module+ test
  (test-equal (SD? sd1) #true))

;; add a means to the language to say “arrow back to the variable declaration.”
;; do not edit language definition : instead: extend definition
(define-metafunction SD
  sd : e -> e
  [(sd e_1) (sd/a e_1 ())])

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
  sd/a : e ((x ...) ...) -> e
  [(sd/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ; bound variable
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  [(sd/a (lambda (x ...) e_1) (e_rest ...))
   (lambda () (sd/a e_1 ((x ...) e_rest ...)))]
  [(sd/a (e_fun e_arg ...) (e_rib ...))
   ((sd/a e_fun (e_rib ...)) (sd/a e_arg (e_rib ...)) ...)]
  [(sd/a e_1 (e_rib ...))             ;; DIFF Robby
   ; a free variable is left alone
   e_1])

;; α equivalence (using above)
;; (=α e_1 e_2) determines whether e_1 and e_2 are α equivalent

(define-extended-language Lambda/n Lambda2
  (e ::= .... n)
  (n ::= natural))

(define in-Lambda/n? (redex-match? Lambda/n e))

(module+ test
  (test-equal (term (=α (lambda (x) x)
                        (lambda (y) y)))
              #true)
  (test-equal (term (=α (lambda (x) (x x))   ;; DIFF: HC
                        (lambda (y) (y y)))) ;; DIFF: HC
              #true)
  (test-equal (term (=α (lambda (x) x)
                        (lambda (y) z)))
              #false)
  )

(define-metafunction SD
  =α : e e -> boolean
  [(=α e_1 e_2) ,(equal? (term (sd e_1)) (term (sd e_2)))])

(define (=α/racket x y) (term (=α ,x ,y)))

;; 2.2.4 Extending a Language: any

;; extend Lambda2 with if and Booleans
(define-extended-language SD2 Lambda/n
  (e ::= ....
     true
     false
     (if e e e)))

(define SD2? (redex-match? SD2 e))

;; but (term (fv (lambda (x y) (if x y false)))) not covered
;; need metafunctions to be generic as possible

;; use 'any' for extensions that don’t add binders
(module+ test
  (test-equal (SD2? sd1) #true))

(define-metafunction SD2
  sd2 : any -> any
  [(sd2 any_1) (sd2/a any_1 ())])

(module+ test
  (test-equal (term (sd2/a x ())) (term x))
  (test-equal (term (sd2/a x ((y) (z) (x)))) (term (K 2 0)))
  (test-equal (term (sd2/a ((lambda (x) x) (lambda (y) y)) ()))
              (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
  (test-equal (term (sd2/a (lambda (x) (x (lambda (y) y))) ()))
              (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
  (test-equal (term (sd2/a (lambda (z x) (x (lambda (y) z))) ()))
              (term (lambda () ((K 0 1) (lambda () (K 1 0)))))))

(define-metafunction SD2
  sd2/a : any ((x ...) ...) -> any
  [(sd2/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ; bound variable
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  [(sd2/a (lambda (x ...) any_1) (any_rest ...))
   (lambda () (sd/a any_1 ((x ...) any_rest ...)))]
  [(sd2/a (any_fun any_arg ...) (any_rib ...))
   ((sd2/a any_fun (any_rib ...)) (sd2/a any_arg (any_rib ...)) ...)]
  [(sd2/a any_1 any)
   ; free variable, constant, etc
   any_1])

;; 2.2.5 Substitution

;; yntactic equivalent of function application

; (subst ([e x] ...) e_*) substitutes e ... for x ... in e_* (hygienically)
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
 
(define-metafunction SD
  subst : ((any x) ...) any -> any
  [(subst [(any_1 x_1) ... (any_x x) (any_2 x_2) ...] x) any_x]
  [(subst [(any_1 x_1) ...]  x) x]
  [(subst [(any_1 x_1) ...]  (lambda (x ...) any_body))
   (lambda (x_new ...)
     (subst ((any_1 x_1) ...)
            (subst-raw ((x_new x) ...) any_body)))
   (where  (x_new ...)  ,(variables-not-in (term any_body) (term (x ...))))]
  [(subst [(any_1 x_1) ...]  (any ...)) ((subst [(any_1 x_1) ...]  any) ...)]
  [(subst [(any_1 x_1) ...]  any_*) any_*])
 
(define-metafunction SD
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((x_n1 x_o1) ... (x_new x) (x_n2 x_o2) ...) x) x_new]
  [(subst-raw ((x_n1 x_o1) ...)  x) x]
  [(subst-raw ((x_n1 x_o1) ...)  (lambda (x ...) any))
   (lambda (x ...) (subst-raw ((x_n1 x_o1) ...)  any))]
  [(subst-raw [(any_1 x_1) ...]  (any ...))
   ((subst-raw [(any_1 x_1) ...]  any) ...)]
  [(subst-raw [(any_1 x_1) ...]  any_*) any_*])


(module+ test (test-results))
