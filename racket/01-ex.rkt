#lang racket

(require redex)

(define-language PCF
  (e ::=
     x
     (lambda (x) e)
     (e e)

     ; booleans
     tt
     ff
     (if e e e)

     ; arithmetic
     n
     (e + e)

     ;; strings
     s
     (s ++ s)

     (err s)

     )
  (n ::=
     integer)
  (s ::=
     string)

  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (lambda (x) e #:refers-to x))

(define-extended-language PCF-eval PCF
  (E-name ::=
     hole
     (E-name e)
     (if E-name e e)
     (E-name + e)
     (E-name ++ e)
     (v + E-name)
     (v ++ E-name)
     )
  (v ::=
     n
     s
     tt
     ff
     (lambda (x) e)))

(define-extended-language PCF-eval-v PCF
  (E-value ::=
     hole
     (E-value e)
     ;;((lambda (x) e) E-value)
     (v E-value)
     (if E-value e e)
     (E-value + e)
     (v + E-value)
     (E-value ++ e)
     (v ++ E-value)
     )
  (v ::=
     n
     s
     tt
     ff
     (lambda (x) e)))

(define ->name
  (reduction-relation
   PCF-eval
   #:domain e
   (--> (in-hole E-name ((lambda (x) e_1) e_2))
        (in-hole E-name (substitute e_1 x e_2))
        beta-name)
   (--> (in-hole E-name (s_1 e_2))
        (in-hole E-name (err "string in function position"))
        beta-name-bad)
   (--> (in-hole E-name (if tt e_1 e_2))
        (in-hole E-name e_1)
        if-tt)
   (--> (in-hole E-name (if ff e_1 e_2))
        (in-hole E-name e_2)
        if-ff)
   (--> (in-hole E-name (n_1 + n_2))
        (in-hole E-name ,(+ (term n_1) (term n_2)))
        plus)
   (--> (in-hole E-name (n_1 ++ n_2))
        (in-hole E-name ,(string-append (term n_1) (term n_2)))
        string-append)
  ))

(test-equal (apply-reduction-relation ->name (term ("foo" 3)))
            '((err "string in function position")))
(test-equal (apply-reduction-relation ->name (term ((lambda (x) x) 3)))
            '(3))
#;
(traces
 ->name
 (term ((lambda (x) x) 3)))
#;
(traces
 ->name
 (term (((lambda (x) x)
         (lambda (y) y))
        3)))

(define ->value
  (reduction-relation
   PCF-eval-v
   #:domain e
   (--> (in-hole E-value ((lambda (x) e_1) v_2))
        (in-hole E-value (substitute e_1 x v_2))
        beta-name)
   (--> (in-hole E-value (if tt e_1 e_2))
        (in-hole E-value e_1)
        if-tt)
   (--> (in-hole E-value (if ff e_1 e_2))
        (in-hole E-value e_2)
        if-ff)
   (--> (in-hole E-value (n_1 + n_2))
        (in-hole E-value ,(+ (term n_1) (term n_2)))
        plus)
   (--> (in-hole E-value (s_1 ++ s_2))
        (in-hole E-value ,(string-append (term s_1) (term s_2)))
        string-append)))
   
(test-equal (apply-reduction-relation ->value (term ((lambda (x) x) 3)))
            '(3))

#;
(traces
 ->value
 (term ((lambda (x) x) 3)))

(test-equal (apply-reduction-relation ->value (term ("hello" ++ "world")))
            '("helloworld"))

(test-equal (apply-reduction-relation ->name (term (3 + 4)))
            '(7))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-results)
