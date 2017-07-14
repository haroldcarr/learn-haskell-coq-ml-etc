#lang racket
(require redex)

;; 1.1 Defining a Language

(define-language L
  (e (e e)       ;; application
     (λ (x t) e) ;; abstraction
     x           ;; variable
     (amb e ...) ;; ??
     number      ;; number
     (+ e ...)   ;; addition
     (if0 e e e) ;; conditional
     (fix e)     ;; recursion
     )
  (t (→ t t) num)
  (x variable-not-otherwise-mentioned))

(redex-match
 L
 e
 (term (λ (x) x)))

(redex-match
 L
 e
 (term ((λ (x num) (amb x 1))
        (+ 1 2))))

(redex-match
 L
 (e_1 e_2)
 (term ((λ (x num) (amb x 1))
        (+ 1 2))))

(redex-match
 L
 (e_1 ... e_2 e_3 ...)
 (term ((+ 1 2)
        (+ 3 4)
        (+ 5 6))))

;; Exercise 1
(redex-match
 L
 ((λ (x num) e) 17)
 (term ((λ (x num) (+ x 1))
        17)))

;; Exercise 2
(redex-match
 L
 (→ num t)
 (term (→ num (→ num num))))

(redex-match
 L
 (→ num (→ num t))
 (term (→ num (→ num num))))

;; Exercise 3
(redex-match
 L
 (e_1 e_2 ...)      ;; HC: incorrect
 (term (1 2 3 4)))

;; Exercise 4
;; TODO

;; 1.2 Typing

(define-extended-language L+Γ L
  [Γ · (x : t Γ)])

(define-judgment-form
  L+Γ
  ;; #:mode tells Redex how to compute derivations.
  ;; Here indicates that Γ and e are inputs, and the type position is an output.
  #:mode (types I I O)
  #:contract (types Γ e t)

  ;; application
  [(types Γ e_1 (→ t_2 t_3)) ;; if e_1 has the type (→ t_2 t_3)
   (types Γ e_2 t_2)         ;; and e_2 has the type t_2
   -------------------------
   (types Γ (e_1 e_2) t_3)]  ;; then the application expression has type t_3

  ;; abstraction
  [(types (x : t_1 Γ) e t_2)
   -----------------------------------
   (types Γ (λ (x t_1) e) (→ t_1 t_2))]

  [(types Γ e (→ (→ t_1 t_2) (→ t_1 t_2)))
   ---------------------------------------
   (types Γ (fix e) (→ t_1 t_2))]

  ;; variable
  [---------------------
   (types (x : t Γ) x t)]

  ;; if a variable type checks in some environment
  ;; then it also type checks in an extended environment
  ;; provided that the extension does not use the variable in question.
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : t_2 Γ) x_1 t_1)]

  ;; addition
  [(types Γ e num) ...
   -----------------------
   (types Γ (+ e ...) num)]

  [--------------------
   (types Γ number num)]

  ;; conditional
  [(types Γ e_1 num)
   (types Γ e_2 t)
   (types Γ e_3 t)
   -----------------------------
   (types Γ (if0 e_1 e_2 e_3) t)]

  [(types Γ e num) ...
   --------------------------
   (types Γ (amb e ...) num)])

(define-metafunction L+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])

;; 1.3 Testing Typing

(judgment-holds
   (types ·
          ((λ (x num) (amb x 1))
           (+ 1 2))
          t)
   t)

(judgment-holds
   (types ·
          (λ (f (→ num (→ num num))) (f (amb 1 2)))
          (→ t_1 t_2))
   t_2)

(test-equal
   (judgment-holds
    (types · (λ (x num) x) t)
    t)
   (list (term (→ num num))))

(test-equal
   (judgment-holds
    (types · (amb 1 2 3) t)
    t)
   (list (term num)))

;; this fails as expected
(test-equal
   (judgment-holds
    (types · (+ 1 2) t)
    t)
   (list (term (→ num num))))



