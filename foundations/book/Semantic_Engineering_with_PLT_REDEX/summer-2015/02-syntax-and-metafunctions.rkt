#lang racket
(require redex)

#|
2 Syntax and Metafunctions

Goals
— Redex versus Racket
— define languages
— develop metafunctions, includes basic testing, submodules
— extend languages
— generalizing with any

2.1 Developing a Language

The define-language from specifies syntax trees via tree grammars:
|#

(define-language Lambda--
  (e ::= x
         (lambda (x ...) e)
         (e e ...))
  (x ::= variable-not-otherwise-mentioned))

#|
The trees are somewhat concrete, which makes it easy to work with
them, but it is confusing on those incredibly rare occasions when we
want truly abstract syntax.

We can include literal numbers (all of Racket’s numbers, including
complex) or integers (all of Racket’s integers) or naturals (all of
Racket’s natural numbers)—and many other things.

After you have a syntax, use the grammar to generate instances and
check them (typos do sneak in). Instances are generated with term:
|#

(define e1 (term y))
(define e2 (term (lambda (y) y)))
(define e3 (term (lambda (x y) y)))
(define e4 (term (,e2 ,e3)))

#|
Mouse over define. It is not a Redex form, it comes from Racket. Take
a close look at the last definition. Comma anyone?

Define yourself a predicate that tests membership:
|#

(define lambda--? (redex-match? Lambda-- e))

#|
Now you can formulate language tests:
|#

(module+ test
  (test-equal (lambda--? e1) #true)
  (test-equal (lambda--? e2) #true)
  (test-equal (lambda--? e3) #true)
  (test-equal (lambda--? e4) #true)

  (define eb1 (term (lambda (x x) y)))
  (define eb2 (term (lambda (x y) 3)))

  ;; should fail, but nothing in grammar prevents repeated parameter names.
  (test-equal (lambda--? eb1) #true)
  ;; fails as expected, because integer in body is not part of grammar.
  (test-equal (lambda--? eb2) #false)
)

#|
Make sure your language contains the terms that you want and does not
contain those you want to exclude. Why should eb1 and eb2 not be in
Lambda’s set of expressions?

2.2 Developing Metafunctions

To make basic statements about (parts of) your language, define
metafunctions. Roughly, a metafunction is a function on the terms of a
specific language.

We don’t want parameter sequences with repeated variables. Can we say this with a metafunction.

(define-metafunction Lambda
  unique-vars : x ... -> boolean)

The second line is a Redex contract, not a type. It says unique-vars
consumes a sequence of xs and produces a boolean.

How do we way we don’t want repeated variables? With patterns.

(define-metafunction Lambda--
  unique-vars : x ... -> boolean
  [(unique-vars) #true]
  [(unique-vars x x_1 ... x x_2 ...) #false]
  [(unique-vars x x_1 ...) (unique-vars x_1 ...)])

Patterns are powerful. More later.

But, don’t just define metafunctions, develop them properly: state
what they are about, work through examples, write down the latter as
tests, then define the function.
|#
; are the identifiers in the given sequence unique?

(module+ test
  (test-equal (term (unique-vars x y)) #true)
  (test-equal (term (unique-vars x y x)) #false))

(define-metafunction Lambda--
  unique-vars : x ... -> boolean
  [(unique-vars) #true]
  [(unique-vars x x_1 ... x x_2 ...) #false]
  [(unique-vars x x_1 ...) (unique-vars x_1 ...)])

#|
Submodules delegate the tests to where they belong and they allow us
to document functions by example.
(module+ test
  (test-results))

Sadly, unique-vars doesn’t work for our language definition. They create circular definitions.

Fortunately, language definitions can employ more than Kleene patterns:
|#

(define-language Lambda
  (e ::=
     x
     (lambda (x_!_ ...) e)
     (e e ...))
  (x ::= variable-not-otherwise-mentioned))

(define lambda? (redex-match? Lambda e))

#|
x_!_ ... means x must differ from all other elements of this sequence

More metafunctions that use patterns in interesting ways:
|#

;; (subtract (x ...) x_1 ...) removes x_1 ... from (x ...)
(module+ test
  (test-equal (term (subtract (x y z x) x z)) (term (y))))

(define-metafunction Lambda
  subtract : (x ...) x ... -> (x ...)
  [(subtract (x ...)) (x ...)]
  [(subtract (x ...) x_1 x_2 ...)
   (subtract (subtract1 (x ...) x_1) x_2 ...)])

;; (subtract1 (x ...) x_1) removes x_1  from (x ...)
(module+ test
  (test-equal (term (subtract1 (x y z x) x)) (term (y z))))

(define-metafunction Lambda
  subtract1 : (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x)
   (x_1 ... x_2new ...)
   (where (x_2new ...) (subtract1 (x_2 ...) x))
   (where #false (in x (x_1 ...)))]
  [(subtract1 (x ...) x_1) (x ...)])

;; (in x x_1 ...) is x a member of (x_1 ...)?
(module+ test
  (test-equal (term (in x (y z x y z))) #true)
  (test-equal (term (in x ())) #false)
  (test-equal (term (in x (y z w))) #false))

(define-metafunction Lambda
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #true]
  [(in x (x_1 ...)) #false])

#|
2.3 Developing a Language 2

One of the first things a language designer ought to specify is
scope. People often do so with a free-variables function that
specifies which language constructs bind and which ones don’t:
|#

; (fv e) computes the sequence of free variables of e
; a variable occurrence of x is free in e
; if no (lambda (... x ...) ...) dominates its occurrence

(module+ test
  (test-equal (term (fv x)) (term (x)))
  (test-equal (term (fv (lambda (x) x))) (term ()))
  (test-equal (term (fv (lambda (x) (y z x)))) (term (y z))))

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

#|
You may know it as the de Bruijn index representation.

The best approach is to specify an α equivalence relation, that is,
the relation that virtually eliminates variables from phrases and
replaces them with arrows to their declarations. In the world of
lambda calculus-based languages, this transformation is often a part
of the compiler, the so-called static-distance phase.

The function is a good example of accumulator-functions in Redex:
|#

; (sd e) computes the static distance version of e

(define-extended-language SD Lambda
  (e ::= .... (K n n)) ;; DIFF (K n)
  (n ::= natural))

(define sd1 (term (K 1 1))) ;; DIFF (K 1)
(define sd2 (term 1))

(define SD? (redex-match? SD e))

(module+ test
  (test-equal (SD? sd1) #true)
  (test-equal (SD? sd2) #false)
  )

#|
We have to add a means to the language to say “arrow back to the
variable declaration.” We do not edit the language definition but
extend the language definition instead.
|#

(define-metafunction SD
  sd/e : e -> e
  [(sd/e e_1) (sd/ae e_1 ())])

(module+ test
  (test-equal (term (sd/e x))
              (term x))
  (test-equal (term (sd/ae x ()))
              (term x))
  (test-equal (term (sd/ae x ((y) (z) (x))))
              (term (K 2 0)))
  (test-equal (term (sd/ae ((lambda (x) x) (lambda (y) y)) ()))
              (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
  (test-equal (term (sd/ae (lambda (x) (x (lambda (y) y))) ()))
              (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
  (test-equal (term (sd/ae (lambda (z x) (x (lambda (y) z))) ()))
              (term (lambda () ((K 0 1) (lambda () (K 1 0)))))))

(define-metafunction SD
  sd/ae : e ((x ...) ...) -> e
  [(sd/ae x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ; bound variable
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  [(sd/ae (lambda (x ...) e_1) (e_rest ...))
   (lambda () (sd/ae e_1 ((x ...) e_rest ...)))]
  [(sd/ae (e_fun e_arg ...) (e_rib ...))
   ((sd/ae e_fun (e_rib ...)) (sd/ae e_arg (e_rib ...)) ...)]
  [(sd/ae e_1 (e_rib ...)) ;; DIFF e_1 e
   ; a free variable is left alone
   e_1])

#|
Now α equivalence is straightforward:
|#

; (=α e_1 e_2) determines whether e_1 and e_2 are α equivalent

(define-extended-language Lambda/n Lambda
  (e ::= .... n)
  (n ::= natural))

(define in-Lambda/n? (redex-match? Lambda/n e))

(module+ test
  (test-equal (term (=α (lambda (x) x) (lambda (y) y)))           #true)
  (test-equal (term (=α (lambda (x) (x 1)) (lambda (y) (y 1))))   #true)
  (test-equal (term (=α (lambda (x) x) (lambda (y) z)))           #false))

(define-metafunction Lambda/n
  =α : e e -> boolean
  [(=α e_1 e_2) ,(equal? (term (sd e_1)) (term (sd e_2)))])

(define (=α/racket x y) (term (=α ,x ,y)))

#|
2.4 Extending a Language: any

Suppose we wish to extend Lambda with if and Booleans, like this:

(define-extended-language SD Lambda
  (e ::= ....
     true
     false
     (if e e e)))

Guess what?   (term (fv (lambda (x y) (if x y false))))
doesn’t work because false and if are not covered.

We want metafunctions that are as generic as possible for computing such notions
as free variable sequences, static distance, and alpha equivalences.

Redex contracts come with any and Redex patterns really are over Racket’s S-expressions.
This definition now works for extensions that don’t add binders:
|#

(module+ test
  (test-equal (SD? sd1) #true))

(define-metafunction SD
  sd : any -> any
  [(sd any_1) (sd/a any_1 ())])

(module+ test
  (test-equal (term (sd x))
              (term x))
  (test-equal (term (sd/a x ()))
              (term x))
  (test-equal (term (sd/a x ((y) (z) (x))))
              (term (K 2 0)))
  (test-equal (term (sd/a ((lambda (x) x) (lambda (y) y)) ()))
              (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
  (test-equal (term (sd/a (lambda (x) (x (lambda (y) y))) ()))
              (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
  (test-equal (term (sd/a (lambda (z x) (x (lambda (y) z))) ()))
              (term (lambda () ((K 0 1) (lambda () (K 1 0)))))))

(define-metafunction SD
  sd/a : any ((x ...) ...) -> any
  [(sd/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ; bound variable
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...))))
   (where n_pos ,(length (term (x_0 ...))))
   (where #false (in x (x_1 ... ...)))]
  [(sd/a (lambda (x ...) any_1) (any_rest ...))
   (lambda () (sd/a any_1 ((x ...) any_rest ...)))]
  [(sd/a (any_fun any_arg ...) (any_rib ...))
   ((sd/a any_fun (any_rib ...)) (sd/a any_arg (any_rib ...)) ...)]
  [(sd/a any_1 any)
   ; free variable, constant, etc
   any_1])

#|
2.5 Substitution

The last thing we need is substitution, because it is the syntactic
equivalent of function application. We define it with any having
future extensions in mind.
|#

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

(define-metafunction Lambda/n
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

(define-metafunction Lambda/n
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((x_n1 x_o1) ... (x_new x) (x_n2 x_o2) ...) x) x_new]
  [(subst-raw ((x_n1 x_o1) ...)  x) x]
  [(subst-raw ((x_n1 x_o1) ...)  (lambda (x ...) any))
   (lambda (x ...) (subst-raw ((x_n1 x_o1) ...)  any))]
  [(subst-raw [(any_1 x_1) ...]  (any ...))
   ((subst-raw [(any_1 x_1) ...]  any) ...)]
  [(subst-raw [(any_1 x_1) ...]  any_*) any_*])

(module+ test
  (test-results))
