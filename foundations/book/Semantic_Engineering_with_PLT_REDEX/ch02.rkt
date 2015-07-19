;;
;; Created       : 2015 Jul 11 (Sat) 20:10:58 by Harold Carr.
;; Last Modified : 2015 Jul 11 (Sat) 20:11:18 by Harold Carr.
;;

#lang scheme
(require redex)

;;; p 13 2.1 From Questions to Mathematical Claims

Does the evaluator always return the same result for a given input (i.e., is it a function)?

;;; p 14 2.2 Answers as Theorems

Theorem 2.1: 
forall B0, eval_r^=r B0 = R0 and eval_r^r=r B0 = R1 then R0 = R1

;;; p 15 Proof:

Assume eval_r^=r B0 = R0 and eval_r^r=r B0 = R1
Prove R1 = R2

assumptions imply B0 =r R1 and B0 =r R2 (by definition of =r)
so prove R1 =r R2

there must be common expression L such that

  R1 ->>r L  and  R2 ->>r L

But elements of R (i.e., 't' and 'f') are not reducible.
Therefore L = R1 and L = R2, so R1 = R2

QED

;;; p 16 Lemma 2.2 : Consistency for =r:

If M =r N then there exists an expr L such that M ->>r L and N ->> L

Proof by induction on structure of derivation of M =r N (i.e., the structure of its proof tree):

Base case:
- M ->>r N : let L = N and the claim holds

Inductive cases:
- M =r N because N =r M
  By induction, an L exists for N =r M, the L we want.
- M =r N because M =r L0 and L0 =r N
  By induction, there exists an L1 such that M ->>r L1 and L0 ->>r L1.
  By induction, there exists an L2 such that N ->>r L2 and L0 ->>r L2.
  Suppose there exists some expression L2 such that L1 ->>r L3 and L2 ->>r L3.
  Then claim holds.
QED

;;; p 17

DIAMOND PROPERTY

                L
              /   \
             /     \
            v       v
            M       N
             \     /
              \   /
                v
                L'

When the transitive, reflexive, compatible closure of a reduction satisfies the diamond
property, it is call Church-Rosser

Lemma 2.3 [Church-Rosser (->>r)]:
If L ->>r M and L ->>r N,
there exists an expression L'
such that M ->>r L' and N ->> L'.

Although the diamond property does not quite hold for ->r, a sufficiently strong property holds.

;;; p 18

Lemma 2.4 [Diamon-like (->r)]
If L ->r M and L ->r N for M /= N, then either
- M ->r N
- N ->r M
- there exists an L' such that M ->r L' and N ->r L'
(i.e., a reduction branch of expressions can be completed to a diamond,
or there is a "triangle" style of completion).

Proof:



;;  p x Exercise 2.1

;; End of file.
