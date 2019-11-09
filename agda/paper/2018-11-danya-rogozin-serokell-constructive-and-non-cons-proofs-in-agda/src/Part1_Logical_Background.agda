https://serokell.io/blog/logical-background

Company
Blog
Team
Hire us
Constructive and Non-Constructive Proofs in Agda (Part 1): Logical Background

Article by Danya Rogozin
Wednesday, November 14th, 2018
7 upvotes
Hi! I’m Danya Rogozin, and I work at Serokell on a blockchain framework called Snowdrop.

I would like to tell you about constructive and non-constructive proofs in a proof assistant and functional programming language called Agda. I’m currently working on a paper on a generalized model of data processing in a blockchain-system together with my teammates George Agapov and Kirill Briantsev. We use Agda to prove interesting properties of read-only state access computation, transaction validation, and block processing. Also, I’m writing a PhD thesis at Moscow State University on modalities in constructive and linear logic and their connection with computer science and mathematical linguistics.

We’ll split this article into several parts:

Logical background;
Brief introduction to Agda;
Examples.
The first two parts are needed to introduce the reader to the necessary background. In the first part, we will give some theoretical background in mathematical logic to know what formal proof and related concepts are, regardless of functional programming context. After that, we will talk about constructive and non-constructive proofs in mathematical logic and discuss a difference between them. Mathematical logic is the theoretical foundation of formal verification in dependently typed languages, so we need to discuss these basic technical and philosophical aspects to see the close connection between proof theory and dependently typed programming.

In the second part, we will introduce the reader to programming and theorem proving in Agda. We will compare the syntax and basic concepts of Agda and Haskell, and discuss the difference.

In the third part, we will prove some theorems in Agda in two ways: constructively and non-constructively. We will see the difference between constructive and non-constructive proofs in Agda through those examples.

As we will see later, Agda is a constructive formal system, i.e. we have no way to write non-constructive proofs without some special postulates. We will discuss what exactly we obtain if we make Agda formal system classical by adding non-constructive postulates.

Logical background
Classical logic
Classical logic is the oldest branch of mathematical logic that has its roots in Aristotle’s works [1]. Classical logic took its shape in G. Boole’s [2] and G. Frege’s [3] works in the second half of the 19th century. The main motivation is a necessity to define whether some statement is true or false regardless of its content. In other words, we would like to establish the truth of a given statement from its form. By form, we mean a result of forming complex statements from atomic statements via special parts of speech by abstracting from specific meanings that can vary from context to context.
In classical logic, atomic statements are propositional variables that can take a value from the two-element set 
{
f
a
l
s
e
,
t
r
u
e
}
 and special parts of speech are logical connectives: conjunction (a counterpart of “and”), disjunction (a counterpart of “or”), implication (a counterpart of “if … then …”), and negation (a counterpart of “not”).

Thus classical proof is a syntactical way to establish the truth of a proposition from a two-valued point of view. We propose this syntactical way by this way. We define axioms (or axiom schemas, in other words, not only primitive formulas but all their special cases too) and inference rules that allows obtaining new theorems from formulas that are already proved yet.
We use the single inference rule called Modus Ponens. Modus Ponens claims that if implication (
A
→
B
) and assumption (
A
) are true, then the conclusion (
B
) is true.

We define the language of a classical propositional calculus:

Definition 1
Let 
V
=
{
p
0
,
p
1
,
…
,
}
 be set of propositional variables. Thus:

Any propositional variable is a formula;
If 
A
 is a formula, then 
¬
A
 is a formula;
If 
A
,
B
 are formulas, then 
(
A
∨
B
)
, 
(
A
∧
B
)
 and 
(
A
→
B
)
 are formulas.
Definition 2 (Classical propositional calculus)
The classical propositional calculus (CPC) is defined by the following list of axiom schemes and inference rules:

(
A
→
(
B
→
C
)
)
→
(
(
A
→
B
)
→
(
A
→
C
)
)
;
A
→
(
B
→
A
)
;
A
→
(
B
→
(
A
∧
B
)
)
;
(
A
∧
B
)
→
A
;
(
A
∧
B
)
→
B
;
(
A
→
C
)
→
(
(
B
→
C
)
→
(
(
A
∨
B
)
→
C
)
)
;
A
→
(
A
∨
B
)
;
B
→
(
A
∨
B
)
;
(
A
→
B
)
→
(
(
A
→
¬
B
)
→
¬
A
)
;
¬
¬
A
→
A
Inference rule, Modus Ponens: 
A
A
→
B
B
.
Equivalently, we may define classical propositional logic as the smallest set 
L
 that consists of all special cases of these schemas and is closed under Modus Ponens rule, i.e. if 
A
∈
L
 and 
A
→
B
∈
L
, then 
B
∈
L
.

Definition 3 (Formal proof)
A (classical propositional) proof is a finite sequence of formulas, each of which is an axiom, or follows from the previous formulas by Modus Ponens rule.

Let us prove the formula 
A
→
A
 as an example:
(
1
)
(
A
→
(
(
A
→
A
)
→
A
)
)
→
(
(
(
A
→
(
A
→
A
)
)
→
(
A
→
A
)
)
Axiom schema
(
2
)
A
→
(
(
A
→
A
)
→
A
)
Axiom schema
(
3
)
(
(
A
→
(
A
→
A
)
)
→
(
A
→
A
)
1, 2, Modus Ponens
(
4
)
A
→
(
A
→
A
)
Axiom schema
(
5
)
A
→
A
1, 2, Modus Ponens
The law of excluded middle (in Latin, tertium non datur) is a law of classical logic initially formulated in Aristotle’s works [1]. This law claims that only one statement from 
A
 and 
¬
A
 is necessarily true and the second one is necessarily false. In other words, a statement may be either true or false, and there is no third option. Formally, 
A
∨
¬
A
. We leave as an exercise to prove the law of excluded middle in CPC.

The equivalent formulation of the law of excluded middle is a law of double negation elimination, which says that any statement is equivalent to its double negation. That is, if we know that it’s false that 
A
 is false, then 
A
 is true.

First-order logic
We have told above about classical propositional logic that has quite weak expressive possibilities. The language of classical propositional logic doesn’t include the structure of propositions, but the structure of a statement often plays a huge role in establishing the truth of this statement. For example,

“A sequence of real numbers 
x
1
,
x
2
,
…
 is a Cauchy sequence, if for all 
ε
>
0
 there exists a natural number 
N
, such that for all 
i
,
j
>
N
, 
|
x
i
−
x
j
|
<
ε
”.

We have the sentence “for all 
ε
>
0
 …”, but we have no way to establish whether this sentence is true or false only looking on connectives in this statement because we have to pay particular attention to the internal structure.

First-order logic (FOL) is a much richer formal system than (classical) propositional logic that allows expressing the internal structure of basic propositions in more detail.

The language of FOL extends the language of classical propositional logic. In addition to logical connectives, we have:

variables, the infinite set of letters 
x
,
y
,
z
,
…
;
constants, 
a
,
b
,
c
,
…
;
relation symbols, the set of letters 
P
,
Q
,
R
,
…
. Generally, we have an infinite collection of 
n
-ary relation symbols for every 
n
∈
N
;
function symbols, the set of letters 
f
,
g
,
h
,
…
. Similarly, we have an infinite collection of 
n
-ary function symbols for any natural number 
n
;
quantifiers: “for all” 
∀
, “there exists” 
∃
.
Variables range over some domain. Constants denote the special elements of the considered domain. Predicate symbols are symbols that represent relations. Function symbols are signs that denote operations. Note that any propositional variable is 
0
-ary relation symbol and any constant is 
0
-ary function symbol. In other words, we don’t need to define propositional variables and constants in the first-order language explicitly.

We build the first-order formulas as follows:

A first-order signature is a pair 
Ω
=
⟨
F
n
,
P
r
⟩
, where 
F
n
 is a set of function symbols and 
P
r
 is a set of relation symbols.

Definition 4 (Terms)

Any variable is a term;
Any constant is a term;
If 
x
1
,
…
,
x
n
 are terms and 
f
∈
F
n
 is a function symbol of valence 
n
, then 
f
(
x
1
,
…
,
x
n
)
 is a term.
Definition 5 (Formulas)

If 
x
1
,
…
,
x
n
 are terms and 
P
∈
P
r
 is a relation symbol of valence 
n
, then 
P
(
x
1
,
…
,
x
n
)
 is a formula;
If 
A
 is a formula, then 
¬
A
 is a formula;
If 
A
,
B
 are formulas, then 
(
A
α
B
)
 is a formula, where 
α
∈
→
,
∧
,
∨
.
If 
A
 is a formula and 
x
 is a variable, then 
∀
x
:
A
 (for all 
x
, 
A
 holds) and 
∃
x
:
A
 (exists 
x
, such that 
A
 holds) are formulas.
Let us write down the definition of a Cauchy sequence as the first-order formula:
∀
ε
:
∃
N
:
∀
i
:
∀
j
:
(
(
i
>
N
∧
j
>
N
)
→
(
|
x
i
−
x
j
|
<
ε
)
)

where 
>
 (“greater than”) is a binary relation symbol, 
−
 (“subtraction”) is a binary function symbol, 
|
|
 (“absolute value”) is an unary function symbol.

More briefly:
∀
ε
:
∃
N
:
∀
i
,
j
>
N
:
(
|
x
i
−
x
j
|
<
ε
)
.
where 
∀
i
,
j
>
N
 is a short form for 
∀
i
∀
j
(
(
i
>
N
∧
j
>
N
)
→
…
)
.

We define first-order logic axiomatically as first-order predicate calculus:

Definition 6 (Classical first-order predicate calculus)

CPC axioms;
∀
x
A
(
x
)
→
A
(
a
)
;
A
(
a
)
→
∃
x
A
(
x
)
;
Modus Ponens;
The first Bernays’ rule: 
A
→
B
A
→
∀
x
B
;
The second Bernays’ rule: 
A
→
B
∃
x
A
→
B
.
where 
A
, 
B
 are metavariables on formulas.
Here, a proof is a finite sequence of formulas, each of which is an axiom, or follows from the previous formulas by inference rules (Modus Ponens and Bernays’ rules).

Note that 
∃
x
A
 is equivalent to 
¬
(
∀
x
¬
A
)
 and 
∀
x
A
 is equivalent to 
¬
(
∃
x
¬
A
)
. Thus, quantifiers are mutually expressible in classical first-order logic.

Constructive logic
Constructive (or intuitionistic) mathematics is a field of mathematical logic that arose at the beginning of the 20th century. This direction was founded by Dutch mathematician L. E. J. Brouwer to provide an answer to the paradoxes of naive set theory such as Russell’s paradox [4]. Brouwer claimed that obtained paradoxes are the evidence of the fact that classical mathematics and its foundation are unsafe.

Brouwer and his followers expressed misgivings about ways of reasoning on mathematical objects and their introduction [5]. For instance, intuitionists widely discussed the nature of existence [6]. Mathematics is full of examples of so-called pure existence theorems, i.e. theorems claiming the existence of an object with some desired property but proved without any explicit presentation of this object. We consider the simplest example:

Theorem 1
There exist irrational numbers 
a
 and 
b
 such that 
a
b
 is a rational number.

Proof
Let us consider the number 
√
2
√
2
. If 
√
2
√
2
 is a rational, then 
a
=
b
=
√
2
. If 
√
2
√
2
 is an irrational number, let 
a
=
√
2
√
2
 and 
b
=
√
2
. Thus 
a
b
=
(
√
2
√
2
)
√
2
=
√
2
√
2
⋅
√
2
=
√
2
2
=
2
, which is a rational number.
□
Such reasoning is unacceptable from an intuitionistic point of view because we did not find those numbers. We just established two alternatives and had no reason to choose one of them. This proof is a proof of existence without any provision of clear evidence for the specific property. Such proofs are often based on the law of excluded middle (
A
∨
¬
A
) as in the example above. But this proof is classically valid since any statement is either true or false.

This critique has led to the rejection of the law of excluded middle. Moreover, logical connectives and quantifiers have become to be understood quite differently. A statement is proved if we have some explicit construction that solves some desired mathematical problem. Logical connectives are understood as follows. We will use Haskell notation:

A proof of (a, b) is an ordered pair (x, y), where x :: a and y :: b. In other words, if we need to prove both statements, then we need to prove each of them;
A proof of Either a b is either Left x or Right y, where x :: a and y :: b. If we are looking for proof of some disjunction, it means that we must prove at least one of members of this disjunction. ;
A proof of a -> b is a function f such that for all x :: a, f x :: b. A proof of this implication means that we have a general method that reduces any proof of b to the proof of a;
A proof ¬ a is a proof of a -> Void, where Void is an empty type.
Logically, Void denotes the absurdity, the statement that has no proof, for instance, 
0
=
1
. In other words, if we need to prove the negation of a, it means that we should show that any proof of a leads to the contradiction. For example, 
4
=
5
→
0
=
1
 is equivalent to 
¬
(
4
=
5
)
.

Type-theoretically, Void is a type of contradiction and has no values as far as the contradiction is not provable (if our system is consistent). Thus, a -> Void may be considered as a type of function with an empty range of values.

In Haskell, non-termination and exceptions inhabit all types including Void (e.g. loop = loop :: Void or exc = undefined :: Void), but we’ll be working in a subset of Haskell without these problematic constructs.

Such way of the interpretation of logical constants is called Brouwer-Heyting-Kolmogorov semantics (BHK-semantics) [7].

As you could see the proof in the example above is not valid within the context of BHK-semantics. Firstly, we did not propose any concrete irrational numbers 
a
 and 
b
 such that 
a
b
 is rational. Secondly, we have used the law of excluded middle 
A
∨
¬
A
. By the definition, a proof 
A
∨
¬
A
 is either proof of 
A
 or proof of 
¬
A
, but classically 
A
∨
¬
A
 is true without any proof of 
A
 or 
¬
A
 (it is easy to check that 
A
∨
¬
A
 is classical tautology via truth tables). The law of excluded middle was rejected by intuitionists for this reason.

We define intuitionistic propositional logic axiomatically as follows. Propositional language and formal proof are defined similarly as above:

Definition 7 (Intuitionistic propositional logic)

(
A
→
(
B
→
C
)
)
→
(
(
A
→
B
)
→
(
A
→
C
)
)
;
A
→
(
B
→
A
)
;
A
→
(
B
→
(
A
∧
B
)
)
;
(
A
∧
B
)
→
A
;
(
A
∧
B
)
→
B
;
(
A
→
C
)
→
(
(
B
→
C
)
→
(
(
A
∨
B
)
→
C
)
)
;
A
→
(
A
∨
B
)
;
B
→
(
A
∨
B
)
;
(
A
→
B
)
→
(
(
A
→
¬
B
)
→
¬
A
)
;
A
→
(
¬
A
→
B
)
.
Inference rule, Modus Ponens: 
A
A
→
B
B
.
In other words, we replaced the last axioms of classical propositional logic 
¬
¬
A
→
A
 to weaker axiom 
A
→
(
¬
A
→
B
)
 and obtained intuitionistic propositional logic.

Moreover, there is the following theorem:
Theorem 2 (Disjunction property, Gödel [1932], Gentzen [1934], Kleene [1945]) [8] [9] [10]

A
∨
B
 is provable intuitionistically if and only if either 
A
 is provable intuitionistically or 
B
 is provable intuitionistically.

By the way, the unprovability of the law of excluded middle may be considered to be the consequence of the disjunction property: 
A
∨
¬
A
 cannot be provable generally, because we have no possibility to establish the provability of this disjunction knowing nothing about 
A
. Note that the disjunction property doesn’t work in classical logic, where 
A
∨
¬
A
 is provable and true regardless of what 
A
 is.

Intuitionistic propositional logic may be extended to intuitionistic first-order logic as follows:

Definition 8 (Intuitionistic first-order predicate calculus)

IPC axioms
∀
x
A
(
x
)
→
A
(
a
)
;
A
(
a
)
→
∃
x
A
(
x
)
;
Modus Ponens;
Bernays’ rules.
Note that, quantifiers are not mutually expressible in contrast to classical first-order logic.

There is the following theorem about intuitionistic first-order logic which is wrong for classical first-order logic:

Theorem 3 (Existence property [9])

If 
∃
x
A
(
x
)
 is provable in intuitionistic first-order logic with signature 
Ω
. Then there exists some term 
t
, such that 
A
(
t
)
 is provable.

Existence property theorem is closely connected with the philosophical motivation of intuitionism, so far as we have told before that existence should be proved explicitly from an intuitionistic point of view.

See [11] to read more about philosophical distinctions between classical and intuitionistic logic in more detail.

Also, we note that the statement formulated in Theorem 1 has a constructive proof. Firstly, we propose some definitions and formulate the following theorem that solves Hilbert’s seventh problem:

Definition 9
An algerbaic number is a real number (generally, complex number) that is a root of some non-zero polynomial with rational coefficients.
Simple example: 
±
√
2
 are roots of polynomial 
f
(
x
)
=
x
2
−
2
, because 
f
(
√
2
)
=
f
(
−
√
2
)
=
0
.

Definition 10
A transcendental number is a real number 
a
 that is not a root of a polynomial with rational coefficients.
The standard examples of transcendental numbers are 
π
 and 
e
.

Theorem 4 (Gelfond–Schneider theorem [1934]) [12]
Let 
a
,
b
 be algebraic numbers such that 
a
≠
1
, 
a
≠
0
 and 
b
 is an irrational number. Then 
a
b
 is a transcendental number.

Thus we may easily prove the previous theorem without any non-constructive steps in the reasoning:

Theorem 5
There exist irrational numbers 
a
 and 
b
 such that 
a
b
 is a rational number.

Proof
By Gelfond-Schneider theorem, 
√
2
√
2
 is a transcendental number, since 
√
2
 is an algebraic number. Hence 
√
2
√
2
 is an irrational number.
But 
(
√
2
√
2
)
√
2
=
√
2
√
2
⋅
√
2
=
√
2
2
 is a rational number. Then we take 
a
=
√
2
√
2
 and 
b
=
√
2
.
□
Conclusion
In this post, we have made a brief introduction to the logical background to understand better the concepts that will be described in the next parts. We have seen the difference between constructive and non-constructive proofs considered mathematically, within in a context of mathematical logic.

In the next post, we will introduce the reader to Agda and compare its concepts and syntax with Haskell. Moreover, we will study theorem proving in Agda and understand the implementation of mathematical reasoning in dependently typed programming languages. If you want to stay updated, follow Serokell on Twitter and Facebook!

References
[1] Smith, R. (tr. & comm.). Aristotle’s Prior Analytics, Indianapolis: Hackett, 1989.
[2] Boole, G. An Investigation of the Laws of Thought. London: Walton & Maberly, 1854.
[3] Frege, G. Begriffsschrift, eine der arithmetischen nachgebildete Formelsprache des reinen Denkens. Halle, 1879.
[4] Russell, B. The principles of mathematics. London, 1903.
[5] Brouwer, L.E.J… Collected works I, A. Heyting (ed.). Amsterdam: North-Holland, 1975.
[6] Heyting, A. Intuitionism, an introduction. Amsterdam: North-Holland, 1956.
[7] Troelstra, A.S. Constructivism and Proof Theory. Illc, University of Amsterdam, 2003.
[8] Gödel, K. Zum intuitionistischen Aussagenkalkül, Anzeiger der Akademie der Wissenschaftischen in Wien, v. 69, 1932.
[9] Gentzen, G. Untersuchungen über das logische Schließen. I, Mathematische Zeitschrift v. 39 n. 2, 1934.
[10] Kleene S.C. On the interpretation of intuitionistic number theory, Journal of Symbolic Logic, vol. 10, 1945.
[11] Dummett, M. Elements of Intuitionism. Oxford University Press, 1977.
[12] Gelfond, A. Sur le septième Problème de Hilbert, Bulletin de l’Académie des Sciences de l’URSS. Classe des sciences mathématiques et na. VII (4), 1934.

agdafunctional programming language
© 2015–2019 Serokell OÜ




