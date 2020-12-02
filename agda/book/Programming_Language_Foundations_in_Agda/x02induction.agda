module x02induction where

-- prove properties of inductive naturals and operations upon via induction


import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)

{-
------------------------------------------------------------------------------
## Properties of operators

* _Identity_.   left/right/both; sometimes called _unit_

* _Associativity_.   e.g., `(m + n) + p ≡ m + (n + p)`

* _Commutativity_.   e.g., `m + n ≡ n + m`

* _Distributivity_.  e.g., from the left  `(m + n) * p ≡ (m * p) + (n * p)`
                           from the right `m * (p + q) ≡ (m * p) + (m * q)`

#### Exercise `operators` (practice) {name=operators} TODO

Give another example of a pair of operators that have an identity
and are associative, commutative, and distribute over one another.
(You do not have to prove these properties.)

Give an example of an operator that has an identity and is
associative but is not commutative.
(You do not have to prove these properties.)

------------------------------------------------------------------------------
## Associativity

    (m + n) + p ≡ m + (n + p)
-}

_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ =
  begin
    (3 + 4) + 5
  ≡⟨⟩
    7 + 5
  ≡⟨⟩
    12
  ≡⟨⟩
    3 + 9
  ≡⟨⟩
    3 + (4 + 5)
  ∎

{-
useful to read chains like above
- from top down  until reaching simplest term (i.e., `12`)
- from bottom up until reaching the same term

Why should `7 + 5` be the same as `3 + 9`?
Perhaps gather more evidence, testing the proposition by choosing other numbers.
But infinite naturals so testing can never be complete.

## Proof by induction

def of naturals has
- _base case_
- _inductive case_

Proof by induction follows the structure of this definition.
Prove two cases
- _base case_ : show property holds for `zero`
- _inductive case_ :
  - assume property holds for an arbitrary natural `m` (the _inductive hypothesis_)
  - then show that the property holds for `suc m`

    ------
    P zero

    P m
    ---------
    P (suc m)

- initially, no properties are known.
- base case : `P zero` holds : add it to set of known properties
    -- On the first day, one property is known.
    P zero
- inductive case tells us that if `P m` holds
    -- On the second day, two properties are known.
    P zero
    P (suc zero)
  then `P (suc m)` also holds

    -- On the third day, three properties are known.
    P zero
    P (suc zero)
    P (suc (suc zero))

    -- On the fourth day, four properties are known.
    P zero
    P (suc zero)
    P (suc (suc zero))
    P (suc (suc (suc zero)))

The process continues: property `P n` first appears on day _n+1_.

## Our first proof: associativity

To prove associativity, take `P m` to be the property:

    (m + n) + p ≡ m + (n + p)

`n` and `p` are arbitrary natural numbers
show the equation holds for all `m` it will also hold for all `n` and `p`.

    -------------------------------
    (zero + n) + p ≡ zero + (n + p)

    (m + n) + p ≡ m + (n + p)
    ---------------------------------
    (suc m + n) + p ≡ suc m + (n + p)

If we can demonstrate both of these, then associativity of addition follows by induction.
-}

-- signature says providing evidence for proposition
+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
-- Evidence is a function that accepts three natural numbers, binds them to `m`, `n`, and `p`,
-- and returns evidence for the corresponding instance of the equation.

-- base case : show: (zero + n) + p ≡ zero + (n + p)
+-assoc zero n p =
  begin
    (zero + n) + p
  ≡⟨⟩
    n + p           -- simplifying both sides yields n + p ≡ n + p
  ≡⟨⟩
    zero + (n + p)
  ∎

-- inductive case : show: (suc m + n) + p ≡ suc m + (n + p)
+-assoc (suc m) n p =
  begin
    (suc m + n) + p
  ≡⟨⟩
    suc (m + n) + p
  ≡⟨⟩
    suc ((m + n) + p) -- simplifying both sides : suc ((m + n) + p) ≡ suc (m + (n + p))
                      -- this follows by prefacing `suc` to both sides of the induction hypothesis:
                      --                               (m + n) + p  ≡      m + (n + p)
  ≡⟨ cong suc (+-assoc m n p) ⟩
    suc (m + (n + p))
  ≡⟨⟩
    suc m + (n + p)
  ∎

{-
identifiers can have any characters NOT including spaces or the characters @.(){};_

The "middle" equation, does not follow from simplification alone: use chain reasoning, `_≡⟨_⟩_`
- justification for equation given in angle brackets:

    ⟨ cong suc (+-assoc m n p) ⟩

Recursive invocation `+-assoc m n p`
- has type of the induction hypothesis
- `cong suc` prefaces `suc` to each side of inductive hypothesis

A relation is a CONGRUENCE for a given function if it is preserved by applying that function.
If `e` is evidence that `x ≡ y`, then `cong f e` is evidence `f x ≡ f y`, for any `f`.

Here the inductive hypothesis is not assumed, but instead proved by a
recursive invocation of the function we are defining, `+-assoc m n p`.

well founded : associativity of larger numbers is proved in of associativity of smaller numbers.

e.g., `assoc (suc m) n p` is proved using `assoc m n p`.

## Induction as recursion

Concrete example of how induction corresponds to recursion : instantiate `m` to `2`
-}

+-assoc-2 : ∀ (n p : ℕ) → (2 + n) + p ≡ 2 + (n + p)
+-assoc-2 n p =
  begin
    (2 + n) + p
  ≡⟨⟩
    suc (1 + n) + p
  ≡⟨⟩
    suc ((1 + n) + p)
  ≡⟨ cong suc (+-assoc-1 n p) ⟩
    suc (1 + (n + p))
  ≡⟨⟩
    2 + (n + p)
  ∎
  where
  +-assoc-1 : ∀ (n p : ℕ) → (1 + n) + p ≡ 1 + (n + p)
  +-assoc-1 n p =
    begin
      (1 + n) + p
    ≡⟨⟩
      suc (0 + n) + p
    ≡⟨⟩
      suc ((0 + n) + p)
    ≡⟨ cong suc (+-assoc-0 n p) ⟩
      suc (0 + (n + p))
    ≡⟨⟩
      1 + (n + p)
    ∎
    where
    +-assoc-0 : ∀ (n p : ℕ) → (0 + n) + p ≡ 0 + (n + p)
    +-assoc-0 n p =
      begin
        (0 + n) + p
      ≡⟨⟩
        n + p
      ≡⟨⟩
        0 + (n + p)
      ∎

{-
------------------------------------------------------------------------------
## Terminology and notation

Evidence for a universal quantifier is a function.  The notations

    +-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)

and

    +-assoc : ∀ (m : ℕ) → ∀ (n : ℕ) → ∀ (p : ℕ) → (m + n) + p ≡ m + (n + p)

are equivalent. They differ from a function type such as `ℕ → ℕ → ℕ`
in that variables are associated with each argument type, and the
result type may mention (or depend upon) these variables; hence they
are called _DEPENDENT functions_.

------------------------------------------------------------------------------
## Our second proof: commutativity

    m + n ≡ n + m

two lemmas used in proof

### The first lemma

The base case of the definition of addition states that zero is a left-identity:

    zero + n ≡ n

First lemma states that zero is also a right-identity:
-}

-- proof by induction on `m`
+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m

-- base case : show: zero + zero ≡ zero (using simplification)
+-identityʳ zero =
  begin
    zero + zero
  ≡⟨⟩
    zero
  ∎

-- inductive case : show: (suc m) + zero = suc m
+-identityʳ (suc m) =
  begin
    suc m + zero
  ≡⟨⟩
    suc (m + zero)
  ≡⟨ cong suc (+-identityʳ m) ⟩
    suc m
  ∎

{-
simplifying both sides with the inductive case of addition yields suc (m + zero) = suc m

This in turn follows by prefacing `suc` to both sides of the induction hypothesis:

    m + zero ≡ m

Reading the chain of equations down from the top and up from the bottom
takes us to the simplified equation above.  The remaining equation has the justification:

    ⟨ cong suc (+-identityʳ m) ⟩

Here, the recursive invocation `+-identityʳ m` has as its type the
induction hypothesis, and `cong suc` prefaces `suc` to each side to
yield the needed equation.  This completes the first lemma.

### The second lemma

inductive case of def of addition pushes `suc` on 1st arg to the outside:

    suc m + n ≡ suc (m + n)

second lemma does same for `suc` on 2nd arg:

    m + suc n ≡ suc (m + n)
-}

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n =
  begin
    zero + suc n
  ≡⟨⟩
    suc n
  ≡⟨⟩
    suc (zero + n)
  ∎
+-suc (suc m) n =
  begin
    suc m + suc n
  ≡⟨⟩
    suc (m + suc n)
  ≡⟨ cong suc (+-suc m n) ⟩
    suc (suc (m + n))
  ≡⟨⟩
    suc (suc m + n)
  ∎

{-
The signature states that we are defining the identifier `+-suc` which provides
evidence for the proposition:

    ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)

Evidence for the proposition is a function that accepts two natural numbers,
binds them to `m` and `n`, and returns evidence for the corresponding instance
of the equation.  The proof is by induction on `m`.

For the base case, we must show:

    zero + suc n ≡ suc (zero + n)

Simplifying with the base case of addition, this is straightforward.

For the inductive case, we must show:

    suc m + suc n ≡ suc (suc m + n)

Simplifying both sides with the inductive case of addition yields the equation:

    suc (m + suc n) ≡ suc (suc (m + n))

This in turn follows by prefacing `suc` to both sides of the induction
hypothesis:

    m + suc n ≡ suc (m + n)

Reading the chain of equations down from the top and up from the bottom
takes us to the simplified equation in the middle.  The remaining
equation has the justification:

    ⟨ cong suc (+-suc m n) ⟩

Here, the recursive invocation `+-suc m n` has as its type the
induction hypothesis, and `cong suc` prefaces `suc` to each side to
yield the needed equation.  This completes the second lemma.

### The proposition

Finally, here is our proposition's statement and proof:
```
+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identityʳ m ⟩
    m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
    m + suc n
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎
```
The first line states that we are defining the identifier
`+-comm` which provides evidence for the proposition:

    ∀ (m n : ℕ) → m + n ≡ n + m

Evidence for the proposition is a function that accepts two
natural numbers, binds them to `m` and `n`, and returns evidence for the
corresponding instance of the equation.  The proof is by
induction on `n`.  (Not on `m` this time!)

For the base case, we must show:

    m + zero ≡ zero + m

Simplifying both sides with the base case of addition yields the equation:

    m + zero ≡ m

The remaining equation has the justification `⟨ +-identityʳ m ⟩`,
which invokes the first lemma.

For the inductive case, we must show:

    m + suc n ≡ suc n + m

Simplifying both sides with the inductive case of addition yields the equation:

    m + suc n ≡ suc (n + m)

We show this in two steps.  First, we have:

    m + suc n ≡ suc (m + n)

which is justified by the second lemma, `⟨ +-suc m n ⟩`.  Then we
have

    suc (m + n) ≡ suc (n + m)

which is justified by congruence and the induction hypothesis,
`⟨ cong suc (+-comm m n) ⟩`.  This completes the proof.

Agda requires that identifiers are defined before they are used,
so we must present the lemmas before the main proposition, as we
have done above.  In practice, one will often attempt to prove
the main proposition first, and the equations required to do so
will suggest what lemmas to prove.

------------------------------------------------------------------------------
## Our first corollary: rearranging {name=sections}

We can apply associativity to rearrange parentheses however we like.
Here is an example:
```
+-rearrange : ∀ (m n p q : ℕ) → (m + n) + (p + q) ≡ m + (n + p) + q
+-rearrange m n p q =
  begin
    (m + n) + (p + q)
  ≡⟨ +-assoc m n (p + q) ⟩
    m + (n + (p + q))
  ≡⟨ cong (m +_) (sym (+-assoc n p q)) ⟩
    m + ((n + p) + q)
  ≡⟨ sym (+-assoc m (n + p) q) ⟩
    (m + (n + p)) + q
  ∎
```
No induction is required, we simply apply associativity twice.
A few points are worthy of note.

First, addition associates to the left, so `m + (n + p) + q`
stands for `(m + (n + p)) + q`.

Second, we use `sym` to interchange the sides of an equation.
Proposition `+-assoc n p q` shifts parentheses from right to left:

    (n + p) + q ≡ n + (p + q)

To shift them the other way, we use `sym (+-assoc n p q)`:

    n + (p + q) ≡ (n + p) + q

In general, if `e` provides evidence for `x ≡ y` then `sym e` provides
evidence for `y ≡ x`.

Third, Agda supports a variant of the _section_ notation introduced by
Richard Bird.  We write `(x +_)` for the function that applied to `y`
returns `x + y`.  Thus, applying the congruence `cong (m +_)` takes
the above equation into:

    m + (n + (p + q)) ≡ m + ((n + p) + q)

Similarly, we write `(_+ x)` for the function that applied to `y`
returns `y + x`; the same works for any infix operator.



## Creation, one last time

Returning to the proof of associativity, it may be helpful to view the inductive
proof (or, equivalently, the recursive definition) as a creation story.  This
time we are concerned with judgments asserting associativity:

     -- In the beginning, we know nothing about associativity.

Now, we apply the rules to all the judgments we know about.  The base
case tells us that `(zero + n) + p ≡ zero + (n + p)` for every natural
`n` and `p`.  The inductive case tells us that if
`(m + n) + p ≡ m + (n + p)` (on the day before today) then
`(suc m + n) + p ≡ suc m + (n + p)` (today).
We didn't know any judgments about associativity before today, so that
rule doesn't give us any new judgments:

    -- On the first day, we know about associativity of 0.
    (0 + 0) + 0 ≡ 0 + (0 + 0)   ...   (0 + 4) + 5 ≡ 0 + (4 + 5)   ...

Then we repeat the process, so on the next day we know about all the
judgments from the day before, plus any judgments added by the rules.
The base case tells us nothing new, but now the inductive case adds
more judgments:

    -- On the second day, we know about associativity of 0 and 1.
    (0 + 0) + 0 ≡ 0 + (0 + 0)   ...   (0 + 4) + 5 ≡ 0 + (4 + 5)   ...
    (1 + 0) + 0 ≡ 1 + (0 + 0)   ...   (1 + 4) + 5 ≡ 1 + (4 + 5)   ...

And we repeat the process again:

    -- On the third day, we know about associativity of 0, 1, and 2.
    (0 + 0) + 0 ≡ 0 + (0 + 0)   ...   (0 + 4) + 5 ≡ 0 + (4 + 5)   ...
    (1 + 0) + 0 ≡ 1 + (0 + 0)   ...   (1 + 4) + 5 ≡ 1 + (4 + 5)   ...
    (2 + 0) + 0 ≡ 2 + (0 + 0)   ...   (2 + 4) + 5 ≡ 2 + (4 + 5)   ...

You've got the hang of it by now:

    -- On the fourth day, we know about associativity of 0, 1, 2, and 3.
    (0 + 0) + 0 ≡ 0 + (0 + 0)   ...   (0 + 4) + 5 ≡ 0 + (4 + 5)   ...
    (1 + 0) + 0 ≡ 1 + (0 + 0)   ...   (1 + 4) + 5 ≡ 1 + (4 + 5)   ...
    (2 + 0) + 0 ≡ 2 + (0 + 0)   ...   (2 + 4) + 5 ≡ 2 + (4 + 5)   ...
    (3 + 0) + 0 ≡ 3 + (0 + 0)   ...   (3 + 4) + 5 ≡ 3 + (4 + 5)   ...

The process continues.  On the _m_'th day we will know all the
judgments where the first number is less than _m_.

There is also a completely finite approach to generating the same equations,
which is left as an exercise for the reader.

#### Exercise `finite-|-assoc` (stretch) {name=finite-plus-assoc}

Write out what is known about associativity of addition on each of the
first four days using a finite story of creation, as
[earlier](/Naturals/#finite-creation).

```
-- Your code goes here
```

## Associativity with rewrite

There is more than one way to skin a cat.  Here is a second proof of
associativity of addition in Agda, using `rewrite` rather than chains of
equations:
```
+-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc′ zero    n p                          =  refl
+-assoc′ (suc m) n p  rewrite +-assoc′ m n p  =  refl
```

For the base case, we must show:

    (zero + n) + p ≡ zero + (n + p)

Simplifying both sides with the base case of addition yields the equation:

    n + p ≡ n + p

This holds trivially. The proof that a term is equal to itself is written `refl`.

For the inductive case, we must show:

    (suc m + n) + p ≡ suc m + (n + p)

Simplifying both sides with the inductive case of addition yields the equation:

    suc ((m + n) + p) ≡ suc (m + (n + p))

After rewriting with the inductive hypothesis these two terms are equal, and the
proof is again given by `refl`.  Rewriting by a given equation is indicated by
the keyword `rewrite` followed by a proof of that equation.  Rewriting avoids
not only chains of equations but also the need to invoke `cong`.


## Commutativity with rewrite

Here is a second proof of commutativity of addition, using `rewrite` rather than
chains of equations:
```
+-identity′ : ∀ (n : ℕ) → n + zero ≡ n
+-identity′ zero = refl
+-identity′ (suc n) rewrite +-identity′ n = refl

+-suc′ : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc′ zero n = refl
+-suc′ (suc m) n rewrite +-suc′ m n = refl

+-comm′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′ m zero rewrite +-identity′ m = refl
+-comm′ m (suc n) rewrite +-suc′ m n | +-comm′ m n = refl
```
In the final line, rewriting with two equations is
indicated by separating the two proofs of the relevant equations by a
vertical bar; the rewrite on the left is performed before that on the
right.


## Building proofs interactively

It is instructive to see how to build the alternative proof of
associativity using the interactive features of Agda in Emacs.
Begin by typing:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ m n p = ?

The question mark indicates that you would like Agda to help with
filling in that part of the code.  If you type `C-c C-l` (control-c
followed by control-l), the question mark will be replaced:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ m n p = { }0

The empty braces are called a _hole_, and 0 is a number used for
referring to the hole.  The hole may display highlighted in green.
Emacs will also create a new window at the bottom of the screen
displaying the text:

    ?0 : ((m + n) + p) ≡ (m + (n + p))

This indicates that hole 0 is to be filled in with a proof of
the stated judgment.

We wish to prove the proposition by induction on `m`.  Move
the cursor into the hole and type `C-c C-c`.  You will be given
the prompt:

    pattern variables to case (empty for split on result):

Typing `m` will cause a split on that variable, resulting
in an update to the code:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = { }0
    +-assoc′ (suc m) n p = { }1

There are now two holes, and the window at the bottom tells you what
each is required to prove:

    ?0 : ((zero + n) + p) ≡ (zero + (n + p))
    ?1 : ((suc m + n) + p) ≡ (suc m + (n + p))

Going into hole 0 and typing `C-c C-,` will display the text:

    Goal: (n + p) ≡ (n + p)
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ

This indicates that after simplification the goal for hole 0 is as
stated, and that variables `p` and `n` of the stated types are
available to use in the proof.  The proof of the given goal is
trivial, and going into the goal and typing `C-c C-r` will fill it in.
Typing `C-c C-l` renumbers the remaining hole to 0:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p = { }0

Going into the new hole 0 and typing `C-c C-,` will display the text:

    Goal: suc ((m + n) + p) ≡ suc (m + (n + p))
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ
    m : ℕ

Again, this gives the simplified goal and the available variables.
In this case, we need to rewrite by the induction
hypothesis, so let's edit the text accordingly:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p rewrite +-assoc′ m n p = { }0

Going into the remaining hole and typing `C-c C-,` will display the text:

    Goal: suc (m + (n + p)) ≡ suc (m + (n + p))
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ
    m : ℕ

The proof of the given goal is trivial, and going into the goal and
typing `C-c C-r` will fill it in, completing the proof:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p rewrite +-assoc′ m n p = refl


#### Exercise `+-swap` (recommended) {name=plus-swap}

Show

    m + (n + p) ≡ n + (m + p)

for all naturals `m`, `n`, and `p`. No induction is needed,
just apply the previous results which show addition
is associative and commutative.

```
-- Your code goes here
```


#### Exercise `*-distrib-+` (recommended) {name=times-distrib-plus}

Show multiplication distributes over addition, that is,

    (m + n) * p ≡ m * p + n * p

for all naturals `m`, `n`, and `p`.

```
-- Your code goes here
```


#### Exercise `*-assoc` (recommended) {name=times-assoc}

Show multiplication is associative, that is,

    (m * n) * p ≡ m * (n * p)

for all naturals `m`, `n`, and `p`.

```
-- Your code goes here
```


#### Exercise `*-comm` (practice) {name=times-comm}

Show multiplication is commutative, that is,

    m * n ≡ n * m

for all naturals `m` and `n`.  As with commutativity of addition,
you will need to formulate and prove suitable lemmas.

```
-- Your code goes here
```


#### Exercise `0∸n≡0` (practice) {name=zero-monus}

Show

    zero ∸ n ≡ zero

for all naturals `n`. Did your proof require induction?

```
-- Your code goes here
```


#### Exercise `∸-|-assoc` (practice) {name=monus-plus-assoc}

Show that monus associates with addition, that is,

    m ∸ n ∸ p ≡ m ∸ (n + p)

for all naturals `m`, `n`, and `p`.

```
-- Your code goes here
```


#### Exercise `+*^` (stretch)

Show the following three laws

     m ^ (n + p) ≡ (m ^ n) * (m ^ p)  (^-distribˡ-|-*)
     (m * n) ^ p ≡ (m ^ p) * (n ^ p)  (^-distribʳ-*)
     (m ^ n) ^ p ≡ m ^ (n * p)        (^-*-assoc)

for all `m`, `n`, and `p`.


#### Exercise `Bin-laws` (stretch) {name=Bin-laws}

Recall that
Exercise [Bin](/Naturals/#Bin)
defines a datatype `Bin` of bitstrings representing natural numbers,
and asks you to define functions

    inc   : Bin → Bin
    to    : ℕ → Bin
    from  : Bin → ℕ

Consider the following laws, where `n` ranges over naturals and `b`
over bitstrings:

    from (inc b) ≡ suc (from b)
    to (from b) ≡ b
    from (to n) ≡ n

For each law: if it holds, prove; if not, give a counterexample.

```
-- Your code goes here
```


## Standard library

Definitions similar to those in this chapter can be found in the standard library:
```
import Data.Nat.Properties using (+-assoc; +-identityʳ; +-suc; +-comm)
```

## Unicode

This chapter uses the following unicode:

    ∀  U+2200  FOR ALL (\forall, \all)
    ʳ  U+02B3  MODIFIER LETTER SMALL R (\^r)
    ′  U+2032  PRIME (\')
    ″  U+2033  DOUBLE PRIME (\')
    ‴  U+2034  TRIPLE PRIME (\')
    ⁗  U+2057  QUADRUPLE PRIME (\')

Similar to `\r`, the command `\^r` gives access to a variety of
superscript rightward arrows, and also a superscript letter `r`.
The command `\'` gives access to a range of primes (`′ ″ ‴ ⁗`).
-}
