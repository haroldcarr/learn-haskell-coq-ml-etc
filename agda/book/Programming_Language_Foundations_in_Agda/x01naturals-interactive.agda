module x01naturals-interactive where

data ℕ : Set where
  zero :     ℕ -- base case
  suc  : ℕ → ℕ -- inductive case

{-# BUILTIN NATURAL ℕ #-}

{-
import Relation.Binary.PropositionalEquality as Eq
open Eq             using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

------------------------------------------------------------------------------
## Writing definitions interactively

    _+_ : ℕ → ℕ → ℕ
    m + n = ?

? : ask Agda to fill in
`C-c C-l`
? replaced:

    _+_ : ℕ → ℕ → ℕ
    m + n = { }0

empty braces : a "numbered" *hole*
Emacs will also create a window displaying the text

    ?0 : ℕ

to indicate that hole 0 is to be filled in with a term of type `ℕ`.

`C-c C-f` (for **f**orward) will move you into the next hole.

To define addition by recursion on the first argument:
cursor in hole : `C-c C-c` (for **c**ase).
prompt:

    pattern variables to case (empty for split on result):

Type `m` : case split on `m`:

    _+_ : ℕ → ℕ → ℕ
    zero + n = { }0
    suc m + n = { }1

window at the bottom type of each:

    ?0 : ℕ
    ?1 : ℕ

in hole 0 : `C-c C-,`
displays info on required type of hole, and what free variables are available:

    Goal: ℕ
    ————————————————————————————————————————————————————————————
    n : ℕ

suggests filling the hole with `n`
after hole filled in : `C-c C-space` : removes hole:

    _+_ : ℕ → ℕ → ℕ
    zero + n = n
    suc m + n = { }1

in hole 1 : `C-c C-,`
displays info

    Goal: ℕ
    ————————————————————————————————————————————————————————————
    n : ℕ
    m : ℕ

in hole : `C-c C-r` (for **r**efine)
will fill in with a constructor (if there is a unique choice)
or tell you what constructors you might use, if there is a choice

    Don't know which constructor to introduce of zero or suc

fill in hole with `suc ?` : `C-c C-space`

    _+_ : ℕ → ℕ → ℕ
    zero + n = n
    suc m + n = suc { }1

in new hole : `C-c C-,`

    Goal: ℕ
    ————————————————————————————————————————————————————————————
    n : ℕ
    m : ℕ

fill hole with `m + n` : `C-c C-space`
complete the program:

    _+_ : ℕ → ℕ → ℕ
    zero + n = n
    suc m + n = suc (m + n)

-}
