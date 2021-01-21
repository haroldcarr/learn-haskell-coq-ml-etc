module z04-lists where

open import bool
open import eq
open import nat
open import nat-thms
open import product-thms using (keep)
open import logic -- needed for filter-idem

{-
-- p 75

'data' : datatype declaration
'𝕃'    : name of type being declared
{ℓ}    : level
A      : element type (polymorphic)

𝕃 is a type level function
- takes a type (bound to 'A') - at level ℓ
- returns a type - at level same level ℓ
-}

data  𝕃 {ℓ} (A : Set ℓ) : Set ℓ where
  []   :                      𝕃 A
  _::_ : (x : A) (xs : 𝕃 A) → 𝕃 A

-- from lists.agda

[_] : ∀ {ℓ} {A : Set ℓ} → A → 𝕃 A
[ x ] = x :: []

-- p 77

length : ∀ {ℓ} {A : Set ℓ } → 𝕃 A → ℕ
length       []  = 0
length (x :: xs) = suc (length xs)

-- 78

_++_ : ∀ {ℓ} {A : Set ℓ} → 𝕃 A → 𝕃 A → 𝕃 A
[]        ++ ys =             ys
(x :: xs) ++ ys = x :: (xs ++ ys)

-- 79

map : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'}  → (A → B) → 𝕃 A → 𝕃 B
map f       []  = []
map f (x :: xs) = f x :: map f xs

-- 80

filter : ∀ {ℓ} {A : Set ℓ} → (A → 𝔹) → 𝕃 A → 𝕃 A
filter p       []  = []
filter p (x :: xs) = if p x then x :: r else r
 where
  r = filter p xs

-- p 81

remove : ∀ {ℓ} {A : Set ℓ} (eq : A → A → 𝔹) (a : A) (l : 𝕃 A) → 𝕃 A
remove eq a l = filter (λ x → ~ (eq a x)) l

-- p 82

data maybe {ℓ} (A : Set ℓ) : Set ℓ where
  just    : A → maybe A
  nothing :     maybe A

nth : ∀ {ℓ} {A : Set ℓ} → ℕ → 𝕃 A → maybe A
nth _             []  = nothing
nth 0       (x :: xs) = just x
nth (suc n) (x :: xs) = nth n xs

-- p 83

-- inefficient
sreverse : ∀ {ℓ} {A : Set ℓ} → 𝕃 A → 𝕃 A
sreverse      []  = []
sreverse (h :: t) = sreverse t ++ [ h ]

reverse-helper : ∀ {ℓ}{A : Set ℓ} → 𝕃 A → 𝕃 A → 𝕃 A
reverse-helper h       []  = h
reverse-helper h (x :: xs) = reverse-helper (x :: h) xs

reverse : ∀ {ℓ} {A : Set ℓ} → 𝕃 A → 𝕃 A
reverse l = reverse-helper [] l

------------------------------------------------------------------------------
-- p 84 Reasong about List Operations

length-++ : ∀ {ℓ}{A : Set ℓ} (l1 l2 : 𝕃 A)
          → length (l1 ++ l2) ≡ length l1 + length l2
length-++ [] l2 -- length ([] ++ l2) ≡ length [] + length l2
                -- length        l2  ≡             length l2
  = refl
length-++ (x :: xs) l2 --      length ((x :: xs) ++ l2)  ≡      length (x :: xs) + length l2
                       -- suc (length       (xs  ++ l2)) ≡ suc (length       xs  + length l2)
                       --                 |
  rewrite              -- IH              ≡
                       --                 v
    length-++ xs l2    -- suc (length  xs +  length l2)  ≡ suc (length       xs  + length l2)
  = refl

-- p 86

++-assoc : ∀ {ℓ} {A : Set ℓ} (l1 l2 l3 : 𝕃 A)
         → (l1 ++  l2) ++ l3
         ≡  l1 ++ (l2  ++ l3)
++-assoc [] l2 l3                  -- (([] ++ l2) ++ l3) ≡ ([] ++ (l2 ++ l3))
                                   --        (l2  ++ l3) ≡        (l2 ++ l3)
  = refl
++-assoc (x :: xs) l2 l3 -- (((x ::   xs) ++ l2) ++ l3)   ≡ ((x ::  xs) ++ (l2 ++ l3))
                         --   (x :: ((xs  ++ l2) ++ l3))  ≡  (x :: (xs  ++ (l2 ++ l3)))
  rewrite                -- IH               v
    ++-assoc xs l2 l3    --   (x ::  (xs ++ (l2  ++ l3))) ≡  (x :: (xs  ++ (l2 ++ l3)))
  = refl

{-
------------------------------------------------------------------------------
-- p 87 - WITH

for
- any type A (of any level),
- any predicate p on A
- any list of A
the length of the list after filtering l with p
<=
length of l
-}

length-filter : ∀ {ℓ} {A : Set ℓ} (p : A → 𝔹) (l : 𝕃 A)
              → length (filter p l) ≤ length l ≡ tt

-- proof case-splits input list

length-filter p      []         -- length (filter p []) ≤ length [] ≡ tt
                                --                   0  ≤         0 ≡ tt
  = refl

-- Consider cases where predicate returns tt or ff.
-- The predicate return value is NOT an input to length-filter.
-- WITH : extend pattern on left side with an additional pattern, here : | tt     and   | ff
length-filter p (x :: l) with p x
length-filter p (x :: l) | tt   --    length (filter p l)  <      length l
                                -- || length (filter p l) =ℕ      length l  ≡ tt
  = length-filter p l           -- IH
length-filter p (x :: l) | ff   --    length (filter p l)  < suc (length l)
  =                             -- || length (filter p l) =ℕ suc (length l) ≡ tt
  ≤-trans {length (filter p l)}
          (length-filter p l)   -- IH
          (≤-suc (length l))    -- ≤-suc proves length l ≤ suc (length l)

{- this is to see the goal for the non-nil case
lf : ∀ {ℓ} {A : Set ℓ} (p : A → 𝔹) (l : 𝕃 A)
   → length (filter p l) ≤ length l ≡ tt
lf p      []  = refl
lf p (x :: l) -- length                  (filter p (x :: l))                ≤      length (x :: l) ≡ tt
              -- length (if p x then x :: filter p       l else filter p l) ≤ suc (length       l) ≡ tt
  = {!!}

------------------------------------------------------------------------------
-- p 90 KEEP (called INSPECT in Agda standard library)

filtering a list twice using same predicate gives the same result as filtering it once

cannot use WITH because Agda only applies the p ≡ tt to the goal once, not the next iteration
(see page 92 for more details)

'with keep (p x)' : make additional variable (here p') available
-}

filter-idem : ∀ {ℓ} {A : Set ℓ} (p : A → 𝔹) (l : 𝕃 A)
            → (filter p (filter p l)) ≡ (filter p l)
filter-idem p []            -- filter p (filter p []) ≡ filter p []
                            --                    []  ≡          []
  = refl
{-
filter-idem p (x :: l)           -- filter p (filter p (x :: l)) ≡ filter p (x :: l)
                                 --
                                 -- filter p (if p x then x :: filter p l else filter p l)
                                 -- ≡ if p x then x :: filter p l else filter p l
  = {!!}
-}
filter-idem p (x :: l) with keep (p x)

filter-idem p (x :: l) | tt , p' -- filter p (if p x then x :: filter p l else filter p l)
                                 -- ≡ if p x then x :: filter p l else filter p l

  rewrite
    -- agda does not instantiate 'p x' in goal
    -- must explicit use it below to change 'p x' to 'tt' (or 'ff' further below)

    p'                           -- filter p (if tt then x :: filter p l else filter p l)
                                 -- ≡ if tt then x :: filter p l else filter p l
                                 --
                                 --   if p x then x :: filter p (filter p l) else filter p (filter p l)
                                 -- ≡ (x :: filter p l)

  -- use it again to eliminate the if

  | p'                           --   if tt  then x :: filter p (filter p l) else filter p (filter p l)
                                 -- ≡ (x :: filter p l)
                                 --
                                 --   (x :: filter p (filter p l)) ≡ (x :: filter p l)


  | filter-idem p l              --   (x :: filter p l) ≡ (x :: filter p l)
  = refl

filter-idem p (x :: l) | ff , p' -- filter p (if p x then x :: filter p l else filter p l)
                                 -- ≡ if p x then x :: filter p l else filter p l

  rewrite p'                     -- filter p (if ff then x :: filter p l else filter p l)
                                 -- ≡ if ff  then x :: filter p l else filter p l
                                 --
                                 -- filter p (filter p l) ≡ filter p l
  = filter-idem p l

{-
------------------------------------------------------------------------------
-- p 93

reverse-helper args
- reverse of the list processed so far
- rest of list to be reversed

tricky to figure out what general property of reverse-helper to prove
-length of reverse-helper h l is sum of lengths of h and l
-}

length-reverse-helper
  : ∀ {ℓ} {A : Set ℓ} (h l : 𝕃 A)
  → length (reverse-helper h l) ≡ length h + length l

length-reverse-helper h []                     -- length (reverse-helper h []) ≡ length h + length []
                                               -- length h                     ≡ length h + 0
  rewrite
    +comm (length h) 0                         -- length h                     ≡ length h
  -- also can do via
  -- rewrite +0 (length h) = refl
  = refl
length-reverse-helper h (x :: xs)
                                               -- length (reverse-helper h (x :: xs)) ≡ length h + length (x :: xs)
                                               -- length (reverse-helper (x :: h) xs) ≡ length h + suc (length xs)
  rewrite
    length-reverse-helper (x :: h) xs -- IH    --          suc (length h + length xs) ≡ length h + suc (length xs)
  | +suc (length h) (length xs)                --          suc (length h + length xs) ≡ suc (length h + length xs)
  = refl

length-reverse : ∀ {ℓ} {A : Set ℓ} (l : 𝕃 A)
               → length (reverse l) ≡ length l
length-reverse l = length-reverse-helper [] l

{-
------------------------------------------------------------------------------
-- p 95  conclusion : WITH     and    KEEP (aka INSPECT)

-- p 96 EXERCISES

-- 1
-}

-------------------------
-- NOT TRUE
-- 1a : ∀ {ℓ} {A : Set ℓ} (l1 l2 : 𝕃 A) → l1 ++ l2 ≡ l2 ++ l1

-------------------------
-- NOT TRUE
-- 1b : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} (f : A → B) (l : 𝕃 A) → length (map f l) ≡ suc (length l)

-------------------------
-- 1c: TRUE

repeat : ∀ {ℓ} {A : Set ℓ} → ℕ → A → 𝕃 A
repeat 0 a = []
repeat (suc n) a = a :: (repeat n a)

1c : ∀ {ℓ} {A : Set ℓ} {p : A → 𝔹} {a : A} (n : ℕ)
   → p a ≡ ff
   → filter p (repeat n a) ≡ []
1c 0 prop            -- filter p (repeat zero a) ≡ []
                     --                       [] ≡ []
  = refl
1c (suc n) prop      -- filter p (repeat (suc n) a) ≡ []
                     -- if p a then a :: filter p (repeat n a) else filter p (repeat n a) ≡ []
  rewrite
    prop             -- if ff  then a :: filter p (repeat n a) else filter p (repeat n a) ≡ []
                     --                                             filter p (repeat n a) ≡ []
  = 1c n prop -- IH

-------------------------
-- NOT TRUE
-- 1d : ∀ {ℓ} {A : Set ℓ} (l : 𝕃 A) → is-empty l ≡ tt → is-empty (reverse l) ≡ ff

-------------------------
-- TRUE
1e : ∀ {ℓ} {A : Set ℓ} (p : A -> 𝔹) (l1 l2 : 𝕃 A)
   → filter p (l1 ++          l2)
   ≡ filter p  l1 ++ filter p l2
1e p       []  l2               -- filter p ([] ++ l2) ≡ (filter p [] ++ filter p l2)
                                -- filter p        l2  ≡  filter p                l2
  = refl
1e p (x :: xs) l2 with keep (p x)
1e p (x :: xs) l2 | tt , p' --     if p x then x :: filter p (xs ++ l2) else filter p (xs ++ l2)
                            -- ≡ ((if p x then x :: filter p xs else filter p xs) ++ filter p l2)
  rewrite
    p'                      --     if tt  then x :: filter p (xs ++ l2) else filter p (xs ++ l2)
                            -- ≡ ((if tt  then x :: filter p xs else filter p xs) ++ filter p l2)
                            --
                            --   (x ::  filter p (xs ++          l2))
                            -- ≡ (x :: (filter p  xs ++ filter p l2))

  | 1e p xs l2              --   (x :: (filter p  xs ++ filter p l2))
                            -- ≡ (x :: (filter p  xs ++ filter p l2))
  = refl
1e p (x :: xs) l2 | ff , p' --     if p x then x :: filter p (xs ++ l2) else filter p (xs ++ l2)
                            -- ≡ ((if p x then x :: filter p xs else filter p xs) ++ filter p l2)
  rewrite
    p'                      --     if ff  then x :: filter p (xs ++ l2) else filter p (xs ++ l2)
                            -- ≡ ((if ff  then x :: filter p xs else filter p xs) ++ filter p l2)
                            --
                            -- filter p (xs ++ l2) ≡ (filter p xs ++ filter p l2)
  = 1e p xs l2              -- IH

--------------------------------------------------
-- 2

-------------------------
-- 2a [] : 𝕃 Set  (𝕃 _A_277)

-------------------------
2b : ∀ {ℓ} {A : Set ℓ} → 𝕃 A → ℕ
2b       []  = 0
2b (x :: xs) = suc (2b xs)

-------------------------
-- Note: 2ci does not explicitly give ℓ
-- Note: some other answers are probably OK - did not check
2c : ∀ {ℓ} {A : Set ℓ} {B : Set ℓ} {C : Set ℓ}
   → (A → B)
   → (B → C)
   → 𝕃 A
   → 𝕃 C
2c f g x = map g (map f x)

--------------------------------------------------
-- 3

takeWhile : ∀ {ℓ} {A : Set ℓ} → (A → 𝔹) -> 𝕃 A → 𝕃 A
takeWhile p       []  = []
takeWhile p (x :: xs) = if p x then x :: takeWhile p xs else takeWhile p xs

--------------------------------------------------
-- 4

4twr : ∀ {ℓ} {A : Set ℓ} {p : A → 𝔹} {a : A} (n : ℕ)
     → p a ≡ tt
     → takeWhile p (repeat n a) ≡ repeat n a
4twr  zero p               -- takeWhile p₁ (repeat zero a) ≡ repeat zero a
                           --                           [] ≡ []
  = refl
4twr {l} {A} {pred} {a} (suc n) p -- takeWhile p₁ (repeat (suc n) a) ≡ repeat (suc n) a
                                  --   if p₁ a then a :: takeWhile p₁ (repeat n a) else takeWhile p₁ (repeat n a)
                                  -- ≡             (a ::               repeat n a)
  rewrite
    p                             --   if tt   then a :: takeWhile p₁ (repeat n a) else takeWhile p₁ (repeat n a)
                                  -- ≡             (a ::               repeat n a)
                                  --               (a :: takeWhile p₁ (repeat n a))
                                  -- ≡             (a ::               repeat n a)
  | 4twr {l} {A} {pred} {a} n p   --               (a ::               repeat n a) ≡ (a :: repeat n a)
  = refl

--------------------------------------------------
-- 5

take    : ∀ {ℓ} {A : Set ℓ} → ℕ → 𝕃 A → 𝕃 A
take         0         _  = []
take         _        []  = []
take    (suc n) (x :: xs) = x :: take n xs

--------------------------------------------------
-- 6

nthTail : ∀ {ℓ} {A : Set ℓ} → ℕ → 𝕃 A → 𝕃 A
nthTail      0        xs  = xs
nthTail      _        []  = []
nthTail (suc n) (x :: xs) = nthTail n xs
{-
6tn : ∀ {ℓ} {A : Set ℓ} {a : A} (n : ℕ) (l : 𝕃 A)
    → take n l ++ nthTail n l ≡ l
6tn  zero   l              -- (take zero l ++ nthTail zero l) ≡ l
                           --                             l  ≡ l
  = refl
6tn {ℓ} {A} {a} (suc n) l  -- (take (suc n) l ++ nthTail (suc n) l) ≡ l
  rewrite
    6tn {ℓ} {A} {a} n l
  = {!!}
-}

6tn : ∀ {ℓ} {A : Set ℓ} {a : A} (n : ℕ) (l : 𝕃 A)
    → take n l ++ nthTail n l ≡ l
6tn               zero        []  --       (take zero      []      ++ nthTail   zero        []) ≡ []
                                  --                                                        []  ≡ []
  = refl
6tn             (suc n)       []  --       (take (suc n)       []  ++ nthTail (suc n)       [])  ≡ []
                                  --                                                        []   ≡ []
  = refl
6tn               zero  (x :: xs) --       (take   zero  (x :: xs) ++ nthTail   zero  (x :: xs)) ≡ (x :: xs)
                                  -- (x :: xs)  ≡ (x :: xs)
  = refl
6tn {ℓ} {A} {a} (suc n) (x :: xs) --       (take (suc n) (x :: xs) ++ nthTail (suc n) (x :: xs)) ≡ (x :: xs)
                                  -- (x :: (take      n        xs  ++ nthTail      n        xs)) ≡ (x :: xs)
  rewrite
    6tn {ℓ} {A} {a} n xs          -- (x :: xs)                                                   ≡ (x :: xs)
  = refl
