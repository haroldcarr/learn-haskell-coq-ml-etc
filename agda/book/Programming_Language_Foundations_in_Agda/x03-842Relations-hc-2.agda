module x03-842Relations-hc-2 where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym) -- added sym
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; *-comm; *-zeroʳ)

------------------------------------------------------------------------------
-- inequality : ≤

data _≤_ : ℕ → ℕ → Set where

  z≤n : ∀ {n : ℕ}
      --------
    → zero ≤ n

  s≤s : ∀ {m n : ℕ}
    →     m ≤     n
      -------------
    → suc m ≤ suc n

infix 4 _≤_

_ : 2 ≤ 4 -- multiple 'refine' will do this automatically
_ = s≤s (s≤s z≤n)

_ : 2 ≤ 4 -- implicit args
_ = s≤s {1} {3} (s≤s {0} {2} z≤n)

_ : 2 ≤ 4 -- named implicit args
_ = s≤s {m = 1} {n = 3} (s≤s {m = 0} {n = 2} z≤n)

_ : 2 ≤ 4 -- some implicit named args
_ = s≤s {n = 3} (s≤s {m = 0} z≤n)


--------------------------------------------------
-- inversion
inv-s≤s : ∀ {m n : ℕ}
  → suc m ≤ suc n
    -------------
  →     m ≤     n
inv-s≤s (s≤s m≤n) = m≤n

inv-z≤n : ∀ {m : ℕ}
  → m ≤ zero
    --------
  → m ≡ zero
inv-z≤n z≤n = refl

-- reflexive
≤-refl : ∀ {n : ℕ}
    -----
  → n ≤ n
≤-refl {zero}  = z≤n
≤-refl {suc n} = s≤s (≤-refl {n})

--------------------------------------------------
-- transitive
≤-trans : ∀ {m n p : ℕ} -- note implicit arguments
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans  z≤n           n≤p  = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

≤-trans′ : ∀ (m n p : ℕ) -- without implicit arguments
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans′      .0      _       _
         z≤n n≤p            = z≤n
≤-trans′ (suc m) (suc n) (suc p)
        (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans′ m n p m≤n n≤p)

--------------------------------------------------
-- antisymmetry

≤-antisym : ∀ {m n : ℕ}
  → m ≤ n
  → n ≤ m
    -----
  → m ≡ n
≤-antisym      z≤n       z≤n = refl
≤-antisym (s≤s m≤n) (s≤s n≤m) rewrite ≤-antisym m≤n n≤m = refl

--------------------------------------------------
-- total ordering def with parameters

data Total (m n : ℕ) : Set where

  forward :
      m ≤ n
      ---------
    → Total m n

  flipped :
      n ≤ m
      ---------
    → Total m n

-- equivalent def without parameters
data Total′ : ℕ → ℕ → Set where

  forward′ : ∀ {m n : ℕ}
    → m ≤ n
      ----------
    → Total′ m n

  flipped′ : ∀ {m n : ℕ}
    → n ≤ m
      ----------
    → Total′ m n

-- prove ≤ is a total order -- using WITH
≤-total : ∀ (m n : ℕ) → Total m n
≤-total  zero        n = forward z≤n
≤-total (suc m)  zero  = flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
... | forward x        = forward (s≤s x)
... | flipped x        = flipped (s≤s x)

≤-total′ : ∀ (m n : ℕ) → Total m n -- using helper defined in 'where'
≤-total′  zero        n  = forward z≤n
≤-total′ (suc m)  zero   = flipped z≤n
≤-total′ (suc m) (suc n) = helper (≤-total m n)
  where
    helper : Total m n → Total (suc m) (suc n)
    helper (forward x) = forward (s≤s x)
    helper (flipped x) = flipped (s≤s x)

-- same but split on n first
≤-totalN : ∀ (m n : ℕ) → Total m n
≤-totalN      m   zero   = flipped z≤n
≤-totalN zero         n  = forward z≤n
≤-totalN (suc m) (suc n) with ≤-totalN m n
... | forward x = forward (s≤s x)
... | flipped x = flipped (s≤s x)

--------------------------------------------------
-- monotonicity of + with respect to ≤

-- inequality added on right
+-monoʳ-≤ : ∀ (m p q : ℕ)
  →     p ≤     q
    -------------
  → m + p ≤ m + q
+-monoʳ-≤  zero   p q p≤q = p≤q
+-monoʳ-≤ (suc m) p q p≤q           -- suc  m + p  ≤ suc  m + q
                                    -- suc (m + p) ≤ suc (m + q)
  = s≤s (+-monoʳ-≤ m p q p≤q)  -- can be done by refine then recurse

-- inequality added on left
+-monoˡ-≤ : ∀ (p q m : ℕ)
  → p     ≤ q
    -------------
  → p + m ≤ q + m
+-monoˡ-≤ p q m p≤q  -- p + m ≤ q + m
  rewrite
    +-comm p m       -- m + p ≤ q + m
  | +-comm q m       -- m + p ≤ m + q
  = +-monoʳ-≤ m p q p≤q

-- combine above
+-mono-≤ : ∀ (m n p q : ℕ)
  → m     ≤ n
  →     p ≤     q
    -------------
  → m + p ≤ n + q
+-mono-≤ m n p q m≤n p≤q = ≤-trans (+-monoʳ-≤ m p q p≤q ) (+-monoˡ-≤ m n q m≤n)

--------------------------------------------------
-- monotonicity of *

m≤n→n≡0→n≡0 : ∀ (m : ℕ) → m ≤ zero → m ≡ 0
m≤n→n≡0→n≡0 zero m≤0 = refl

-- inequality multiplied on right
*-monoʳ-≤ : ∀ (m p q : ℕ)
  →     p ≤     q
    -------------
  → m * p ≤ m * q
*-monoʳ-≤ zero   p       q  p≤q = z≤n
*-monoʳ-≤ m   zero       q  p≤q -- m *  zero ≤ m * q
  rewrite
    *-zeroʳ m                   --         0 ≤ m * q
  = z≤n
*-monoʳ-≤ m      p    zero  p≤q -- m *     p ≤ m * zero
  rewrite
    *-zeroʳ m                   -- m *     p ≤ 0
  | m≤n→n≡0→n≡0 p p≤q           -- m *     0 ≤ 0
  | *-zeroʳ m                   --         0 ≤ 0
  = z≤n
*-monoʳ-≤ m (suc p) (suc q) p≤q -- m * suc p ≤ m * suc q
  rewrite
    *-comm m (suc p)            -- m + p * m ≤ m * suc q
  | *-comm m (suc q)            -- m + p * m ≤ m + q * m
  | *-comm p m                  -- m + m * p ≤ m + q * m
  | *-comm q m                  -- m + m * p ≤ m + m * q
  = +-monoʳ-≤ m (m * p) (m * q) (*-monoʳ-≤ m p q (inv-s≤s p≤q))

-- inequality multiplied on left
*-monoˡ-≤ : ∀ (p q m : ℕ)
  → p     ≤ q
    -------------
  → p * m ≤ q * m
*-monoˡ-≤  p q m p≤q  -- p * m ≤ q * m
  rewrite
    *-comm p m        -- m * p ≤ q * m
  | *-comm q m        -- m * p ≤ m * q
  = *-monoʳ-≤ m p q p≤q

*-mono-≤ : ∀ (m n p q : ℕ)
  → m     ≤ n
  →     p ≤     q
    -------------
  → m * p ≤ n * q
*-mono-≤ m n p q m≤n p≤q = ≤-trans (*-monoʳ-≤ m p q p≤q ) (*-monoˡ-≤ m n q m≤n)


------------------------------------------------------------------------------
-- strict inequality : <

infix 4 _<_

data _<_ : ℕ → ℕ → Set where

  z<s : ∀ {n : ℕ}
      ------------
    → zero < suc n

  s<s : ∀ {m n : ℕ}
    →     m <     n
      -------------
    → suc m < suc n

<-trans : ∀ {m n p : ℕ}
        → m < n
        → n < p
        -------
        → m < p
<-trans  z<s    (s<s y) = z<s
<-trans (s<s x) (s<s y) = s<s (<-trans x y)

------------------------------------------------------------------------------

data Trichotomy (m n : ℕ) : Set where
  is-< : m < n → Trichotomy m n
  is-≡ : m ≡ n → Trichotomy m n
  is-> : n < m → Trichotomy m n

suc-injective : ∀ {m n : ℕ} → suc m ≡ suc n → m ≡ n
suc-injective refl = refl

m≡n→sucm≡sucn : ∀ (m n : ℕ) → m ≡ n → suc m ≡ suc n
m≡n→sucm≡sucn  zero    zero   p = refl
m≡n→sucm≡sucn  zero   (suc n) ()
m≡n→sucm≡sucn (suc m)  zero   ()
m≡n→sucm≡sucn (suc m) (suc n) p = cong suc (m≡n→sucm≡sucn m n (suc-injective p))

<-trichotomy : ∀ (m n : ℕ) → Trichotomy m n
<-trichotomy  zero    zero   = is-≡ refl
<-trichotomy  zero   (suc n) = is-< z<s
<-trichotomy (suc m)  zero   = is-> z<s
<-trichotomy (suc m) (suc n) with <-trichotomy m n
... | is-< m<n = is-< (s<s m<n)
... | is-≡ m≡n = is-≡ (m≡n→sucm≡sucn m n m≡n)
... | is-> n<m = is-> (s<s n<m)


------------------------------------------------------------------------------
-- monotonicity of + with respect to <

-- all I did was copy the +-monoʳ-≤ proof and replace ≤ with < - it worked
-- inequality added on right
+-monoʳ-< : ∀ (m p q : ℕ)
  →     p <     q
    -------------
  → m + p < m + q
+-monoʳ-<  zero   p q p<q = p<q
+-monoʳ-< (suc m) p q p<q
  = s<s (+-monoʳ-< m p q p<q)

-- inequality added on left
+-monoˡ-< : ∀ (p q m : ℕ)
  → p     < q
    -------------
  → p + m < q + m
+-monoˡ-< p q m p<q
  rewrite
    +-comm p m
  | +-comm q m
  = +-monoʳ-< m p q p<q

-- combine above
+-mono-< : ∀ (m n p q : ℕ)
  → m     < n
  →     p <     q
    -------------
  → m + p < n + q
+-mono-< m n p q m<n p<q = <-trans (+-monoʳ-< m p q p<q ) (+-monoˡ-< m n q m<n)

------------------------------------------------------------------------------
-- Proofs showing relation between strict inequality and inequality.
-- (Do the proofs in the order below, to avoid induction for two of the four proofs.)

--------------------------------------------------
-- turn ≤ into <

≤-<-to : ∀ {m n : ℕ}
       → m ≤     n
       → m < suc n
≤-<-to  {zero}  {zero}  z≤n      = z<s
≤-<-to  {zero} {suc n}  z≤n      = z<s
≤-<-to {suc m} {suc n} (s≤s m≤n) = s<s (≤-<-to m≤n)
--- removing unneeded stuff
≤-<-to' : ∀ {m n : ℕ}
        → m ≤     n
        → m < suc n
≤-<-to'                 z≤n      = z<s
≤-<-to'                (s≤s m≤n) = s<s (≤-<-to' m≤n)


≤-<--to′ : ∀ {m n : ℕ}
         → suc m ≤ n
         →     m < n
≤-<--to′ (s≤s sm≤n) = ≤-<-to sm≤n

--------------------------------------------------
-- turn < into ≤

≤-<-from : ∀ {m n : ℕ}
         →     m < n
         → suc m ≤ n
≤-<-from  z<s      = s≤s z≤n
≤-<-from (s<s m<n) = s≤s (≤-<-from m<n)


≤-<-from′ : ∀ {m n : ℕ}
          → m < suc n
          → m ≤     n
≤-<-from′  z<s       = z≤n
≤-<-from′ (s<s m<sn) = ≤-<-from m<sn

------------------------------------------------------------------------------
-- use the above to give a proof of <-trans that uses ≤-trans

m<n→m<sucn : ∀ {m n : ℕ} → m < n → m < suc n
m<n→m<sucn  z<s      = z<s
m<n→m<sucn (s<s m<n) = s<s (m<n→m<sucn m<n)

<-trans' : ∀ {m n p : ℕ}
        → m < n
        →     n < p
        -------
        → m <     p
<-trans' m<n n<p = ≤-<--to′ (≤-trans (≤-<-from m<n) ( ≤-<-from′ (m<n→m<sucn n<p)))


------------------------------------------------------------------------------
-- Mutually recursive datatypes. Specify types first, then definitions.

data even : ℕ → Set
data odd  : ℕ → Set

-- HC : added 'E' suffix to constructors
data even where

  zeroE :
      ---------
      even zero

  sucE  : ∀ {n : ℕ}
    → odd       n
      ------------
    → even (suc n)

-- HC : added 'O' suffix to constructor
data odd where

  sucO   : ∀ {n : ℕ}
    → even     n
      -----------
    → odd (suc n)


-------------------------
-- Mutually recursive proofs.  Specify types first, then implementations.

e+e≡e : ∀ {m n : ℕ}
  → even  m
  → even      n
    ------------
  → even (m + n)

o+e≡o : ∀ {m n : ℕ}
  → odd  m
  → even     n
    -----------
  → odd (m + n)

e+e≡e  zeroE   en =               en
e+e≡e (sucE x) en = sucE (o+e≡o x en)

o+e≡o (sucO x) en = sucO (e+e≡e x en)


-------------------------

o+o≡e : ∀ {m n : ℕ}
  → odd   m
  → odd       n
  --------------
  → even (m + n)

e+o≡o : ∀ {m n : ℕ}
  → even m
  → odd      n
    -----------
  → odd (m + n)

e+o≡o  zeroE   on = on
e+o≡o (sucE m) (sucO n) = sucO (o+o≡e m (sucO n))

o+o≡e (sucO m) on = sucE (e+o≡o m on)

