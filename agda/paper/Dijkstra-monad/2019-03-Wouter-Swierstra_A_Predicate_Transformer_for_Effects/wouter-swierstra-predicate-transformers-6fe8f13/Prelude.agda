
open import Level


const : {l₁ l₂ : Level} {a : Set l₁} {b : Set l₂} -> a -> b -> a
const x _ = x

id : {l : Level} {a : Set l} -> a -> a
id x = x

flip : ∀ {l : Level} {a : Set l} {b : Set l} {c : Set l}
  -> (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

infixr 90 _∘_
_∘_ : ∀ {l l' l''} {a : Set l} {b : Set l'} {c : Set l''} ->
      (b -> c) -> (a -> b) -> a -> c
f ∘ g = λ x → f (g x)

open import Relation.Binary.PropositionalEquality public
  hiding (preorder; cong)
  renaming ([_] to [[[_]]])
infix 1 _==_
_==_ = _≡_

cong : ∀ {l₁ l₂} {a : Set l₁} {b : Set l₂} → (f : a → b) → ∀ {x y} → x == y → f x == f y
cong f refl = refl

cong2 : {a b c : Set} {x y : a} {z w : b} (f : a -> b -> c) -> x == y -> z == w -> f x z == f y w
cong2 f refl refl = refl

liftPath : {a : Set} {b : a -> Set} {x x' : a} ->
  x == x' -> b x == b x'
liftPath refl = refl

coerce : {l : Level} {a b : Set l} -> a == b -> a -> b
coerce refl x = x

infixr 2 _⟨_⟩_
_⟨_⟩_ : {a : Set} -> (x : a) -> { y z : a} -> x == y -> y == z -> x == z
_⟨_⟩_ x = trans

_■ : forall {a : Set} (x : a) -> x == x
_■ x = refl

data Bool : Set where
  True : Bool
  False : Bool

if_then_else : {l : Level} {a : Set l} -> Bool -> a -> a -> a
if True then t else e = t
if False then t else e = e

boolElim : {l : Level} {P : Bool -> Set l} ->
  P True -> P False -> (b : Bool) -> P b
boolElim t f True = t
boolElim t f False = f

not : Bool -> Bool
not True = False
not False = True

_||_ : Bool -> Bool -> Bool
True || _ = True
_ || True = True
False || False = False

record Pair {l l'} (a : Set l) (b : Set l') : Set (l ⊔ l') where
  constructor _,_
  field
    fst : a
    snd : b

¹_ : {a b : Set} -> Pair a b -> a
¹ (a , _) = a
²_ : {a b : Set} -> Pair a b -> b
² (_ , b) = b

_×_ : ∀ {l l'} -> Set l -> Set l' -> Set (l ⊔ l')
_×_ A B  = Pair A B

infixr 20 _×_

_∧_ : ∀ {l l'} -> Set l -> Set l' -> Set (l ⊔ l')
_∧_ A B  = Pair A B

infixr 1 _∧_

record Triple {l l' l''} (a : Set l) (b : Set l') (c : Set l'') : Set (l ⊔ l' ⊔ l'') where
  constructor _,_,_
  field
    fst : a
    snd : b
    thd : c


curry : ∀ {l : Level} {a b c : Set l} -> (Pair a b -> c) -> a -> b -> c
curry f x y = f (x , y)

uncurry :  ∀ {l l' l'' : Level} {a : Set l} {b : Set l'} {c : Set l''} -> (a -> b -> c) -> Pair a b -> c
uncurry f (x , y) = f x y

data Either {l : Level} (a b : Set l) : Set l where
  Inl : a -> Either a b
  Inr : b -> Either a b

_∨_ : {l : Level} -> Set l -> Set l -> Set l
p ∨ q = Either p q

record ⊤' (l : Level) : Set l where
  constructor tt
⊤ = ⊤' Level.zero

open import Data.Empty public

magic : ∀ {l} {a : Set l} -> ⊥ -> a
magic ()

So : Bool -> Set
So True = ⊤
So False = ⊥

intoSo : So True
intoSo = tt

open import Relation.Nullary public using
    ( ¬_
    ; Dec
    ; yes
    ; no
    )

if'_then_else : ∀ {l k} {P : Set l} {a : Set k} → Dec P → (P → a) → (¬ P → a) → a
if' yes x then t else f = t x
if' no x then t else f = f x

decideFrom : (b : Bool) -> Dec (So b)
decideFrom True = yes tt
decideFrom False = no λ z → z

open import Data.Nat public
  using
    (
    )
  renaming
    ( ℕ to Nat
    ; zero to Zero
    ; suc to Succ
    )

module NaturalLemmas where
  open Data.Nat
  succ-inj : (i j : Nat) → Succ i == Succ j → i == j
  succ-inj i .i refl = refl

  eq-Nat : (i j : Nat) → Dec (i == j)
  eq-Nat zero zero = yes refl
  eq-Nat zero (suc j) = no (λ ())
  eq-Nat (suc i) zero = no (λ ())
  eq-Nat (suc i) (suc j) with eq-Nat i j
  eq-Nat (suc i) (suc j) | yes x = yes (cong Succ x)
  eq-Nat (suc i) (suc j) | no x = no (λ z → x (succ-inj i j z))

  zero-cancellative : (n : Nat) -> (n * Zero) == Zero
  zero-cancellative Zero = refl
  zero-cancellative (Succ n) = zero-cancellative n

  plus-zero : (n : Nat) -> n == (n + Zero)
  plus-zero Zero = refl
  plus-zero (Succ n) = cong Succ (plus-zero n)

  plus-one : (n : Nat) -> n + 1 == Succ n
  plus-one Zero = refl
  plus-one (Succ n) = cong Succ (plus-one n)

  plus-succ : (x y : Nat) -> Succ (x + y) == (x + Succ y)
  plus-succ Zero y = refl
  plus-succ (Succ x) y = cong Succ (plus-succ x y)

  +-assoc : ∀ a b c → a + (b + c) == (a + b) + c
  +-assoc zero b c = refl
  +-assoc (suc a) b c = cong Succ (+-assoc a b c)

  ≤-refl : ∀ {x} → x ≤ x
  ≤-refl {zero} = z≤n
  ≤-refl {suc x} = s≤s ≤-refl
  ≤-trans : ∀ {x y z} → x ≤ y → y ≤ z → x ≤ z
  ≤-trans z≤n x₂ = z≤n
  ≤-trans (s≤s x₁) (s≤s x₂) = s≤s (≤-trans x₁ x₂)

  ≤-succ : ∀ {i j} → i ≤ j → i ≤ Succ j
  ≤-succ z≤n = z≤n
  ≤-succ (s≤s x) = s≤s (≤-succ x)

  ≤-+ : ∀ {a b} c → a ≤ b → a + c ≤ b + c
  ≤-+ {zero} {zero} c x = ≤-refl
  ≤-+ {zero} {suc b} c x = ≤-succ (≤-+ {Zero} {b} c z≤n)
  ≤-+ {suc a} {zero} c ()
  ≤-+ {suc a} {suc b} c (s≤s x) = s≤s (≤-+ c x)

  _lt_ : (i j : Nat) → Dec (i < j)
  _ lt Zero = no (λ ())
  Zero lt Succ j = yes (s≤s z≤n)
  Succ i lt Succ j with i lt j
  suc i lt suc j | yes x = yes (s≤s x)
  suc i lt suc j | no x = no (λ z → x (≤-pred z))

  antisymm : ∀ x y → x < y → y < x → ⊥
  antisymm zero y x₁ ()
  antisymm (suc x) zero () x₂
  antisymm (suc x) (suc y) (s≤s x₁) (s≤s x₂) = antisymm x y x₁ x₂

  +-succ : ∀ a b → a + Succ b == Succ (a + b)
  +-succ zero b = refl
  +-succ (suc a) b = cong Succ (+-succ a b)

  =-≤-= : ∀ {i j k l} → i == j → j ≤ k → k == l → i ≤ l
  =-≤-= refl x refl = x

  +-inj-left : ∀ a b c → a + c == b + c → a == b
  +-inj-left a b Zero pf = trans (plus-zero a) (trans pf (sym (plus-zero b)))
  +-inj-left a b (Succ c) pf = succ-inj a b (+-inj-left (Succ a) (Succ b) c (trans (sym (+-succ a c)) (trans pf (+-succ b c))))
  +-inj-right : ∀ a b c → a + b == a + c → b == c
  +-inj-right Zero b c refl = refl
  +-inj-right (Succ a) b c pf = +-inj-right a b c (succ-inj (a + b) (a + c) pf)


module NumberTheory where
  even : Nat → Bool
  even 0 = True
  even 1 = False
  even (Succ (Succ n)) = even n

  half : Nat → Nat
  half 0 = 0
  half 1 = 1
  half (Succ (Succ n)) = Succ (half n)

  double : Nat → Nat
  double 0 = 0
  double (Succ n) = Succ (Succ (double n))

  open import Data.Nat.Divisibility

  _eq_ : Nat → Nat → Bool
  Zero eq Zero = True
  Zero eq Succ b = False
  Succ a eq Zero = False
  Succ a eq Succ b = a eq b


open import Data.Integer public
  using
    (
    )
  renaming
    ( ℤ to Int
    )

infixr 5 _::_
infixr 6 _++_
data List {l : Level} (a : Set l) : Set l where
  Nil : List a
  _::_ : a -> List a -> List a
Cons : {l : Level} {a : Set l} → a → List a → List a
Cons = _::_

[_] : ∀ {a : Set} -> a -> List a
[ x ] = Cons x Nil

foldr : {a b : Set} -> (a -> b -> b) -> b -> List a -> b
foldr f e Nil = e
foldr f e (x :: xs) = f x (foldr f e xs)

_++_ : {l : Level} {a : Set l} -> List a -> List a -> List a
Nil ++ ys = ys
(x :: xs) ++ ys = Cons x (xs ++ ys)

++-nil : {l : Level} {a : Set l} (xs : List a) → xs == xs ++ Nil
++-nil Nil = refl
++-nil (x :: xs) = cong (Cons x) (++-nil xs)

when-++-is-nil : ∀ {l} {a : Set l} (ys zs : List a) → Nil == ys ++ zs → Pair (ys == Nil) (zs == Nil)
when-++-is-nil Nil Nil x = refl , refl
when-++-is-nil Nil (z :: zs) ()
when-++-is-nil (y :: ys) zs ()

++-assoc : {l : Level} {a : Set l} (xs ys zs : List a) →
  (xs ++ (ys ++ zs)) == ((xs ++ ys) ++ zs)
++-assoc Nil ys zs = refl
++-assoc (x :: xs) ys zs = cong (Cons x) (++-assoc xs ys zs)

map : {a b : Set} -> (a -> b) -> List a -> List b
map f Nil = Nil
map f (x :: xs) = Cons (f x) (map f xs)

filter : {a : Set} -> (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (x :: xs) =
  if p x then Cons x (filter p xs) else (filter p xs)

length : {a : Set} -> List a -> Nat
length Nil = Zero
length (_ :: xs) = Succ (length xs)

<=-dec : Nat -> Nat -> Bool
<=-dec Zero m = True
<=-dec (Succ n) Zero = False
<=-dec (Succ n) (Succ m) = <=-dec n m

>-dec : Nat -> Nat -> Bool
>-dec n m = not (<=-dec n m)

all : {a : Set} -> (P : a -> Bool) -> List a -> Set
all P Nil = ⊤
all P (x :: xs) = Pair (So (P x)) (all P xs)

data _∈_ {a : Set} : a -> List a -> Set where
  ∈Head : ∀ {x xs} -> x ∈ Cons x xs
  ∈Tail : ∀ {x x' xs} -> x ∈ xs -> x ∈ Cons x' xs

delete : {a : Set} {x : a} (xs : List a) -> x ∈ xs -> List a

delete (x :: ys) ∈Head = ys
delete (y :: ys) (∈Tail i) = Cons y (delete ys i)

deleteHead : {a : Set} {x : a} {xs : List a} -> delete (Cons x xs) ∈Head == xs
deleteHead = refl

delete-length : {a : Set} {x : a} {xs : List a} (i : x ∈ xs) ->
  Succ (length (delete xs i)) == length xs
delete-length ∈Head = refl
delete-length (∈Tail i) = cong Succ (delete-length i)

data Inspect {a} {A : Set a} (x : A) : Set a where
    _with-≡_ : (y : A) (eq : x == y) → Inspect x

record Sigma {l l'} (a : Set l) (b : a -> Set l') : Set (l ⊔ l') where
  constructor _,_
  field
    fst : a
    snd : b fst

uncurryΣ : {a : Set} {b : a → Set} {c : (x : a) → b x → Set} → (f : (x : a) → (y : b x) → c x y) → (s : Sigma a b) → c (Sigma.fst s) (Sigma.snd s)
uncurryΣ f (fst , snd) = f fst snd

filter' : ∀ {l} {a : Set l} {P : a → Set} → ((x : a) → Dec (P x)) → List a → List (Sigma a P)
filter' p Nil = Nil
filter' p (x :: xs) with p x
filter' p (x :: xs) | yes px = (x , px) :: filter' p xs
filter' p (x :: xs) | no np = filter' p xs

filter-shortens : ∀ {a} {P : a → Set} (p : (x : a) → Dec (P x)) (xs : List a) → length xs Data.Nat.≥ length (filter' p xs)
filter-shortens p Nil = Data.Nat.z≤n
filter-shortens p (x :: xs) with p x
filter-shortens p (x :: xs) | yes x₁ = Data.Nat.s≤s (filter-shortens p xs)
filter-shortens p (x :: xs) | no x₁ = NaturalLemmas.≤-succ (filter-shortens p xs)

-- Constant function for Set
K : {a : Set} -> Set -> (a -> Set)
K b = \_ -> b

data Id (a : Set) : Set where
  In : a -> Id a
out : {a : Set} -> Id a -> a
out (In x) = x

sum : ∀ {l} {a : Set l} (f : a → Nat) (xs : List a) → Nat
sum f Nil = 0
sum f (x :: xs) = f x Data.Nat.+ sum f xs
