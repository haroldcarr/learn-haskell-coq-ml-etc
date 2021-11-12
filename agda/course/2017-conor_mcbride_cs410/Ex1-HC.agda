{-# OPTIONS --allow-unsolved-metas #-}
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- CS410 2017/18 Exercise 1  VECTORS AND FRIENDS (worth 25%)
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- NOTE (19/9/17) This file is currently incomplete: more will arrive on
-- GitHub.

-- MARK SCHEME (transcribed from paper): the (m) numbers add up to slightly
-- more than 25, so should be taken as the maximum number of marks losable on
-- the exercise. In fact, I did mark it negatively, but mostly because it was
-- done so well (with Agda's help) that it was easier to find the errors.


------------------------------------------------------------------------------
-- Dependencies
------------------------------------------------------------------------------

open import CS410-Prelude

cong : ∀ {A B : Set} {x y : A}
     → (f : A → B)
     →   x ==   y
       ----------
     → f x == f y
cong f (refl x) =  refl (f x)

------------------------------------------------------------------------------
-- Vectors
------------------------------------------------------------------------------

data Vec (X : Set) : Nat -> Set where  -- like lists, but length-indexed
  []   :                              Vec X zero
  _,-_ : {n : Nat} -> X -> Vec X n -> Vec X (suc n)
infixr 4 _,-_   -- the "cons" operator associates to the right

-- I like to use the asymmetric ,- to remind myself that the element is to
-- the left and the rest of the list is to the right.

-- Vectors are useful when there are important length-related safety
-- properties.

------------------------------------------------------------------------------
-- Heads and Tails
------------------------------------------------------------------------------

-- We can rule out nasty head and tail errors by insisting on nonemptiness!

--??--1.1-(2)-----------------------------------------------------------------

vHead : {X : Set}{n : Nat} -> Vec X (suc n) -> X
vHead (x ,- _) = x

vTail : {X : Set}{n : Nat} -> Vec X (suc n) -> Vec X n
vTail (_ ,- xs) = xs

vHeadTailFact :  {X : Set}{n : Nat}(xs : Vec X (suc n)) ->
                 (vHead xs ,- vTail xs) == xs
vHeadTailFact (x ,- xs) = refl (x ,- xs)

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Concatenation and its Inverse
------------------------------------------------------------------------------

--??--1.2-(2)-----------------------------------------------------------------

_+V_ : {X : Set}{m n : Nat} -> Vec X m -> Vec X n -> Vec X (m +N n)
[]        +V ys = ys
(x ,- xs) +V ys = x ,- xs +V ys
infixr 4 _+V_

vChop : {X : Set}(m : Nat){n : Nat} -> Vec X (m +N n) -> Vec X m * Vec X n
vChop  zero         xs = [] , xs
vChop (suc m) (x ,- xs)
  with vChop m xs
... | vm , vn
  = (x ,- vm) , vn

vChopAppendFact : {X : Set}{m n : Nat}(xs : Vec X m)(ys : Vec X n) ->
                  vChop m (xs +V ys) == (xs , ys)
vChopAppendFact       []  ys = refl ([] , ys)
vChopAppendFact (x ,- xs) ys rewrite vChopAppendFact xs ys = refl ((x ,- xs) , ys)

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Map, take I
------------------------------------------------------------------------------

-- Implement the higher-order function that takes an operation on
-- elements and does it to each element of a vector. Use recursion
-- on the vector.
-- Note that the type tells you the size remains the same.

-- Show that if the elementwise function "does nothing", neither does
-- its vMap. "map of identity is identity"

-- Show that two vMaps in a row can be collapsed to just one, or
-- "composition of maps is map of compositions"

--??--1.3-(2)-----------------------------------------------------------------

vMap : {X Y : Set} -> (X -> Y) -> {n : Nat} -> Vec X n -> Vec Y n
vMap f       []  = []
vMap f (x ,- xs) = f x ,- vMap f xs

vMapIdFact : {X : Set}{f : X -> X}(feq : (x : X) -> f x == x) ->
             {n : Nat}(xs : Vec X n) -> vMap f xs == xs
vMapIdFact feq       []  = refl []
vMapIdFact feq (x ,- xs)
  rewrite vMapIdFact feq xs
  |       feq x
  = refl (x ,- xs)

vMapCpFact : {X Y Z : Set}{f : Y -> Z}{g : X -> Y}{h : X -> Z}
               (heq : (x : X) -> f (g x) == h x) ->
             {n : Nat}(xs : Vec X n) ->
               vMap f (vMap g xs) == vMap h xs
vMapCpFact heq       []  = refl []
vMapCpFact {_}{_}{_}{f} {g} {h} heq (x ,- xs)
  rewrite heq x
  |       vMapCpFact {_}{_}{_}{f} {g} {h} heq xs
  = refl (h x ,- vMap h xs)

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- vMap and +V
------------------------------------------------------------------------------

-- Show that if you've got two vectors of Xs and a function from X to Y,
-- and you want to concatenate and map, it doesn't matter which you do
-- first.

--??--1.4-(1)-----------------------------------------------------------------

vMap+VFact : {X Y : Set}(f : X -> Y) ->
             {m n : Nat}(xs : Vec X m)(xs' : Vec X n) ->
             vMap f (xs +V xs') == (vMap f xs +V vMap f xs')
vMap+VFact f       []  xs' = refl (vMap f xs')
vMap+VFact f (x ,- xs) xs'
  rewrite vMap+VFact f xs xs'
  = refl (f x ,- vMap f xs +V vMap f xs')

--??--------------------------------------------------------------------------

-- Think about what you could prove, relating vMap with vHead, vTail, vChop...
-- Now google "Philip Wadler" "Theorems for Free"

-- TODO

------------------------------------------------------------------------------
-- Applicative Structure (giving mapping and zipping cheaply)
------------------------------------------------------------------------------

--??--1.5-(2)-----------------------------------------------------------------

-- HINT: you will need to override the default invisibility of n to do this.
-- HC : replicate
vPure : {X : Set} -> X -> {n : Nat} -> Vec X n
vPure x {zero}  = []
vPure x {suc n} = x ,- vPure x {n}

_$V_ : {X Y : Set}{n : Nat} -> Vec (X -> Y) n -> Vec X n -> Vec Y n
[]      $V      [] = []
f ,- fs $V x ,- xs = f x ,- (fs $V xs)
infixl 3 _$V_  -- "Application associates to the left,
               --  rather as we all did in the sixties." (Roger Hindley)

-- Pattern matching and recursion are forbidden for the next two tasks.

-- implement vMap again, but as a one-liner
vec : {X Y : Set} -> (X -> Y) -> {n : Nat} -> Vec X n -> Vec Y n
vec f xs = vPure f $V xs

-- implement the operation which pairs up corresponding elements
vZip : {X Y : Set}{n : Nat} -> Vec X n -> Vec Y n -> Vec (X * Y) n
vZip xs ys = vec (_,_) xs $V ys

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Applicative Laws
------------------------------------------------------------------------------

-- According to "Applicative programming with effects" by
--   Conor McBride and Ross Paterson
-- some laws should hold for applicative functors.
-- Check that this is the case.

--??--1.6-(2)-----------------------------------------------------------------

vIdentity : {X : Set}{f : X -> X}(feq : (x : X) -> f x == x) ->
            {n : Nat}(xs : Vec X n) -> (vPure f $V xs) == xs
vIdentity feq        [] = refl []
vIdentity feq (x ,- xs) rewrite vIdentity feq xs | feq x = refl (x ,- xs)

vHomomorphism : {X Y : Set}(f : X -> Y)(x : X) ->
                {n : Nat} -> (vPure f $V vPure x) == vPure (f x) {n}
vHomomorphism f x {zero}  = refl []
vHomomorphism f x {suc n} rewrite vHomomorphism f x {n} = refl (f x ,- vPure (f x))

vInterchange : {X Y : Set}{n : Nat}(fs : Vec (X -> Y) n)(x : X) ->
               (fs $V vPure x) == (vPure (_$ x) $V fs)
vInterchange []        x = refl []
vInterchange (f ,- fs) x rewrite vInterchange fs x = refl (f x ,- (vPure (λ x→y → x→y x) $V fs))

vComposition : {X Y Z : Set}{n : Nat}
               (fs : Vec (Y -> Z) n)(gs : Vec (X -> Y) n)(xs : Vec X n) ->
               (vPure _<<_ $V fs $V gs $V xs) == (fs $V (gs $V xs))
vComposition       []        []        []  = refl []
vComposition (f ,- fs) (g ,- gs) (x ,- xs)
  rewrite vComposition fs gs xs
  = refl (f (g x) ,- (fs $V (gs $V xs)))

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Order-Preserving Embeddings (also known in the business as "thinnings")
------------------------------------------------------------------------------

-- What have these to do with Pascal's Triangle?

-- how to choose N things from M things
data _<=_ : Nat -> Nat -> Set where
  oz :                          zero  <= zero  -- stop
  os : {n m : Nat} -> n <= m -> suc n <= suc m -- take this one and keep going
  o' : {n m : Nat} -> n <= m ->     n <= suc m -- skip this one and keep going

refl-<= : (n : Nat) -> n <= n
refl-<= zero    = oz
refl-<= (suc n) = os (refl-<= n)

trans-<= : {n m p : Nat} -> n <= m -> m <= p -> n <= p
trans-<=  oz           zero<=p  = zero<=p
trans-<= (os n<=m) (os sucm<=p) = os (trans-<=     n<=m  sucm<=p)
trans-<= (os n<=m) (o' sucm<=p) = os (trans-<= (o' n<=m) sucm<=p)
trans-<= (o' n<=m) (os    m<=p) = o' (trans-<=     n<=m     m<=p)
trans-<= (o' n<=m) (o'    m<=p) = o' (trans-<= (o' n<=m)    m<=p)

<=-suc : (x : Nat) -> x <= suc x
<=-suc  zero   = o' oz
<=-suc (suc x) = os (<=-suc x)

n<=m→sucn<=sucm : {n m : Nat} → n <= m -> suc n <= suc m
n<=m→sucn<=sucm  oz       = os oz
n<=m→sucn<=sucm (os n<=m) = os (n<=m→sucn<=sucm n<=m)
n<=m→sucn<=sucm (o' n<=m) = o' (n<=m→sucn<=sucm n<=m)

sucsucn<=m→sucn<=m : {n m : Nat} -> suc (suc n) <= m -> suc n <= m
sucsucn<=m→sucn<=m (os sucn<=m)  = o' sucn<=m
sucsucn<=m→sucn<=m (o' (os xxx)) = o' (o' xxx)
sucsucn<=m→sucn<=m (o' (o' xxx)) = o' (o' (sucsucn<=m→sucn<=m xxx))

sucn<=m→sucn<=sucm : {n m : Nat} -> suc n <= m -> suc n <= suc m
sucn<=m→sucn<=sucm (os p) = os (o' p)
sucn<=m→sucn<=sucm (o' p) = o' (sucn<=m→sucn<=sucm p)

zero<=m : {m : Nat} -> zero <= m
zero<=m  {zero} = oz
zero<=m {suc m} = o' zero<=m

sucn<=sucm→n<=m : {n m : Nat} -> suc n <= suc m -> n <= m
sucn<=sucm→n<=m {zero}  {zero}       p  = oz
sucn<=sucm→n<=m {zero}  {suc m} (os  p) = o' (zero<=m {m})
sucn<=sucm→n<=m {zero}  {suc m} (o'  p) = o' (zero<=m {m})
sucn<=sucm→n<=m {suc n} {zero}  (os ())
sucn<=sucm→n<=m {suc n} {zero}  (o' ())
sucn<=sucm→n<=m {suc n} {suc m} (os  p) = p
sucn<=sucm→n<=m {suc n} {suc m} (o'  p) = o' (sucn<=sucm→n<=m p)

-- Find all the values in each of the following <= types.
-- This is a good opportunity to learn to use C-c C-a with the -l option
--   (a.k.a. "google the type" without "I feel lucky")
-- The -s n option also helps.

--??--1.7-(1)-----------------------------------------------------------------

all0<=4 : Vec (0 <= 4) 1
all0<=4 = o' (o' (o' (o' oz))) ,- []

all1<=4 : Vec (1 <= 4) 1
all1<=4 = os (o' (o' (o' oz))) ,- []

all2<=4 : Vec (2 <= 4) 1
all2<=4 = os (os (o' (o' oz))) ,- []

all3<=4 : Vec (3 <= 4) 1
all3<=4 = os (os (os (o' oz))) ,- []

all4<=4 : Vec (4 <= 4) 1
all4<=4 = os (os (os (os oz))) ,- []

-- Prove the following. A massive case analysis "rant" is fine.

no5<=4 : 5 <= 4 -> Zero
no5<=4 (os (os (os (os ()))))
no5<=4 (os (os (os (o' ()))))
no5<=4 (os (os (o' (os ()))))
no5<=4 (os (os (o' (o' ()))))
no5<=4 (os (o' (os (os ()))))
no5<=4 (os (o' (os (o' ()))))
no5<=4 (os (o' (o' (os ()))))
no5<=4 (os (o' (o' (o' ()))))
no5<=4 (o' (os (os (os ()))))
no5<=4 (o' (os (os (o' ()))))
no5<=4 (o' (os (o' (os ()))))
no5<=4 (o' (os (o' (o' ()))))
no5<=4 (o' (o' (os (os ()))))
no5<=4 (o' (o' (os (o' ()))))
no5<=4 (o' (o' (o' (os ()))))
no5<=4 (o' (o' (o' (o' ()))))

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Order-Preserving Embeddings Select From Vectors
------------------------------------------------------------------------------

-- Use n <= m to encode the choice of n elements from an m-Vector.
-- The os constructor tells you to take the next element of the vector;
-- the o' constructor tells you to omit the next element of the vector.

--??--1.8-(2)-----------------------------------------------------------------

_<?=_ : {X : Set}{n m : Nat} -> n <= m -> Vec X m
                     -> Vec X n
oz    <?=        _  = []
os th <?= (x ,- xs) = x ,- th <?= xs
o' th <?= (_ ,- xs) =      th <?= xs

vn4 : Vec Nat 4
vn4 = os (os (os (os oz))) <?= (1 ,- 2 ,- 3 ,- 4 ,- [])
_ :                     vn4 == (1 ,- 2 ,- 3 ,- 4 ,- [])
_ =                       refl (1 ,- 2 ,- 3 ,- 4 ,- [])

vn3 : Vec Nat 3
vn3 = o' (os (o' (os (o' (os oz))))) <?= (1 ,- 2 ,- 3 ,- 4 ,- 5 ,- 6 ,- [])
_ :                               vn3 ==      (2      ,- 4      ,- 6 ,- [])
_ =                                      refl (2      ,- 4      ,- 6 ,- [])

vn2 : Vec Nat 2
vn2 = os (o' (os (o' oz))) <?= (1 ,- 2 ,- 3 ,- 4 ,- [])
_ :                     vn2 == (1      ,- 3      ,- [])
_ =                       refl (1      ,- 3      ,- [])

vn2' : Vec Nat 2
vn2' = o' (os (os (o' oz))) <?= (1 ,- 2 ,- 3 ,- 4 ,- [])
_ :                          vn2' == (2 ,- 3      ,- [])
_ =                             refl (2 ,- 3      ,- [])

-- it shouldn't matter whether you map then select or select then map

vMap<?=Fact : {X Y : Set}(f : X -> Y)
              {n m : Nat}(th : n <= m)(xs : Vec X m) ->
              vMap f (th <?= xs) == (th <?= vMap f xs)
vMap<?=Fact f  oz     [] = refl []
vMap<?=Fact f (os th) (x ,- xs) rewrite vMap<?=Fact f th xs = refl (f x ,- (th <?= vMap f xs))
vMap<?=Fact f (o' th) (x ,- xs) rewrite vMap<?=Fact f th xs = refl         (th <?= vMap f xs)

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Our Favourite Thinnings
------------------------------------------------------------------------------

-- Construct the identity thinning and the empty thinning.

--??--1.9-(1)-----------------------------------------------------------------

oi : {n : Nat} -> n <= n
oi  {zero} = oz
oi {suc n} = os oi

oe : {n : Nat} -> 0 <= n
oe  {zero} = oz
oe {suc n} = o' oe

vnoi : Vec Nat 4
vnoi = oi  <?= (1 ,- 2 ,- 3 ,- 4 ,- [])
_    : vnoi == (1 ,- 2 ,- 3 ,- 4 ,- [])
_    =    refl (1 ,- 2 ,- 3 ,- 4 ,- [])

vnoe : Vec Nat 0
vnoe = oe <?= (1 ,- 2 ,- 3 ,- 4 ,- [])
_    :                     vnoe == []
_    =                        refl []


--??--------------------------------------------------------------------------


-- Show that all empty thinnings are equal to yours.

--??--1.10-(1)----------------------------------------------------------------

oeUnique : {n : Nat}(th : 0 <= n) -> th == oe
oeUnique  oz                       = refl  oz
oeUnique (o' i) rewrite oeUnique i = refl (o' oe)

--??--------------------------------------------------------------------------


-- Show that there are no thinnings of form big <= small  (TRICKY)
-- Then show that all the identity thinnings are equal to yours.
-- Note that you can try the second even if you haven't finished the first.
-- HINT: you WILL need to expose the invisible numbers.
-- HINT: check CS410-Prelude for a reminder of >=

--??--1.11-(3)----------------------------------------------------------------

oTooBig : {n m : Nat} -> n >= m -> suc n <= m -> Zero
oTooBig  {zero}  {zero} n>=m ()
oTooBig  {zero} {suc m} n>=m (os th) = n>=m
oTooBig  {zero} {suc m} n>=m (o' th) = n>=m
oTooBig {suc n} {suc m} n>=m (os th) = oTooBig n>=m th
oTooBig {suc n} {suc m} n>=m (o' th) = oTooBig {n} {m} n>=m (sucsucn<=m→sucn<=m th)

oiUnique : {n : Nat}(n<=n : n <= n) -> n<=n == oi
oiUnique          oz                             = refl oz
oiUnique         (os n<=n) rewrite oiUnique n<=n = refl (os oi)
oiUnique {suc n} (o' sucn<=n)
  with oTooBig (refl->= n) sucn<=n
... | ()

--??--------------------------------------------------------------------------


-- Show that the identity thinning selects the whole vector

--??--1.12-(1)----------------------------------------------------------------

id-<?= : {X : Set}{n : Nat}(xs : Vec X n) -> (oi <?= xs) == xs
id-<?=       []  = refl []
id-<?= (x ,- xs) = cong (x ,-_) (id-<?= xs)

--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Composition of Thinnings
------------------------------------------------------------------------------

-- Define the composition of thinnings and show that selecting by a
-- composite thinning is like selecting then selecting again.
-- A small bonus applies to minimizing the length of the proof.
-- To collect the bonus, you will need to think carefully about
-- how to make the composition as *lazy* as possible.

--??--1.13-(3)----------------------------------------------------------------

-- my 1st attempt - but everything gets stuff because pattern matching on right instead of left
_o>>1_ : {p n m : Nat} -> p <= n -> n <= m -> p <= m
p<=zero o>>1 oz      = p<=zero
p<=sucn o>>1 os n<=m = trans-<= p<=sucn (n<=m→sucn<=sucm n<=m)
p<=n    o>>1 o' n<=m = trans-<= p<=n                 (o' n<=m)

-- 2nd attempt - but things further down get stuck
_o>>2_ : {p n m : Nat} -> p <= n -> n <= m -> p <= m
oz       o>>2 oz          = oz
os n<=m₁ o>>2 os    m₁<=m = os     (n<=m₁ o>>2    m₁<=m)
os n<=m₁ o>>2 o' sucm₁<=m = os (o'  n<=m₁ o>>2 sucm₁<=m)
o' p<=m₁ o>>2 os    m₁<=m =     o' (p<=m₁ o>>2    m₁<=m)
o' p<=m₁ o>>2 o' sucm₁<=m = o' (o'  p<=m₁ o>>2 sucm₁<=m)
oz       o>>2 o'  zero<=m = o'        (oz o>>2  zero<=m)

-- https://github.com/laMudri/thinning/blob/master/src/Data/Thinning.agda
_o>>_ : ∀ {m n o} → m <= n → n <= o → m <= o
m<=z o>> oz   = m<=z
os θ o>> os φ = os (θ o>> φ)
o' θ o>> os φ = o' (θ o>> φ)
θ    o>> o' φ = o' (θ o>> φ)


-- empty thinning returns an empty vector
oe-<?= : {X : Set}{n : Nat}(xs : Vec X n) -> (oe <?= xs) == []
oe-<?=       []  = refl []
oe-<?= (x ,- xs) = oe-<?= xs

cp-<?= : {p n m : Nat}(p<=n : p <= n)(n<=m : n <= m) ->
         {X : Set}(xs : Vec X m) ->
         ((p<=n o>> n<=m) <?= xs) == (p<=n <?= (n<=m <?= xs))
cp-<?=     p<=n   oz             []  = refl (p<=n <?= [])
cp-<?=     p<=n  (o' n<=m) (_ ,- xs) = cp-<?= p<=n n<=m xs
cp-<?= (o' p<=n) (os n<=m) (_ ,- xs) = cp-<?= p<=n n<=m xs
cp-<?= (os p<=n) (os n<=m) (x ,- xs) = cong (x ,-_) (cp-<?= p<=n n<=m xs)


zero<=4 : zero <= 4
zero<=4 = o' (o' (o' (o' oz)))

1<=4    : 1    <= 4
1<=4    = os (o' (o' (o' oz)))

v04     : Vec Nat zero
v04     = zero<=4 <?= (1 ,- 2 ,- 3 ,- 4 ,- [])
_       :                           v04 == []
_       =                             refl []

v14     : Vec Nat 1
v14     =    1<=4 <?= (1 ,- 2 ,- 3 ,- 4 ,- [])
_       :      v14 == (1                ,- [])
_       =        refl (1                ,- [])


--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Thinning Dominoes
------------------------------------------------------------------------------

--??--1.14-(3)----------------------------------------------------------------

idThen-o>> : {n m : Nat}(n<=m : n <= m) -> (oi o>> n<=m) == n<=m
idThen-o>>  oz                               = refl oz
idThen-o>> (os n<=m) = cong os (idThen-o>> n<=m) -- rewrite idThen-o>> n<=m = refl (os n<=m)
idThen-o>> (o' n<=m) = cong o' (idThen-o>> n<=m)

idAfter-o>> : {n m : Nat}(n<=m : n <= m) -> (n<=m o>> oi) == n<=m
idAfter-o>>  oz       = refl oz
idAfter-o>> (os n<=m) = cong os (idAfter-o>> n<=m) -- rewrite idAfter-o>> n<=m = refl (os n<=m)
idAfter-o>> (o' n<=m) = cong o' (idAfter-o>> n<=m) -- rewrite idAfter-o>> n<=m = refl (o' n<=m)

assoc-o>> : {q p n m : Nat}(q<=p : q <= p)(p<=n : p <= n)(n<=m : n <= m) ->
            ((q<=p o>> p<=n) o>> n<=m) == (q<=p o>> (p<=n o>> n<=m))
assoc-o>>     q<=p      p<=n   oz       = refl (q<=p o>> p<=n)
assoc-o>>     q<=p      p<=n  (o' n<=m) = cong o' (assoc-o>> q<=p p<=n n<=m)
assoc-o>>     q<=p  (o' p<=n) (os n<=m) = cong o' (assoc-o>> q<=p p<=n n<=m)
assoc-o>> (o' q<=p) (os p<=n) (os n<=m) = cong o' (assoc-o>> q<=p p<=n n<=m)
assoc-o>> (os q<=p) (os p<=n) (os n<=m) = cong os (assoc-o>> q<=p p<=n n<=m)


--??--------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Vectors as Arrays
------------------------------------------------------------------------------

-- We can use 1 <= n as the type of bounded indices into a vector and do
-- a kind of "array projection". First we select a 1-element vector from
-- the n-element vector, then we take its head to get the element out.

vProject : {n : Nat}{X : Set} -> Vec X n -> 1 <= n -> X
vProject xs i = vHead (i <?= xs)

vp3 : Nat
vp3 = vProject (1 ,- 2 ,- 3 ,- 4 ,- []) (o' (o' (os (o' oz))))
_ :                vp3 == 3
_ =                  refl 3


-- Your (TRICKY) mission is to reverse the process, tabulating a function
-- from indices as a vector. Then show that these operations are inverses.

--??--1.15-(3)----------------------------------------------------------------

-- HINT: composition of functions
vTabulate : {n : Nat}{X : Set} -> (1 <= n -> X) -> Vec X n
vTabulate  {zero} _ = []
vTabulate {suc n} f = f (os (zero<=m {n})) ,- (vTabulate (λ 1<=n → f (o' 1<=n)))

vt : Vec Nat 4
vt = vTabulate f
 where
  f : (1 <= 4) → Nat
  f             (os _)    = 1
  f         (o' (os _))   = 2
  f     (o' (o' (os _)))  = 3
  f (o' (o' (o' (os _)))) = 4
_ : vt == (1 ,- 2 ,- 3 ,- 4 ,- [])
_ =  refl (1 ,- 2 ,- 3 ,- 4 ,- [])

-- This should be easy if vTabulate is correct.
vTabulateProjections : {n : Nat}{X : Set}(xs : Vec X n) ->
                       vTabulate (vProject xs) == xs
vTabulateProjections [] = refl []
vTabulateProjections (x ,- xs) = cong (x ,-_) (vTabulateProjections xs)

-- HINT: oeUnique
vProjectFromTable : {n : Nat}{X : Set}(f : 1 <= n -> X)(i : 1 <= n) ->
                    vProject (vTabulate f) i == f i
vProjectFromTable f i = {!!}

--??--------------------------------------------------------------------------
