{-
Created       : 2012 Jan 11 (Wed) 20:25:14 by carr.
Last Modified : 2012 Jan 20 (Fri) 18:16:38 by carr.
-}

-- ----------------------------------------------------------------------
-- Chapter 1

-- A number d divides n if there is a natural number a with a*d=n.
--    i.e., d divides n if there is a natural number a with n/d=a
--          i.e., division of n by d leaves no remainder.
divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

-- LD(n) is the least natural number greater than 1 that divides n.
-- Note: LD(n) exists for every natural number n > 1,
--       since the natural number d = n is greater than 1 and divides n.
--       Therefore, the set of divisors of n that are greater than 1 is non-empty.
--       Thus, the set will have a least element.
ld :: Integral a => a -> a
ld n = ldf 2 n

-- Proposition 1.2
-- 1. If n > 1 then LD(n) is a prime number.
--   Proof (by contradiction)
--     Suppose, c = LD(n) is not a prime.
--     Then there are natural numbers a and b with c=a*b, and 1<a<c.
--     But then a divides n, a contradiction
--     Thus, LD(n) must be a prime number.
-- 2. If n > 1 and n is not a prime number, then LD(n)^2 <= n.
--   Proof
--     Suppose n > 1, n is not a prime and that p=LD(n).
--     Then there is a natural number a>1 with n=p*a.
--     Thus, a divides n.
--     Since p is the smallest divisor of n with p > 1,
--     we have that p<=a, and therefore p^2<=p*a=n, i.e., LD(n)^2 <= n.
-- Therefore ldf looks for a prime divisor of n by checking k|n for all k, 2<=k<=sqrt(n)

ldf :: Integral a => a -> a -> a
ldf k n | k `divides` n = k  -- covers even numbers on first call then smallest prime divisor on iterations
        | k^2 > n       = n  -- covers a "new" prime
        | otherwise     = ldf (k+1) n

-- Exercise 1.4
-- if ldf has k^2>=n instead, does it matter?
-- no, because k divides n so ">=" would never be reached

prime0 :: Integral a => a -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

-- Example 1.8/Exercise 1.9
min' :: Ord t => [t] -> t
min' l = mnmx (min) l
max' :: Ord t => [t] -> t
max' l = mnmx (max) l
mnmx :: (t -> t -> t) -> [t] -> t
mnmx f []     = error "empty list"
mnmx f [x]    = x
mnmx f (x:xs) = f x (mnmx f xs)

-- Exercise 1.10

removeFst :: Eq a => a -> [a] -> [a]
removeFst m []                 = []
removeFst m (x:xs) | m == x    = xs
                   | otherwise = x : removeFst m xs
-- removeFst 4 [1,2,3,4,5,6,4,8]

-- Example 1.11
-- Sort a list of integers in order of increasing size
-- If a list is non-empty,
-- then put its minimum in front
-- of the result of sorting the list that results from removing its minimum.
srt :: Ord a => [a] -> [a]
srt [] = []
srt xs = m : srt (removeFst m xs) where m = min' xs
-- srt [4,8,1,8,3,5,7,4,0,1,3,4,5,6,8,1]

-- Example 1.12

average :: (Integral a1, Fractional a) => [a1] -> a
average [] = error "empty list"
average xs = fromIntegral (sum' xs) / fromIntegral (length xs)
-- average [0,1,2,3,4,5,6,7,8,9]

sum' [] = 0
sum' (x:xs) = x + sum' xs

length' [] = 0
length' (x:xs) = 1 + length' xs

fld :: (t1 -> t -> t1) -> t1 -> [t] -> t1
fld f v l = fld' f v l
  where fld' f acc []     = acc
        fld' f acc (x:xs) = fld' f (f acc x) xs

-- fld   (\acc x -> acc + x) 0 [2,3,4,5]
-- fld   (\acc x -> acc + 1) 0 [2,3,4,5]
-- foldl (\acc x -> acc + x) 0 [2,3,4,5]
-- foldl (\acc x -> acc + 1) 0 [2,3,4,5]

-- Exercise 1.13

countChar :: (Num a, Eq a1) => a1 -> [a1] -> a
countChar c = foldl (\acc x -> if x == c then acc + 1 else acc) 0
-- countChar 'r' "harold carr"

-- Exercise 1.14

-- repeat letters by the number of the position (counting from 1)
blowup :: String -> String
blowup s = let (_,r) = foldl (\(n,r) x -> (n+1, r++(replicate n x))) (1,"") s in r
-- blowup "bang!"

-- Exercise 1.15
-- already done, just use srt from Example 1.11
-- srt ["harold", "carr"]

-- Example 1.16

-- PRO: stops search on first fail (assuming left-to-right eval order of &&)
-- CON: this one is not tail-recursive
prefix []     ys     = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys
-- map (\(l,r) -> prefix l r) [("foo","fo"), ("foo","foo"), ("foo","foobar"), ("bar","foobar")]

-- PRO: tail recursive
-- CON: will go through the entire shortest list (a or b)
prefix' a b = prefix'' True a b
    where prefix'' acc []     ys     = acc
          prefix'' acc (x:xs) []     = False
          prefix'' acc (x:xs) (y:ys) = prefix'' ((x==y) && acc) xs ys
-- map (\(l,r) -> prefix' l r) [("foo","fo"), ("foo","foo"), ("foo","foobar"), ("bar","foobar")]

-- Exercise 1.17
substring x []         = False
substring x all@(y:ys) = if prefix x all then True else substring x ys
-- substring "aro" "harold"
-- substring "ld" "harold"
-- substring "el" "harold"

-- 1.7 Prime Factorization

{-
             n0=84
n0/=1  p1=2  n1=84/2=42
n1/=1  p2=2  n2=42/2=21
n2/=1  p3=3  n3=21/3= 7
n3/=1  p4=7  n4= 7/7= 1
n4 =1
-}
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n
-- factors 84
-- factors 557940830126698960967415390

-- 1.9 map/filter

map' :: (t -> a) -> [t] -> [a]
map' f []     = []
map' f (x:xs) = (f x) : (map' f xs)

-- Exercise 1.20-21
-- sum' $ map' length [[1],[1,2],[1,2,3]]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []                 = []
filter' p (x:xs) | p x       = x : filter' p xs
                 | otherwise = filter' p xs
-- filter (>3) [1..10]

-- Example 1.22
primes0 = filter prime0 [2..]


-- Example 1.23
-- more efficient Least Divisor
-- Previous ldf looks for a prime divisor of n by checking k|n for all k, 2<=k<=sqrt(n)
-- But better to only check p|n for primes p with 2<=p<=sqrt(n)
-- since other numbers are "composites" of primes already checked

ldp n = ldpf primes1 n

ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

-- To define primes1 above we need a test for primality.
-- but the test is defined in terms of the function LD,
-- which refers to primes1 (i.e. a cycle).
-- Break cycle by avoiding primality test for 2.
-- Make it given that 2 is prime.
-- Then we can use the primality of 2 in LD to check that 3 is prime, etc.

primes1 = 2 : filter prime [3..]

prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n

-- Exercise 1.24 - point free style

ldp'' = ldpf primes1

-- ----------------------------------------------------------------------
-- Chapter 2

-- Implication (if/then)

{-
Truth table of => is surprising.
To motivate:

  for every natural number n, 5<n => 3<n.

Therefore, implication must hold (be true) for n = 2, 4 and 6.
- both antecedent and consequent are false (n = 2)  (f f => t)
- antecedent false, consequent true (n = 4)         (f t => t)
- both antecedent and consequent true (n = 6)       (t t => t)
False only
- when antecedent true and consequent false         (t f => f)
  (which never happens above).

But it means anything with false antecedent is trivially true.

- introduce infix ==>
- number 1 declaration indicates binding power (0 lowest, 9 highest)
- declaration of an infix operator with it binding power is called a fixity declaration
-}

infix 1 ==>
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> x = True


-- Necessary and Sufficient Conditions

{-
P is called a sufficient condition for Q and
Q a necessary condition for P
if the implication P => Q holds

expressed various ways:
- if P,then Q
- Q if P
- P only if Q
- Q whenever P
- P is sufficient for Q
- Q is necessary for P
-}

-- Equivalence (iff)

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y


-- Example 2.3 proving IFF
{-
To prove something of the form P iff Q
- split into two parts
- "only if" part: P==>Q
- "if" part: Q==>P
-}

-- Exercise 2.4 - exclusive OR

infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

-- exclusive OR is NOT equivalent to "not (P<=>Q)"
e24 = map (\(p,q) -> let x = (p <=> q)
                         y = (p <+> q)
                     in (x,y,x==y))
          [(False,False), (False,True), (True,False), (True,True)]

-- 2.2 Logical Validity ...

-- logically valid: true for any values of terms

valid1 :: (Bool -> Bool) -> Bool
valid1 f = (f True) && (f False)

excluded_middle :: Bool -> Bool
excluded_middle p = p || not p

-- valid1 excluded_middle

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 f =    (f True True)
           && (f True False)
           && (f False True)
           && (f False False)

-- using list comprehensions

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 f = and [ f p q r | p <- [True,False],
                           q <- [True,False],
                           r <- [True,False]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 f = and [ f p q r s | p <- [True,False],
                             q <- [True,False],
                             r <- [True,False],
                             s <- [True,False]]

-- Logically Equivalent
-- Two formulas are logically equivalent if, for all truth values of
-- P, Q, . . . , the truth values obtained for them are the same.

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 f1 f2 = (f1 True <=> f2 True) && (f1 False <=> f2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 f1 f2 = and [(f1 p q) <=> (f2 p q) | p <- [True,False],
                                               q <- [True,False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 f1 f2 = and [(f1 p q r) <=> (f2 p q r) | p <- [True,False],
                                                   q <- [True,False],
                                                   r <- [True,False]]

-- TODO p 44
-- Exercise 2.9



