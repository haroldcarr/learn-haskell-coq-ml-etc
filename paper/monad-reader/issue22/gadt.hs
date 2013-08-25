
{-# LANGUAGE GADTs, ExistentialQuantification, DataKinds, TypeFamilies, TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}

import Data.Char

data Test a = TI Int | TS String a

data Test' a where
  TI' :: Int         -> Test' a
  TS' :: String -> a -> Test' a

data Test'' a where
  TI'' :: Int         -> Test'' Int  -- locks down return type HERE
  TS'' :: String -> a -> Test'' a

f :: Test'' a -> a
f (TI'' i) = i + 10

data IntExpr' = IntVal' Int
              | AddInt' IntExpr' IntExpr'

evaluate' :: IntExpr' -> Int
evaluate' e = case e of
    IntVal' i     -> i
    AddInt' e1 e2 -> evaluate' e1 + evaluate' e2

data ExtExpr'' = IntVal''  Int
               | BoolVal'' Bool
               | AddInt''  ExtExpr'' ExtExpr''
               | IsZero''  ExtExpr''

evaluate'' :: ExtExpr'' -> Maybe (Either Int Bool)
evaluate'' e = case e of
    AddInt'' e1 e2 -> case (evaluate'' e1, evaluate'' e2) of
                        (Just (Left i1), Just (Left  i2)) -> Just $ Left $ i1 + i2
                        (Just (Left i1), Just (Right b2)) -> error "wrong type given to AddInt''" -- dynamic type-checking
                        _                                 -> error "not implemented"
    IntVal''  i    -> Just (Left  i)
    BoolVal'' b    -> Just (Right b)
    _              -> error "not implemented"

data PhantomExpr''' t = IntVal'''  Int
                      | BoolVal''' Bool
                      | AddInt'''  (PhantomExpr''' Int) (PhantomExpr''' Int)
                      | IsZero'''  (PhantomExpr''' Int)

intVal'''  :: Int                -> PhantomExpr''' Int
intVal'''   = IntVal'''
boolVal''' :: Bool               -> PhantomExpr''' Bool
boolVal'''  = BoolVal'''
isZero''   :: PhantomExpr''' Int -> PhantomExpr''' Bool
isZero''    = IsZero'''

data PhantomExpr'''' t where
    IntVal''''  :: Int                                        -> PhantomExpr'''' t
    BoolVal'''' :: Bool                                       -> PhantomExpr'''' t
    AddInt''''  :: PhantomExpr'''' Int -> PhantomExpr'''' Int -> PhantomExpr'''' t
    IsZero''''  :: PhantomExpr'''' Int                        -> PhantomExpr'''' t

data Expr t where
    IntVal  :: Int                             -> Expr Int
    BoolVal :: Bool                            -> Expr Bool
    AddInt  :: Expr Int  -> Expr Int           -> Expr Int
    IsZero  :: Expr Int                        -> Expr Bool
    If      :: Expr Bool -> Expr t   -> Expr t -> Expr t

evaluate :: Expr t -> t
evaluate (IntVal i)     = i                           -- right hand side has type Int
evaluate (BoolVal b)    = b                           -- right hand side has type Bool
evaluate (AddInt e1 e2) = evaluate e1 + evaluate e2   -- right hand side has type Expr Int
                                                      --       and types of e1 e2 must be Expr Int
evaluate (IsZero e)     = evaluate e == 0
evaluate (If e1 e2 e3)  = if evaluate e1 then evaluate e2 else evaluate e3

data Type t where
    TInt  :: Type Int
    TChar :: Type Char
    TList :: Type t -> Type [t]
    TDyn  :: Type Dynamic        -- not used until p. 14 in exposition

tString :: Type String
tString = TList TChar

data Bit = F | T deriving (Eq, Show)

encode :: Type t -> t -> [Bit]
encode TInt i           = encodeInt i
encode TChar c          = encodeChar c
-- note T consed on front and F on end as separators
encode (TList _) []     = F : []
encode (TList t) (x:xs) = T : (encode t x) ++ encode (TList t) xs
encode TDyn (Dyn t v)   = encode t v                               -- not used until p. 14

encodeInt :: Int -> [Bit]
encodeInt 0 = [F]
encodeInt n = reverse $ helper n
    where helper 0 = []
          helper n = let (q,r) = n `divMod` 2 in (mkBit r) : helper q
          mkBit  i = if i == 1 then T else F

encodeChar :: Char -> [Bit]
encodeChar c = encodeInt $ ord c

data Dynamic' = forall t. Dyn' (Type t) t

data Dynamic where
    Dyn :: Type t -> t -> Dynamic

encode' :: Dynamic -> [Bit]
encode' (Dyn t v) = encode t v

castInt :: Dynamic -> Maybe Int
castInt (Dyn TInt i) = Just i
castInt (Dyn _    _) = Nothing

-- ADT:
-- data List t = Nil | Cons t (List t)

-- GADT:
data List t where
    Nil  ::                List t
    Cons :: t -> List t -> List t

listHead :: List t -> t
listHead (Cons a _) = a
listHead Nil        = error "empty list"

data Empty
data NonEmpty

-- param f is Empty when list is empty, NonEmpty otherwise
data SafeList' t f where
    Nil'  ::                       SafeList' t Empty
    Cons' :: t -> SafeList' t f -> SafeList' t NonEmpty

-- head that can ONLY take non-empty lists (p. 16):
headSafe' :: SafeList' t NonEmpty -> t
headSafe' (Cons' t _) = t

data SafeList'' t f where
    Nil''  ::                        SafeList'' t Empty
    Cons'' :: t -> SafeList'' t f -> SafeList'' t f'     -- note f'

repeatElem'' :: a -> Int -> SafeList'' a Empty
repeatElem'' a 0 = Nil''
repeatElem'' a n = Cons'' a (repeatElem'' a (n-1))

-- Peano numbers
data Zero'''
data Succ''' n

data List''' a n where
    Nil'''  ::                     List''' a Zero'''
    Cons''' :: a -> List''' a n -> List''' a (Succ''' n)

headSafe''' :: List''' t (Succ''' n) -> t
headSafe''' (Cons''' t _) = t

-- type encode that map does not change length
mapSafe''' :: (a -> b) -> List''' a n -> List''' b n
mapSafe''' _         Nil''' = Nil'''
mapSafe''' f (Cons''' x xs) = Cons''' (f x) (mapSafe''' f xs)

type family Plus''' a b
type instance Plus''' Zero'''     n = n
type instance Plus''' (Succ''' m) n = Succ''' (Plus''' m n)

concatenate''' :: List''' a m -> List''' a n -> List''' a (Plus''' m n)
concatenate''' Nil''' ys = ys
concatenate''' (Cons''' x xs) ys = Cons''' x (concatenate''' xs ys)

data Nat'''' = Zero'''' | Succ'''' Nat''''

data List'''' a (n::Nat'''') where
    Nil''''  ::                      List'''' a 'Zero''''
    Cons'''' :: a -> List'''' a n -> List'''' a ('Succ'''' n)

data NatSing (n::Nat'''') where
    ZeroSing ::              NatSing 'Zero''''
    SuccSing :: NatSing n -> NatSing ('Succ'''' n)

repeatElem'''' :: a -> NatSing n -> List'''' a n
repeatElem'''' _ ZeroSing     = Nil''''
repeatElem'''' x (SuccSing n) = Cons'''' x (repeatElem'''' x n)  -- note: substraction done by structural induction

type family   (m::Nat'''')  :< (n::Nat'''') :: Bool
type instance  m            :< 'Zero''''     = 'False
type instance 'Zero''''     :< ('Succ'''' n) = 'True
type instance ('Succ'''' m) :< ('Succ'''' n) = m :< n

nthElem'''' :: (n :< m) ~ 'True => List'''' a m -> NatSing n -> a
nthElem'''' (Cons'''' x  _) ZeroSing     = x
nthElem'''' (Cons'''' _ xs) (SuccSing n) = nthElem'''' xs n

data Color   = R | B
data Node' a = E' | N' Color (Node' a) a (Node' a)
type Tree' a = Node' a

member' :: Ord a => a -> Tree' a -> Bool
member' _ E' = False
member' x (N' _ l a r)
    | x < a = member' x l
    | x > a = member' x r
    | otherwise = True

insert' :: Ord a => Tree' a -> a -> Tree' a
insert' t v = blacken (insert'' t v) where
    insert'' n@(N' c l a r) x
        | x < a = leftBalance'  (N' c (insert'' l x) a           r)
        | x > a = rightBalance' (N' c           l    a (insert'' r x))
        | otherwise = n
    insert''    E'     x    = N' R E' x E'
    blacken    (N' _ l x r) = N' B l  x r

leftBalance' :: Node' a -> Node' a
leftBalance' (N' B (N' R (N' R a x       b) y       c)  z d) =
              N' R (N' B       a x       b) y (N' B c   z d)
leftBalance' (N' B (N' R       a x (N' R b  y       c)) z d) =
              N' R (N' B       a x       b) y (N' B c   z d)
leftBalance' n = n

rightBalance' :: Node' a -> Node' a
rightBalance' (N' B       a x (N' R       b  y (N' R c  z d))) =
               N' R (N' B a x             b) y (N' B c  z d)
rightBalance' (N' B       a x (N' R (N' R b  y       c) z d))  =
               N' R (N' B a x             b) y (N' B c  z d)

data Nat = Zero | Succ Nat

type family IncBH (c::Color) (bh::Nat) :: Nat
type instance IncBH R bh = bh
type instance IncBH B bh = Succ bh

data ColorSingleton (c::Color) where
    SR :: ColorSingleton R
    SB :: ColorSingleton B

instance Show (ColorSingleton c) where
    show SR = show "R"
    show SB = show "B"

data Node4 a (bh::Nat) where
    E4 :: Node4 a 'Zero
    N4 :: ColorSingleton c -> Node4 a bh -> a -> Node4 a bh
                           -> Node4 a (IncBH c bh)

data Tree4 a where
    Root4 :: Node4 a bh -> Tree4 a

insert4 :: Ord a => Tree4 a -> a -> Tree4 a
insert4 (Root4 t) v = blacken (insert' t v) where
    insert' :: Ord a => Node4 a n -> a -> Node4 a n
    insert' n@(N4 c l a r) x
        | x < a = leftBalance4  (N4 c (insert' l x) a          r)
        | x > a = rightBalance4 (N4 c          l    a (insert' r x))
        | otherwise = n
    insert'    E4     x    =        N4 SR E4 x E4
    blacken   (N4 _ l x r) = Root4 (N4 SB l x r)


leftBalance4  :: Node4 a bh -> Node4 a bh
leftBalance4  (N4 SB (N4 SR (N4 SR a x       b) y        c)  z d) =
               N4 SR (N4 SB       a x        b) y (N4 SB c   z d)
leftBalance4  (N4 SB (N4 SR       a x (N4 SR b  y        c)) z d) =
               N4 SR (N4 SB       a x        b) y (N4 SB c   z d)
leftBalance4 n = n

rightBalance4 :: Node4 a bh -> Node4 a bh
rightBalance4 (N4 SB        a x (N4 SR        b  y (N4 SR c  z d))) =
               N4 SR (N4 SB a x               b) y (N4 SB c  z d)
rightBalance4 (N4 SB        a x (N4 SR (N4 SR b  y        c) z d))  =
               N4 SR (N4 SB a x               b) y (N4 SB c  z d)

class ValidColors (parent::Color) (child1::Color) (child2::Color)

instance ValidColors R B  B  -- red with only black children
instance ValidColors B c1 c2 -- black with children of any color

data Node a (bh::Nat) (c::Color) where
    E :: Node a 'Zero B
    N :: ValidColors c c1 c1 => ColorSingleton c
           -> Node a bh c1 -> a -> Node a bh c2
              -> Node a (IncBH c bh) c

instance Show (Node a b c) where
    show  E          = show "(E 0 B)"
    show (N c l x r) = show "(N"
                           ++ " " ++ (show c)
                           ++ " " ++ (show l)
                        -- ++ " " ++ (show x) -- TODO
                           ++ " " ++ (show r)
                           ++ ")"

data Tree a where
    Root :: Node a bh B -> Tree a

data IntNode a (n::Nat) where
    IntNode :: ColorSingleton c
                 -> Node a n c1 -> a -> Node a n c2
                    -> IntNode a (IncBH c n)

insert :: Ord a => Tree a -> a -> Tree a
insert (Root t) v = blacken (insert' t v) where
    insert' :: Ord a => Node a n c -> a -> IntNode a n
    insert'     n@(N c l a r) x
        | x < a = leftBalance  c (insert' l x) a          r
        | x > a = rightBalance c          l    a (insert' r x)
        | otherwise = IntNode  c          l    a          r
    insert'        E          x =
                      IntNode  SR         E    x          E
    blacken (IntNode _ l x r) =
                      Root  (N SB         l    x          r)

leftBalance :: ColorSingleton c
               -> IntNode a n -> a -> Node a n c'
                  -> IntNode a (IncBH c n)
leftBalance SB (IntNode SR (N SR a              x       b) y       c)   z d =
                IntNode SR (N SB a              x       b) y (N SB c    z d)
leftBalance SB (IntNode SR       a              x (N SR b  y       c))  z d =
                IntNode SR (N SB a              x       b) y (N SB c    z d)
-- tree balanced, but need to change type from IntNode to Node:
leftBalance c  (IntNode SB       a              x       b)              z d =
                IntNode c  (N SB a              x       b)              z d
-- red nodes must have black children
leftBalance c  (IntNode SR       a@(N SB _ _ _) x       b@(N SB _ _ _)) z d =
                IntNode c  (N SR a              x       b)              z d
-- red nodes must have black children
leftBalance c  (IntNode SR  E                   x          E)           z d =
                IntNode c  (N SR    E           x          E)           z d

-- cannot happen, but not enough type info to omit:
leftBalance _  (IntNode SR (N SR    _           _       _) _       _)   _ _ =
                error "cannot happen"
leftBalance _  (IntNode SR  _                           _ (N SR _ _ _)) _ _ =
                error "cannot happen"

-- The case of one regular node and one leaf node is not valid,
-- because nodes have different black heights
-- so no need to look for that case.

rightBalance :: ColorSingleton c
               -> Node a n c' -> a -> IntNode a n
                  -> IntNode a (IncBH c n)
rightBalance SB               a x       (IntNode SR b              y (N SR c  z d)) =
             IntNode SR (N SB a x                   b)             y (N SB c  z d)
rightBalance SB               a x (IntNode SR (N SR b              y       c) z d)  =
             IntNode SR (N SB a x                   b)             y (N SB c  z d)
rightBalance c                a x (IntNode SB       b              y            d)  =
             IntNode c        a x             (N SB b              y            d)
rightBalance c                a x (IntNode SR       b@(N SB _ _ _) y            d@(N SB _ _ _)) =
             IntNode c        a x             (N SR b              y            d)
rightBalance c                a x (IntNode SR  E                   y  E)            =
             IntNode c        a x             (N SR E              y  E)
rightBalance _                _ _ (IntNode SR (N SR _ _ _)         _  _)            =
             error "cannot happen"
rightBalance _                _ _ (IntNode SR _                    _ (N SR _ _ _))  =
             error "cannot happen"

member :: Ord a => a -> Tree a -> Bool
member x (Root t) = mem x t where
    mem :: Ord a => a -> Node a bh c -> Bool
    mem x E = False
    mem x (N _ l y r)
        | x < y     = mem x l
        | x > y     = mem x r
        | otherwise = True

elements :: Ord a => Tree a -> [a]
elements (Root t) = aux t [] where
    aux :: Ord a => Node a bh c -> [a] -> [a]
    aux E xs = xs
    aux (N _ l y r) xs = aux l (y : aux r xs)
