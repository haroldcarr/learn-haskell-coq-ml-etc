module Parse where

------------------------------------------------------------------------------
import           PiTypes
import           STTypes
------------------------------------------------------------------------------
import qualified Data.Functor.Identity                  as DFI
import           Data.List
import           Text.ParserCombinators.Parsec          hiding (State, parse)
import qualified Text.ParserCombinators.Parsec          as P
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- ST

simplyTyped :: GenTokenParser String u DFI.Identity
simplyTyped  = makeTokenParser (haskellStyle { identStart = letter <|> P.char '_'
                                             , reservedNames = ["let", "assume", "putStrLn"] })

parseBindings :: CharParser () ([String], [Info])
parseBindings  =
                     (let rec :: [String] -> [Info] -> CharParser () ([String], [Info])
                          rec e ts =
                            do
                             (x,t) <- parens lambdaPi
                                        (do
                                           x <- identifier simplyTyped
                                           reserved simplyTyped "::"
                                           t <- pInfo
                                           pure (x,t))
                             rec (x : e) (t : ts) <|> pure (x : e, t : ts)
                      in rec [] [])
                     <|>
                     do x <- identifier simplyTyped
                        reserved simplyTyped "::"
                        t <- pInfo
                        pure ([x], [t])
    where
      pInfo = fmap HasType (parseType 0 []) <|> fmap (const (HasKind Star)) (reserved simplyTyped "*")
{-# ANN parseBindings ("HLint: ignore Reduce duplication" :: Prelude.String) #-}

parseStmt :: [String] -> CharParser () (Stmt ITerm Info)
parseStmt e
     =  do
          reserved simplyTyped "let"
          x <- identifier simplyTyped
          reserved simplyTyped "="
          t <- parseITerm 0 e
          pure (Let x t)
    <|> do
          reserved simplyTyped "assume"
          (xs, ts) <- parseBindings
          pure (Assume (reverse (zip xs ts)))
    <|> do
          reserved simplyTyped "putStrLn"
          x <- stringLiteral simplyTyped
          pure (PutStrLn x)
    <|> do
          reserved lambdaPi "out"
          x <- option "" (stringLiteral simplyTyped)
          pure (Out x)
    <|> fmap Eval (parseITerm 0 e)

parseType :: Int -> [String] -> CharParser () Type
parseType 0 e =
    try
       (do
          t <- parseType 1 e
          rest t <|> pure t)
    where
      rest t =
        do
          reserved simplyTyped "->"
          t' <- parseType 0 e
          pure (Fun t t')
parseType 1 e =
        do
          x <- identifier simplyTyped
          pure (TFree (Global x))
    <|> parens simplyTyped (parseType 0 e)
parseType _ _ = undefined

parseITerm :: Int -> [String] -> CharParser () ITerm
parseITerm 0 e =
    try
       (parseITerm 1 e)
parseITerm 1 e =
    try
       (do
          t <- parseITerm 2 e
          rest (Inf t) <|> pure t)
    <|> do
          t <- parens simplyTyped (parseLam e)
          rest t
    where
      rest t =
        do
          reserved simplyTyped "::"
          t' <- parseType 0 e
          pure (Ann t t')
parseITerm 2 e =
        do
          t <- parseITerm 3 e
          ts <- many (parseCTerm 3 e)
          pure (foldl (:@:) t ts)
parseITerm 3 e =
        do
          x <- identifier simplyTyped
          case elemIndex x e of
            Just n  -> pure (Bound n)
            Nothing -> pure (Free (Global x))
    <|> parens simplyTyped (parseITerm 0 e)
parseITerm _ _ = undefined

parseCTerm :: Int -> [String] -> CharParser () CTerm
parseCTerm 0 e =
        parseLam e
    <|> fmap Inf (parseITerm 0 e)
parseCTerm p e =
        try (parens simplyTyped (parseLam e))
    <|> fmap Inf (parseITerm p e)

parseLam :: [String] -> CharParser () CTerm
parseLam e = do
  reservedOp simplyTyped "\\"
  xs <- many1 (identifier simplyTyped)
  reservedOp simplyTyped "->"
  t <- parseCTerm 0 (reverse xs ++ e)
  --  reserved simplyTyped "."
  pure (iterate Lam t !! length xs)

parseIO :: String -> CharParser () a -> String -> IO (Maybe a)
parseIO f p x = case P.parse (whiteSpace simplyTyped >> p >>= \x' -> eof >> pure x') f x of
  Left e  -> Prelude.print e >> pure Nothing
  Right r -> pure (Just r)

------------------------------------------------------------------------------
-- Pi

lambdaPi :: GenTokenParser String u DFI.Identity
lambdaPi  = makeTokenParser (haskellStyle { identStart = letter <|> P.char '_'
                                          , reservedNames = ["forall", "let", "assume", "putStrLn", "out"] })

parseStmt_ :: [String] -> CharParser () (Stmt ITerm_ CTerm_)
parseStmt_ e =
        do
          reserved lambdaPi "let"
          x <- identifier lambdaPi
          reserved lambdaPi "="
          t <- parseITerm_ 0 e
          pure (Let x t)
    <|> do
          reserved lambdaPi "assume"
          (xs, ts) <- parseBindings_ False []
          pure (Assume (reverse (zip xs ts)))
    <|> do
          reserved lambdaPi "putStrLn"
          x <- stringLiteral lambdaPi
          pure (PutStrLn x)
    <|> do
          reserved lambdaPi "out"
          x <- option "" (stringLiteral lambdaPi)
          pure (Out x)
    <|> fmap Eval (parseITerm_ 0 e)

parseBindings_ :: Bool -> [String] -> CharParser () ([String], [CTerm_])
parseBindings_ b e0 =
                     (let rec :: [String] -> [CTerm_] -> CharParser () ([String], [CTerm_])
                          rec e ts =
                            do
                             (x,t) <- parens lambdaPi
                                        (do
                                           x <- identifier lambdaPi
                                           reserved lambdaPi "::"
                                           t <- parseCTerm_ 0 (if b then e else [])
                                           pure (x,t))
                             rec (x : e) (t : ts) <|> pure (x : e, t : ts)
                      in rec e0 [])
                     <|>
                     do x <- identifier lambdaPi
                        reserved lambdaPi "::"
                        t <- parseCTerm_ 0 e0
                        pure (x : e0, [t])

parseITerm_ :: Int -> [String] -> CharParser () ITerm_
parseITerm_ 0 e =
        do
          reserved lambdaPi "forall"
          (fe,t:ts) <- parseBindings_ True e
          reserved lambdaPi "."
          t' <- parseCTerm_ 0 fe
          pure (foldl (\p t0 -> Pi_ t0 (Inf_ p)) (Pi_ t t') ts)
    <|>
    try
       (do
          t <- parseITerm_ 1 e
          rest (Inf_ t) <|> pure t)
    <|> do
          t <- parens lambdaPi (parseLam_ e)
          rest t
    where
      rest t =
        do
          reserved lambdaPi "->"
          t' <- parseCTerm_ 0 ([]:e)
          pure (Pi_ t t')
parseITerm_ 1 e =
    try
       (do
          t <- parseITerm_ 2 e
          rest (Inf_ t) <|> pure t)
    <|> do
          t <- parens lambdaPi (parseLam_ e)
          rest t
    where
      rest t =
        do
          reserved lambdaPi "::"
          t' <- parseCTerm_ 0 e
          pure (Ann_ t t')
parseITerm_ 2 e =
        do
          t <- parseITerm_ 3 e
          ts <- many (parseCTerm_ 3 e)
          pure (foldl (:$:) t ts)
parseITerm_ 3 e =
        do
          reserved lambdaPi "*"
          pure Star_
    <|> do
          n <- natural lambdaPi
          pure (toNat_ n)
    <|> do
          x <- identifier lambdaPi
          case elemIndex x e of
            Just n  -> pure (Bound_ n)
            Nothing -> pure (Free_ (Global x))
    <|> parens lambdaPi (parseITerm_ 0 e)
parseITerm_ _ _ = undefined

parseCTerm_ :: Int -> [String] -> CharParser () CTerm_
parseCTerm_ 0 e =
        parseLam_ e
    <|> fmap Inf_ (parseITerm_ 0 e)
parseCTerm_ p e =
        try (parens lambdaPi (parseLam_ e))
    <|> fmap Inf_ (parseITerm_ p e)

parseLam_ :: [String] -> CharParser () CTerm_
parseLam_ e =
        do reservedOp lambdaPi "\\"
           xs <- many1 (identifier lambdaPi)
           reservedOp lambdaPi "->"
           t <- parseCTerm_ 0 (reverse xs ++ e)
           --  reserved lambdaPi "."
           pure (iterate Lam_ t !! length xs)

toNat_ :: Integer -> ITerm_
toNat_ n = Ann_ (toNat_' n) (Inf_ Nat_)

toNat_' :: Integer -> CTerm_
toNat_' 0 = Zero_
toNat_' n = Succ_ (toNat_' (n - 1))
