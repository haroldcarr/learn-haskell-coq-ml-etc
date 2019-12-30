module LambdaPi where

import           Control.Monad.Except
import           Data.Char
import qualified Data.Functor.Identity                  as DFI
import           Data.List
import           Data.Maybe
import           Prelude                                hiding (print)
import qualified Prelude
import qualified System.IO                              as SIO
import           Text.ParserCombinators.Parsec          hiding (State, parse)
import qualified Text.ParserCombinators.Parsec          as P
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token
import qualified Text.PrettyPrint.HughesPJ              as PP

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
                                           return (x,t))
                             rec (x : e) (t : ts) <|> return (x : e, t : ts)
                      in rec [] [])
                     <|>
                     do x <- identifier simplyTyped
                        reserved simplyTyped "::"
                        t <- pInfo
                        return ([x], [t])
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
          return (Let x t)
    <|> do
          reserved simplyTyped "assume"
          (xs, ts) <- parseBindings
          return (Assume (reverse (zip xs ts)))
    <|> do
          reserved simplyTyped "putStrLn"
          x <- stringLiteral simplyTyped
          return (PutStrLn x)
    <|> do
          reserved lambdaPi "out"
          x <- option "" (stringLiteral simplyTyped)
          return (Out x)
    <|> fmap Eval (parseITerm 0 e)

parseType :: Int -> [String] -> CharParser () Type
parseType 0 e =
    try
       (do
          t <- parseType 1 e
          rest t <|> return t)
    where
      rest t =
        do
          reserved simplyTyped "->"
          t' <- parseType 0 e
          return (Fun t t')
parseType 1 e =
        do
          x <- identifier simplyTyped
          return (TFree (Global x))
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
          rest (Inf t) <|> return t)
    <|> do
          t <- parens simplyTyped (parseLam e)
          rest t
    where
      rest t =
        do
          reserved simplyTyped "::"
          t' <- parseType 0 e
          return (Ann t t')
parseITerm 2 e =
        do
          t <- parseITerm 3 e
          ts <- many (parseCTerm 3 e)
          return (foldl (:@:) t ts)
parseITerm 3 e =
        do
          x <- identifier simplyTyped
          case elemIndex x e of
            Just n  -> return (Bound n)
            Nothing -> return (Free (Global x))
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
parseLam e =
        do reservedOp simplyTyped "\\"
           xs <- many1 (identifier simplyTyped)
           reservedOp simplyTyped "->"
           t <- parseCTerm 0 (reverse xs ++ e)
           --  reserved simplyTyped "."
           return (iterate Lam t !! length xs)

parseIO :: String -> CharParser () a -> String -> IO (Maybe a)
parseIO f p x = case P.parse (whiteSpace simplyTyped >> p >>= \x' -> eof >> return x') f x of
                    Left e  -> Prelude.print e >> return Nothing
                    Right r -> return (Just r)

tPrint :: Int -> Type -> PP.Doc
tPrint _ (TFree (Global s))  = PP.text s
tPrint p (Fun ty ty')        = parensIf (p > 0) (PP.sep [tPrint 0 ty <> PP.text " ->", PP.nest 2 (tPrint 0 ty')])
tPrint _ _ = undefined

iPrint :: Int -> Int -> ITerm -> PP.Doc
iPrint p ii (Ann c ty)       = parensIf (p > 1) (cPrint 2 ii c <> PP.text " :: " <> tPrint 0 ty)
iPrint _ ii (Bound k)        = PP.text (vars !! (ii - k - 1))
iPrint _ _  (Free (Global s))= PP.text s
iPrint p ii (i :@: c)        = parensIf (p > 2) (PP.sep [iPrint 2 ii i, PP.nest 2 (cPrint 3 ii c)])
iPrint _  _ x                = PP.text ("[" ++ show x ++ "]")

cPrint :: Int -> Int -> CTerm -> PP.Doc
cPrint p ii (Inf i)    = iPrint p ii i
cPrint p ii (Lam c)    = parensIf (p > 0) (PP.text "\\" <> PP.text (vars !! ii) <> PP.text " -> " <> cPrint 0 (ii + 1) c)

vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Int)..], c <- ['x','y','z'] ++ ['a'..'w'] ]

parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf True  = PP.parens
parensIf False = id

print    :: CTerm -> String
print     = PP.render . cPrint 0 0
printType :: Type -> String
printType = PP.render . tPrint 0

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
          return (Let x t)
    <|> do
          reserved lambdaPi "assume"
          (xs, ts) <- parseBindings_ False []
          return (Assume (reverse (zip xs ts)))
    <|> do
          reserved lambdaPi "putStrLn"
          x <- stringLiteral lambdaPi
          return (PutStrLn x)
    <|> do
          reserved lambdaPi "out"
          x <- option "" (stringLiteral lambdaPi)
          return (Out x)
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
                                           return (x,t))
                             rec (x : e) (t : ts) <|> return (x : e, t : ts)
                      in rec e0 [])
                     <|>
                     do x <- identifier lambdaPi
                        reserved lambdaPi "::"
                        t <- parseCTerm_ 0 e0
                        return (x : e0, [t])

parseITerm_ :: Int -> [String] -> CharParser () ITerm_
parseITerm_ 0 e =
        do
          reserved lambdaPi "forall"
          (fe,t:ts) <- parseBindings_ True e
          reserved lambdaPi "."
          t' <- parseCTerm_ 0 fe
          return (foldl (\p t0 -> Pi_ t0 (Inf_ p)) (Pi_ t t') ts)
    <|>
    try
       (do
          t <- parseITerm_ 1 e
          rest (Inf_ t) <|> return t)
    <|> do
          t <- parens lambdaPi (parseLam_ e)
          rest t
    where
      rest t =
        do
          reserved lambdaPi "->"
          t' <- parseCTerm_ 0 ([]:e)
          return (Pi_ t t')
parseITerm_ 1 e =
    try
       (do
          t <- parseITerm_ 2 e
          rest (Inf_ t) <|> return t)
    <|> do
          t <- parens lambdaPi (parseLam_ e)
          rest t
    where
      rest t =
        do
          reserved lambdaPi "::"
          t' <- parseCTerm_ 0 e
          return (Ann_ t t')
parseITerm_ 2 e =
        do
          t <- parseITerm_ 3 e
          ts <- many (parseCTerm_ 3 e)
          return (foldl (:$:) t ts)
parseITerm_ 3 e =
        do
          reserved lambdaPi "*"
          return Star_
    <|> do
          n <- natural lambdaPi
          return (toNat_ n)
    <|> do
          x <- identifier lambdaPi
          case elemIndex x e of
            Just n  -> return (Bound_ n)
            Nothing -> return (Free_ (Global x))
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
           return (iterate Lam_ t !! length xs)

toNat_ :: Integer -> ITerm_
toNat_ n = Ann_ (toNat_' n) (Inf_ Nat_)

toNat_' :: Integer -> CTerm_
toNat_' 0 = Zero_
toNat_' n = Succ_ (toNat_' (n - 1))

iPrint_ :: Int -> Int -> ITerm_ -> PP.Doc
iPrint_ p ii (Ann_ c ty)       = parensIf (p > 1) (cPrint_ 2 ii c <> PP.text " :: " <> cPrint_ 0 ii ty)
iPrint_ _  _ Star_             = PP.text "*"
iPrint_ p ii (Pi_ d (Inf_ (Pi_ d' r)))
                               = parensIf (p > 0) (nestedForall_ (ii + 2) [(ii + 1, d'), (ii, d)] r)
iPrint_ p ii (Pi_ d r)         = parensIf (p > 0) (PP.sep [PP.text "forall " <> PP.text (vars !! ii) <> PP.text " :: " <> cPrint_ 0 ii d <> PP.text " .", cPrint_ 0 (ii + 1) r])
iPrint_ _ ii (Bound_ k)        = PP.text (vars !! (ii - k - 1))
iPrint_ _  _ (Free_ (Global s))= PP.text s
iPrint_ p ii (i :$: c)         = parensIf (p > 2) (PP.sep [iPrint_ 2 ii i, PP.nest 2 (cPrint_ 3 ii c)])
iPrint_ _  _ Nat_              = PP.text "Nat"
iPrint_ p ii (NatElim_ m z s n)= iPrint_ p ii (Free_ (Global "natElim") :$: m :$: z :$: s :$: n)
iPrint_ p ii (Vec_ a n)        = iPrint_ p ii (Free_ (Global "Vec") :$: a :$: n)
iPrint_ p ii (VecElim_ a m mn mc n xs)
                               = iPrint_ p ii (Free_ (Global "vecElim") :$: a :$: m :$: mn :$: mc :$: n :$: xs)
iPrint_ p ii (Eq_ a x y)       = iPrint_ p ii (Free_ (Global "Eq") :$: a :$: x :$: y)
iPrint_ p ii (EqElim_ a m mr x y eq)
                                 = iPrint_ p ii (Free_ (Global "eqElim") :$: a :$: m :$: mr :$: x :$: y :$: eq)
iPrint_ p ii (Fin_ n)          = iPrint_ p ii (Free_ (Global "Fin") :$: n)
iPrint_ p ii (FinElim_ m mz ms n f)
                               = iPrint_ p ii (Free_ (Global "finElim") :$: m :$: mz :$: ms :$: n :$: f)
iPrint_ _  _ x                 = PP.text ("[" ++ show x ++ "]")

cPrint_ :: Int -> Int -> CTerm_ -> PP.Doc
cPrint_ p ii (Inf_ i)    = iPrint_ p ii i
cPrint_ p ii (Lam_ c)    = parensIf (p > 0) (PP.text "\\" <> PP.text (vars !! ii) <> PP.text " -> " <> cPrint_ 0 (ii + 1) c)
cPrint_ _ ii Zero_       = fromNat_ 0 ii Zero_     --  PP.text "Zero"
cPrint_ _ ii (Succ_ n)   = fromNat_ 0 ii (Succ_ n) --  iPrint_ p ii (Free_ (Global "Succ") :$: n)
cPrint_ p ii (Nil_ a)    = iPrint_ p ii (Free_ (Global "Nil") :$: a)
cPrint_ p ii (Cons_ a n x xs) =
                             iPrint_ p ii (Free_ (Global "Cons") :$: a :$: n :$: x :$: xs)
cPrint_ p ii (Refl_ a x) = iPrint_ p ii (Free_ (Global "Refl") :$: a :$: x)
cPrint_ p ii (FZero_ n)  = iPrint_ p ii (Free_ (Global "FZero") :$: n)
cPrint_ p ii (FSucc_ n f)= iPrint_ p ii (Free_ (Global "FSucc") :$: n :$: f)

fromNat_ :: Int -> Int -> CTerm_ -> PP.Doc
fromNat_ n  _ Zero_ = PP.int n
fromNat_ n ii (Succ_ k) = fromNat_ (n + 1) ii k
fromNat_ n ii t = parensIf True (PP.int n <> PP.text " + " <> cPrint_ 0 ii t)

nestedForall_ :: Int -> [(Int, CTerm_)] -> CTerm_ -> PP.Doc
nestedForall_ ii ds (Inf_ (Pi_ d r)) = nestedForall_ (ii + 1) ((ii, d) : ds) r
nestedForall_ ii ds x                = PP.sep [PP.text "forall " <> PP.sep [parensIf True (PP.text (vars !! n) <> PP.text " :: " <> cPrint_ 0 n d) | (n,d) <- reverse ds] <> PP.text " .", cPrint_ 0 ii x]

data Stmt i tinf = Let String i           --  let x = t
                 | Assume [(String,tinf)] --  assume x :: t, assume x :: *
                 | Eval i
                 | PutStrLn String        --  lhs2TeX hacking, allow to print "magic" string
                 | Out String             --  more lhs2TeX hacking, allow to print to files
    deriving (Eq, Show)

  --  read-eval-print loop
readevalprint :: Interpreter i c v t tinf inf -> State v inf -> IO ()
readevalprint int0 state0@(inter, _out, _ve, _te) =
    let rec int state =
          do
            putStr (iprompt int); SIO.hFlush SIO.stdout
            x0 <- Just <$> getLine
            case x0 of
              Nothing -> return ()
              Just "" -> rec int state
              Just x  ->
                do
                  -- when inter (addHistory x x) -- HC/TODO
                  c      <- interpretCommand x
                  state' <- handleCommand int state c
                  maybe (return ()) (rec int) state'
    in
      do
        --  welcome
        when inter $ putStrLn ("Interpreter for " ++ iname int0 ++ ".\n" ++
                               "Type :? for help.")
        --  enter loop
        rec int0 state0

data Command = TypeOf String
             | Compile CompileForm
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

type NameEnv v = [(Name, v)]
type Ctx inf = [(Name, inf)]
type State v inf = (Bool, String, NameEnv v, Ctx inf)

commands :: [InteractiveCommand]
commands
    = [ Cmd [":type"]        "<expr>"  TypeOf         "print type of expression",
        Cmd [":browse"]      ""        (const Browse) "browse names in scope",
        Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                       "load program from file",
        Cmd [":quit"]        ""        (const Quit)   "exit interpreter",
        Cmd [":help",":?"]   ""        (const Help)   "display this list of commands" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs0
    = "List of commands:  Any command may be abbreviated to :c where\n" ++
       "c is the first character in the full name.\n\n" ++
       "<expr>                  evaluate expression\n" ++
       "let <var> = <expr>      define variable\n" ++
       "assume <var> :: <expr>  assume variable\n\n"
       ++
       unlines (map (\(Cmd cs a _ d) ->
                       let ct = intercalate ", " (map (++ if null a then "" else " " ++ a) cs)
                        in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs0)

interpretCommand :: String -> IO Command
interpretCommand x
    = if ":" `isPrefixOf` x then
         do let (cmd,t') = break isSpace x
                t        = dropWhile isSpace t'
            --  find matching commands
            let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
            case matching of
               []  -> do putStrLn ("Unknown command `" ++ cmd ++ "'. Type :? for help.")
                         return Noop
               [Cmd _ _ f _]
                   ->    return (f t)
               _x  -> do putStrLn ("Ambiguous command, could be " ++ intercalate ", " [ head cs | Cmd cs _ _ _ <- matching ] ++ ".")
                         return Noop
      else
         return (Compile (CompileInteractive x))

handleCommand :: Interpreter i c v t tinf inf -> State v inf -> Command -> IO (Maybe (State v inf))
handleCommand int state@(inter, _out, ve, te) cmd
    = case cmd of
         Quit   -> unless inter (putStrLn "!@#$^&*") >> return Nothing
         Noop   -> return (Just state)
         Help   -> putStr (helpTxt commands) >> return (Just state)
         TypeOf x0 ->
                    do x <- parseIO "<interactive>" (iiparse int) x0
                       t <- maybe (return Nothing) (iinfer int ve te) x
                       maybe (return ()) (putStrLn . PP.render . itprint int) t
                       return (Just state)
         Browse -> do putStr (unlines [ s | Global s <- reverse (nub (map fst te)) ])
                      return (Just state)
         Compile c ->
                    do state' <- case c of
                                   CompileInteractive s -> compilePhrase int state s
                                   CompileFile f        -> compileFile int state f
                       return (Just state')

compileFile :: Interpreter i c v t tinf inf -> State v inf -> String -> IO (State v inf)
compileFile int state f = do
  x     <- readFile f
  stmts <- parseIO f (many (isparse int)) x
  maybe (error "compileFile") {-(return state)-} (foldM (handleStmt int) state) stmts

compilePhrase :: Interpreter i c v t tinf inf -> State v inf -> String -> IO (State v inf)
compilePhrase int state x0 = do
  x <- parseIO "<interactive>" (isparse int) x0
  maybe (error "compilePhrase") {-(return state)-} (handleStmt int state) x

data Interpreter i c v t tinf inf =
    I { iname    :: String,
        iprompt  :: String,
        iitype   :: NameEnv v -> Ctx inf -> i -> Result t,
        iquote   :: v -> c,
        ieval    :: NameEnv v -> i -> v,
        ihastype :: t -> inf,
        icprint  :: c -> PP.Doc,
        itprint  :: t -> PP.Doc,
        iiparse  :: CharParser () i,
        isparse  :: CharParser () (Stmt i tinf),
        iassume  :: State v inf -> (String, tinf) -> IO (State v inf) }

st :: Interpreter ITerm CTerm Value Type Info Info
st = I { iname = "the simply typed lambda calculus",
         iprompt = "ST> ",
         iitype = \_v c -> iType 0 c,
         iquote = quote0,
         ieval  = \e x -> iEval x (e, []),
         ihastype = HasType,
         icprint = cPrint 0 0,
         itprint = tPrint 0,
         iiparse = parseITerm 0 [],
         isparse = parseStmt [],
         iassume = \s (x, t) -> stassume s x t }

lp :: Interpreter ITerm_ CTerm_ Value_ Value_ CTerm_ Value_
lp = I { iname = "lambda-Pi",
         iprompt = "LP> ",
         iitype = curry (iType_ 0),
         iquote = quote0_,
         ieval = \e x -> iEval_ x (e, []),
         ihastype = id,
         icprint = cPrint_ 0 0,
         itprint = cPrint_ 0 0 . quote0_,
         iiparse = parseITerm_ 0 [],
         isparse = parseStmt_ [],
         iassume = \s (x, t) -> lpassume s x t }

lpte :: Ctx Value_
lpte =     [   (Global "Zero", VNat_),
               (Global "Succ", VPi_ VNat_ (const VNat_)),
               (Global "Nat", VStar_),
               (Global "natElim", VPi_ (VPi_ VNat_ (const VStar_)) (\m ->
                                 VPi_ (m `vapp_` VZero_) (\_ ->
                                 VPi_ (VPi_ VNat_ (\k -> VPi_ (m `vapp_` k) (const (m `vapp_` VSucc_ k)))) ( \_ ->
                                 VPi_ VNat_ (\n -> m `vapp_` n))))),
               (Global "Nil", VPi_ VStar_ (`VVec_` VZero_)),
               (Global "Cons", VPi_ VStar_ (\a ->
                              VPi_ VNat_ (\n ->
                              VPi_ a (\_ -> VPi_ (VVec_ a n) (\_ -> VVec_ a (VSucc_ n)))))),
               (Global "Vec", VPi_ VStar_ (\_ -> VPi_ VNat_ (const VStar_))),
               (Global "vecElim", VPi_ VStar_ (\a ->
                                 VPi_ (VPi_ VNat_ (\n -> VPi_ (VVec_ a n) (const VStar_))) (\m ->
                                 VPi_ (m `vapp_` VZero_ `vapp_` VNil_ a) (\_ ->
                                 VPi_ (VPi_ VNat_ (\n ->
                                       VPi_ a (\x ->
                                       VPi_ (VVec_ a n) (\xs ->
                                       VPi_ (m `vapp_` n `vapp_` xs) (\_ ->
                                       m `vapp_` VSucc_ n `vapp_` VCons_ a n x xs))))) (\_ ->
                                 VPi_ VNat_ (\n ->
                                 VPi_ (VVec_ a n) (\xs -> m `vapp_` n `vapp_` xs))))))),
               (Global "Refl", VPi_ VStar_ (\a -> VPi_ a (\x ->
                              VEq_ a x x))),
               (Global "Eq", VPi_ VStar_ (\a -> VPi_ a (\_x -> VPi_ a (const VStar_)))),
               (Global "eqElim", VPi_ VStar_ (\a ->
                                VPi_ (VPi_ a (\x -> VPi_ a (\y -> VPi_ (VEq_ a x y) (const VStar_)))) (\m ->
                                VPi_ (VPi_ a (\x -> m `vapp_` x `vapp_` x `vapp_` VRefl_ a x)) (\_ ->
                                VPi_ a (\x -> VPi_ a (\y ->
                                VPi_ (VEq_ a x y) (\eq ->
                                m `vapp_` x `vapp_` y `vapp_` eq))))))),
               (Global "FZero", VPi_ VNat_ (VFin_ . VSucc_)),
               (Global "FSucc", VPi_ VNat_ (\n -> VPi_ (VFin_ n) (\_f ->
                               VFin_ (VSucc_ n)))),
               (Global "Fin", VPi_ VNat_ (const VStar_)),
               (Global "finElim", VPi_ (VPi_ VNat_ (\n -> VPi_ (VFin_ n) (const VStar_))) (\m ->
                                 VPi_ (VPi_ VNat_ (\n -> m `vapp_` VSucc_ n `vapp_` VFZero_ n)) (\_ ->
                                 VPi_ (VPi_ VNat_ (\n -> VPi_ (VFin_ n) (\f -> VPi_ (m `vapp_` n `vapp_` f) (\_ -> m `vapp_` VSucc_ n `vapp_` VFSucc_ n f)))) (\_ ->
                                 VPi_ VNat_ (\n -> VPi_ (VFin_ n) (\f ->
                                 m `vapp_` n `vapp_` f))))))]

lpve :: Ctx Value_
lpve =     [   (Global "Zero", VZero_),
               (Global "Succ", VLam_ VSucc_),
               (Global "Nat", VNat_),
               (Global "natElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (NatElim_ (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0)))))))) ([], [])),
               (Global "Nil", VLam_ VNil_),
               (Global "Cons", VLam_ (\a -> VLam_ (\n -> VLam_ (VLam_ . VCons_ a n)))),
               (Global "Vec", VLam_ (VLam_ . VVec_)),
               (Global "vecElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (VecElim_ (Inf_ (Bound_ 5)) (Inf_ (Bound_ 4)) (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0)))))))))) ([],[])),
               (Global "Refl", VLam_ (VLam_ . VRefl_)),
               (Global "Eq", VLam_ (\a -> VLam_ (VLam_ . VEq_ a))),
               (Global "eqElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (EqElim_ (Inf_ (Bound_ 5)) (Inf_ (Bound_ 4)) (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0)))))))))) ([],[])),
               (Global "FZero", VLam_ VFZero_),
               (Global "FSucc", VLam_ (VLam_ . VFSucc_)),
               (Global "Fin", VLam_ VFin_),
               (Global "finElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (FinElim_ (Inf_ (Bound_ 4)) (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0))))))))) ([],[]))]

repLP :: Bool -> IO ()
repLP b = readevalprint lp (b, [], lpve, lpte)

repST :: Bool -> IO ()
repST b = readevalprint st (b, [], [], [])

iinfer
  :: Interpreter i c v a tinf inf
  -> NameEnv v
  -> Ctx inf
  -> i
  -> IO (Maybe a)
iinfer int d g t =
    case iitype int d g t of
      Left e  -> putStrLn e >> return Nothing
      Right v -> return (Just v)

handleStmt
  :: Interpreter i c v t tinf inf
  -> State v inf
  -> Stmt i tinf
  -> IO (State v inf)
handleStmt int state@(inter, out, ve, te) stmt =
      case stmt of
          Assume ass -> foldM (iassume int) state ass
          Let x e    -> checkEval x e
          Eval e     -> checkEval it e
          PutStrLn x -> putStrLn x >> return state
          Out f      -> return (inter, f, ve, te)
    where
      --  checkEval :: String -> i -> IO (State v inf)
      checkEval i t =
        check int state i t
          (\(y, v) -> do
                         --  ugly, but we have limited space in the paper
                         --  usually, you'd want to have the bound identifier *and*
                         --  the result of evaluation
                         let outtext = if i == it then PP.render (icprint int (iquote int v) <> PP.text " :: " <> itprint int y)
                                                  else PP.render (PP.text i <> PP.text " :: " <> itprint int y)
                         putStrLn outtext
                         unless (null out) (writeFile out (process outtext)))
          (\(y, v) ->
              (inter, "", (Global i, v) : ve, (Global i, ihastype int y) : te))

check
  :: Interpreter i c v t tinf inf
  -> State v inf
  -> String
  -> i
  -> ((t, v) -> IO ())
  -> ((t, v) -> State v inf)
  -> IO (State v inf)
check int state@(_inter, _out, ve, te) _i t kp k =
                  do
                    --  typecheck and evaluate
                    x <- iinfer int ve te t
                    case x of
                      Nothing  ->
                        do
                          putStrLn "type error"
                          return state
                      Just y   ->
                        do
                          let v = ieval int ve t
                          kp (y, v)
                          return (k (y, v))

stassume
  :: Monad m
  => State v inf
  -> String
  -> inf
  -> m (State v inf)
stassume _state@(inter, out, ve, te) x t =
  return               (inter, out, ve, (Global x, t) : te)


lpassume :: State Value_ Value_ -> String -> CTerm_ -> IO (State Value_ Value_)
lpassume state@(inter, out, ve, te) x t =
    check lp state x (Ann_ t (Inf_ Star_))
          (\(_, v) -> do
              putStrLn (PP.render (PP.text x <> PP.text " :: " <> cPrint_ 0 0 (quote0_ v)))
              return ())
          (\(_, v) ->
              (inter, out, ve, (Global x, v) : te))

it :: String
it  = "it"

process :: String -> String
process = unlines . map (\x -> "< " ++ x) . lines

main :: IO ()
main = repLP True

------------------------------------------------------------------------------
-- p2 : Simply Typed Lambda Calculus (aka λ→)

{-
- terms explicitly typed
- no type inference
- only base types and functions
- functions are not polymorphic
- strongly normalizing: evaluation terminates for any term, independent of the evaluation strategy
-}

------------------------------------------------------------------------------
-- p3 : (STLC) Abstract syntax

{-
terms

e ::= e :: τ  annotated term
    | x       variable
    | e e'    application
    | λx → e  lambda abstraction
-}
data ITerm -- Inferable
     = Ann    CTerm Type -- explicit Annotation
     | Bound  Int        -- variables : deBruijn indice
     | Free   Name       -- variable : e.g., top level
     | ITerm :@: CTerm   -- application
    deriving (Show, Eq)

data CTerm -- Checkable
     = Inf  ITerm -- Inferable embedded in a CTerm
     | Lam  CTerm -- Lambda abstraction
    deriving (Show, Eq)

data Name
     = Global  String
     | Local   Int -- when passing a binding into an algorithm, temporarily convert a bound var into a free
     | Quote   Int
    deriving (Show, Eq)

{-
types

τ ::= α       base type
    | τ → τ'  function type
-}
data Type
     = TFree  Name       -- type identifier
     | Fun    Type Type  -- function arrows (not polymorphic)
    deriving (Show, Eq)

{-
values

v ::= n       neutral term
    | λx → v  lambda abstraction
-}
data Value
     = VLam      (Value -> Value) -- lambda abstraction (HOAS : rep funs as Haskell funs)
     | VNeutral  Neutral
instance Eq Value where
  (==) (VLam     _)           _  = False
  (==)           _  (VLam     _) = False
  (==) (VNeutral l) (VNeutral r) = l == r
instance Show Value where
  show (VLam _)     = "VLam"
  show (VNeutral n) = "VNeutral " ++ show n
{-
neutral : a variable applied to a (possibly empty) sequence of values

n ::= x     variable
    | n v   application
-}
data Neutral
     = NFree  Name
     | NApp   Neutral Value
     deriving (Show, Eq)

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

------------------------------------------------------------------------------
-- p 3 : (STLC) Evaluation : big-step eval rules

{-
- e ⇓ v means result of completely evaluating e is v
- evaluate everything as far as possible, including under lambda

examples:

(id :: α → α) y ⇓ y
(const :: (β → β) → α → β → β) id y ⇓ id
-}

data Kind = Star
    deriving (Eq, Show)

data Info
     = HasKind  Kind
     | HasType  Type
    deriving (Eq, Show)

type Context = [(Name, Info)]
type Env     = [Value]

iEval :: ITerm -> (NameEnv Value,Env) -> Value

{-
ignore annotation

e ⇓ v
----------
e :: τ ⇓ v
-}
iEval (Ann  e _)  d = cEval e d

{-
variables eval to themselves

------
x ⇓ x
-}
iEval (Free   x)  d = fromMaybe (vfree x) (lookup x (fst d))
iEval (Bound ii)  d = snd d !! ii

-- application
iEval (e1 :@: e2) d = vapp (iEval e1 d) (cEval e2 d)

vapp :: Value -> Value -> Value
{-
application, case left subterm evaluates to lambda abstraction
β-reduce
may produce new redexes, so evaluate result

e ⇓ λx → v    v[x ↦ e'] ⇓ v'
----------------------------
e e' ⇓ v'
-}
vapp (VLam f) v = f v -- Beta-reduce

{-
application, case left subterm evaluates to neutral term
eval cannot proceed further
construct new neutral term from results of evaluating the two subterms
e ⇓ n    e' ⇓ v'
----------------
e e' ⇓ n v'

e ⇓ v
---------------
λx → e ⇓ λx → v
-}
vapp (VNeutral n)  v = VNeutral (NApp n v)

cEval :: CTerm -> (NameEnv Value, Env) -> Value
cEval (Inf  ii) d  = iEval ii d
cEval (Lam  e0) d0 = VLam (\x -> cEval e0 ((\(e, d) -> (e,  x : d)) d0)) -- eval under lambda

------------------------------------------------------------------------------
-- p4 : (STLC) Type System

{-
Γ ::= ε          empty context
    | Γ, α :: ∗  adding a type identifier
    | Y, x :: τ  adding a term identifier

               valid (Γ)               valid (Γ)   Γ ⊢ τ :: ∗
---------      -----------------       ----------------------
valid (ε)      valid (Γ, α :: ∗)       valid (Γ,  x :: τ )

Γ(α) = ∗                 Γ ⊢ τ :: ∗   Γ ⊢ τ' :: ∗
---------- (TVAR)        ------------------------- (FUN)
Γ ⊢ α :: ∗               Γ ⊢ τ      →     τ' :: ∗

Figure 2.  definitions of contexts and their validity

- Γ ⊢ e::t : e is type t in context Γ
- context
  - lists valid base types
  - associates identifiers with type information
- α :: ∗ - indicates α is a base type
- x :: t - indicates x is a term of type t
- free vars in terms and types must occur in the context, e.g.,
  - for const to be of type (β → β) → α → β → β
  - context must contain at least: α :: ∗, β :: ∗, const :: (β → β) → α → β → β
  - Note α and β are introduced before they are used in the type of const
- multiple bindings for same var can occur, rightmost binding takes precedence
- Γ(z) denotes information associated with identifier z by context Γ
- TVAR, FUN explain when a type is well-formed
  - i.e., when all its free variables appear in the context
- in the well-formedness and type rules assume all contexts are valid


Γ ⊢ τ :: ∗   Γ ⊢ e :: ↓ τ
------------------------- (ANN)
Γ ⊢ (e :: τ ) :: ↑ τ

Γ(x) = τ
------------ (var)
Γ ⊢ x :: ↑ τ

Γ ⊢ e :: ↑ τ → τ'     Γ ⊢ e' :: ↓ τ
----------------------------------- (APP)
Γ ⊢ e e' :: ↑ τ'

Γ ⊢ e :: ↑ τ
------------ (CHK)
Γ ⊢ e :: ↓ τ

Γ, x :: τ ⊢ e :: ↓ τ
---------------------- (LAM)
Γ ⊢ λx → e :: ↓ τ → τ'

Figure 3 Type rules for λ →

- no type inference of types of lambda-bound vars
- only type checking
- for annotated terms, vars and applications the types can be determined
- :: ↓ -- type is an input to type checking algorithm
- :: ↑ -- type is produced by type checking algorithm

- inferable terms
  - ANN : check annotated terms against their type annotation, and then return the type
  - VAR : look up type in environment
  - APP : 1st postion must be of a function type;
          check arg against fun’s domain;
          return fun range as the result type
-}

cKind :: Context -> Type -> Kind -> Result ()
cKind g (TFree x) Star
    = case lookup x g of
         Just (HasKind Star) -> return ()
         Nothing             -> throwError "unknown identifier"
cKind g (Fun kk kk') Star
    = do cKind g kk   Star
         cKind g kk'  Star

iType0 :: Context -> ITerm -> Result Type
iType0 = iType 0

iType :: Int -> Context -> ITerm -> Result Type
iType ii g (Ann e ty)
    = do cKind g ty Star
         cType ii g e ty
         return ty
iType  _ g (Free x)
    = case lookup x g of
         Just (HasType ty) -> return ty
         Nothing           -> throwError "unknown identifier"
iType ii g (e1 :@: e2)
    = do si <- iType ii g e1
         case si of
             Fun ty ty'  -> do cType ii g e2 ty
                               return ty'
             _           -> throwError "illegal application"

cType :: Int -> Context -> CTerm -> Type -> Result ()
cType ii g (Inf e) ty
    = do ty' <- iType ii g e
         unless (ty == ty') (throwError "type mismatch")
cType ii g (Lam e) (Fun ty ty')
    = cType  (ii + 1) ((Local ii, HasType ty) : g)
             (cSubst 0 (Free (Local ii)) e) ty'
cType  _ _ _ _
    = throwError "type mismatch"

type Result a = Either String a

iSubst :: Int -> ITerm -> ITerm -> ITerm
iSubst ii r (Ann e ty)  = Ann (cSubst ii r e) ty
iSubst ii r (Bound j)   = if ii == j then r else Bound j
iSubst  _ _ (Free y)    = Free y
iSubst ii r (e1 :@: e2) = iSubst ii r e1 :@: cSubst ii r e2

cSubst :: Int -> ITerm -> CTerm -> CTerm
cSubst ii r (Inf e) = Inf (iSubst ii r e)
cSubst ii r (Lam e) = Lam (cSubst (ii + 1) r e)

quote0 :: Value -> CTerm
quote0 = quote 0

quote :: Int -> Value -> CTerm
quote ii (VLam f)     = Lam (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n) = Inf (neutralQuote ii n)

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote ii (NFree x)  = boundfree ii x
neutralQuote ii (NApp n v) = neutralQuote ii n :@: quote ii v

boundfree :: Int -> Name -> ITerm
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree  _ x         = Free x

id',const' :: CTerm
id'      = Lam (Inf (Bound 0))
const'   = Lam (Lam (Inf (Bound 1)))

tfree   :: String -> Type
tfree a  = TFree (Global a)
free    :: String -> CTerm
free x   = Inf (Free (Global x))

term1,term2 :: ITerm
term1    = Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y"
term2    = Ann const' (Fun  (Fun (tfree "b") (tfree "b"))
                               (Fun  (tfree "a")
                                     (Fun (tfree "b") (tfree "b"))))
              :@: id' :@: free "y"

env1,env2 :: [(Name, Info)]
env1     = [ (Global "y", HasType (tfree "a"))
            , (Global "a", HasKind Star) ]
env2     =   (Global "b", HasKind Star) : env1

test_eval1,test_eval2 :: CTerm
test_eval1= quote0 (iEval term1 ([],[]))
   {-  \eval{test_eval1}  -}

test_eval2= quote0 (iEval term2 ([],[]))
   {-  \eval{test_eval2}  -}

test_type1,test_type2 :: Result Type
test_type1= iType0 env1 term1
   {-  \eval{test_type1}  -}

test_type2= iType0 env2 term2
   {-  \eval{test_type2}  -}

------------------------------------------------------------------------------
-- 3. Dependent Types

data CTerm_
     = Inf_  ITerm_
     | Lam_  CTerm_

     | Zero_
     | Succ_ CTerm_

     | Nil_ CTerm_
     | Cons_ CTerm_ CTerm_ CTerm_ CTerm_

     | Refl_ CTerm_ CTerm_

     | FZero_ CTerm_
     | FSucc_ CTerm_ CTerm_

    deriving (Show, Eq)

data ITerm_
     = Ann_ CTerm_ CTerm_
     | Star_
     | Pi_ CTerm_ CTerm_
     | Bound_  Int
     | Free_  Name
     | ITerm_ :$: CTerm_

     | Nat_
     | NatElim_ CTerm_ CTerm_ CTerm_ CTerm_

     | Vec_ CTerm_ CTerm_
     | VecElim_ CTerm_ CTerm_ CTerm_ CTerm_ CTerm_ CTerm_

     | Eq_ CTerm_ CTerm_ CTerm_
     | EqElim_ CTerm_ CTerm_ CTerm_ CTerm_ CTerm_ CTerm_

     | Fin_ CTerm_
     | FinElim_ CTerm_ CTerm_ CTerm_ CTerm_ CTerm_

    deriving (Show, Eq)

data Value_
    = VLam_  (Value_ -> Value_)
    | VStar_
    | VPi_ Value_ (Value_ -> Value_)
    | VNeutral_ Neutral_

    | VNat_
    | VZero_
    | VSucc_ Value_

    | VNil_ Value_
    | VCons_ Value_ Value_ Value_ Value_
    | VVec_ Value_ Value_

    | VEq_ Value_ Value_ Value_
    | VRefl_ Value_ Value_

    | VFZero_ Value_
    | VFSucc_ Value_ Value_
    | VFin_ Value_

data Neutral_
    = NFree_  Name
    | NApp_  Neutral_ Value_

    | NNatElim_ Value_ Value_ Value_ Neutral_

    | NVecElim_ Value_ Value_ Value_ Value_ Value_ Neutral_

    | NEqElim_ Value_ Value_ Value_ Value_ Value_ Neutral_

    | NFinElim_ Value_ Value_ Value_ Value_ Neutral_

type Env_ = [Value_]

vapp_ :: Value_ -> Value_ -> Value_
vapp_ (VLam_ f)      v = f v
vapp_ (VNeutral_ n)  v = VNeutral_ (NApp_ n v)

vfree_ :: Name -> Value_
vfree_ n = VNeutral_ (NFree_ n)

cEval_ :: CTerm_ -> (NameEnv Value_,Env_) -> Value_
cEval_ (Inf_  ii)    d  = iEval_ ii d
cEval_ (Lam_  c)     d0 = VLam_ (\x -> cEval_ c ((\(e, d) -> (e,  x : d)) d0))

cEval_ Zero_     _d  = VZero_
cEval_ (Succ_ k)  d  = VSucc_ (cEval_ k d)

cEval_ (Nil_ a)          d  = VNil_ (cEval_ a d)
cEval_ (Cons_ a n x xs)  d  = VCons_  (cEval_ a d) (cEval_ n d)
                                         (cEval_ x d) (cEval_ xs d)

cEval_ (Refl_ a x)       d  = VRefl_ (cEval_ a d) (cEval_ x d)

cEval_ (FZero_ n)    d  = VFZero_ (cEval_ n d)
cEval_ (FSucc_ n f)  d  = VFSucc_ (cEval_ n d) (cEval_ f d)

iEval_ :: ITerm_ -> (NameEnv Value_,Env_) -> Value_
iEval_ (Ann_  c _)       d  = cEval_ c d

iEval_ Star_          _d  = VStar_
iEval_ (Pi_ ty ty')    d0 = VPi_ (cEval_ ty d0) (\x -> cEval_ ty' ((\(e, d) -> (e, x : d)) d0))

iEval_ (Free_  x)      d  = fromMaybe (vfree_ x) (lookup x (fst d))
iEval_ (Bound_  ii)    d  = snd d !! ii
iEval_ (i :$: c)       d  = vapp_ (iEval_ i d) (cEval_ c d)

iEval_ Nat_                 _d  = VNat_
iEval_ (NatElim_ m mz ms n0) d
    = let mzVal = cEval_ mz d
          msVal = cEval_ ms d
          rec nVal =
              case nVal of
                VZero_       -> mzVal
                VSucc_ k     -> msVal `vapp_` k `vapp_` rec k
                VNeutral_ n  -> VNeutral_
                                 (NNatElim_ (cEval_ m d) mzVal msVal n)
                _            -> error "internal: eval natElim"
       in   rec (cEval_ n0 d)

iEval_ (Vec_ a n)                 d  = VVec_ (cEval_ a d) (cEval_ n d)

iEval_ (VecElim_ a m mn mc n0 xs0) d  =
    let mnVal = cEval_ mn d
        mcVal = cEval_ mc d
        rec nVal xsVal =
          case xsVal of
             VNil_ _          -> mnVal
             VCons_ _ k x xs  -> foldl vapp_ mcVal [k, x, xs, rec k xs]
             VNeutral_ n      -> VNeutral_
                                  (NVecElim_  (cEval_ a d) (cEval_ m d)
                                              mnVal mcVal nVal n)
             _                -> error "internal: eval vecElim"
    in   rec (cEval_ n0 d) (cEval_ xs0 d)

iEval_ (Eq_ a x y)                d  = VEq_ (cEval_ a d) (cEval_ x d) (cEval_ y d)
iEval_ (EqElim_ a m mr x y eq)    d  =
    let mrVal = cEval_ mr d
        rec eqVal =
           case eqVal of
             VRefl_ _ z -> mrVal `vapp_` z
             VNeutral_ n ->
               VNeutral_ (NEqElim_  (cEval_ a d) (cEval_ m d) mrVal
                                    (cEval_ x d) (cEval_ y d) n)
             _ -> error "internal: eval eqElim"
    in   rec (cEval_ eq d)

iEval_ (Fin_ n)                d  = VFin_ (cEval_ n d)
iEval_ (FinElim_ m mz ms n f)  d  =
    let mzVal = cEval_ mz d
        msVal = cEval_ ms d
        rec fVal =
           case fVal of
             VFZero_ k        -> mzVal `vapp_` k
             VFSucc_ k g      -> foldl vapp_ msVal [k, g, rec g]
             VNeutral_ n'     -> VNeutral_
                                  (NFinElim_  (cEval_ m d) (cEval_ mz d)
                                              (cEval_ ms d) (cEval_ n d) n')
             _                -> error "internal: eval finElim"
    in   rec (cEval_ f d)

iSubst_ :: Int -> ITerm_ -> ITerm_ -> ITerm_
iSubst_ ii i'   (Ann_ c c')     = Ann_ (cSubst_ ii i' c) (cSubst_ ii i' c')

iSubst_  _ _  Star_           = Star_
iSubst_ ii r  (Pi_ ty ty')    = Pi_  (cSubst_ ii r ty) (cSubst_ (ii + 1) r ty')

iSubst_ ii i' (Bound_ j)      = if ii == j then i' else Bound_ j
iSubst_  _ _  (Free_ y)       = Free_ y
iSubst_ ii i' (i :$: c)       = iSubst_ ii i' i :$: cSubst_ ii i' c

iSubst_  _ _  Nat_            = Nat_
iSubst_ ii r  (NatElim_ m mz ms _n)
                                = NatElim_ (cSubst_ ii r m)
                                            (cSubst_ ii r mz) (cSubst_ ii r ms)
                                            (cSubst_ ii r ms)

iSubst_ ii r  (Vec_ a n)      = Vec_ (cSubst_ ii r a) (cSubst_ ii r n)
iSubst_ ii r  (VecElim_ a m mn mc n xs)
                              = VecElim_ (cSubst_ ii r a) (cSubst_ ii r m)
                                         (cSubst_ ii r mn) (cSubst_ ii r mc)
                                         (cSubst_ ii r n) (cSubst_ ii r xs)

iSubst_ ii r  (Eq_ a x y)     = Eq_ (cSubst_ ii r a)
                                       (cSubst_ ii r x) (cSubst_ ii r y)
iSubst_ ii r  (EqElim_ a m mr x y eq)
                              = VecElim_ (cSubst_ ii r a) (cSubst_ ii r m)
                                         (cSubst_ ii r mr) (cSubst_ ii r x)
                                         (cSubst_ ii r y) (cSubst_ ii r eq)

iSubst_ ii r  (Fin_ n)        = Fin_ (cSubst_ ii r n)
iSubst_ ii r  (FinElim_ m mz ms n f)
                              = FinElim_ (cSubst_ ii r m)
                                         (cSubst_ ii r mz) (cSubst_ ii r ms)
                                         (cSubst_ ii r n) (cSubst_ ii r f)

cSubst_ :: Int -> ITerm_ -> CTerm_ -> CTerm_
cSubst_ ii i' (Inf_ i)      = Inf_ (iSubst_ ii i' i)
cSubst_ ii i' (Lam_ c)      = Lam_ (cSubst_ (ii + 1) i' c)

cSubst_  _ _  Zero_         = Zero_
cSubst_ ii r  (Succ_ n)     = Succ_ (cSubst_ ii r n)

cSubst_ ii r  (Nil_ a)      = Nil_ (cSubst_ ii r a)
cSubst_ ii r  (Cons_ a _n x xs)
                            = Cons_ (cSubst_ ii r a) (cSubst_ ii r x)
                                    (cSubst_ ii r x) (cSubst_ ii r xs)

cSubst_ ii r  (Refl_ a x)   = Refl_ (cSubst_ ii r a) (cSubst_ ii r x)

cSubst_ ii r  (FZero_ n)    = FZero_ (cSubst_ ii r n)
cSubst_ ii r  (FSucc_ n k)  = FSucc_ (cSubst_ ii r n) (cSubst_ ii r k)

quote_ :: Int -> Value_ -> CTerm_
quote_ ii (VLam_ t)
    =    Lam_ (quote_ (ii + 1) (t (vfree_ (Quote ii))))


quote_  _ VStar_ = Inf_ Star_
quote_ ii (VPi_ v f)
    = Inf_ (Pi_ (quote_ ii v) (quote_ (ii + 1) (f (vfree_ (Quote ii)))))

quote_ ii (VNeutral_ n)
    =    Inf_ (neutralQuote_ ii n)

quote_  _ VNat_       = Inf_ Nat_
quote_  _ VZero_      = Zero_
quote_ ii (VSucc_ n)  = Succ_ (quote_ ii n)

quote_ ii (VVec_ a n)         = Inf_ (Vec_ (quote_ ii a) (quote_ ii n))
quote_ ii (VNil_ a)           = Nil_ (quote_ ii a)
quote_ ii (VCons_ a n x xs)   = Cons_  (quote_ ii a) (quote_ ii n)
                                          (quote_ ii x) (quote_ ii xs)

quote_ ii (VEq_ a x y)  = Inf_ (Eq_ (quote_ ii a) (quote_ ii x) (quote_ ii y))
quote_ ii (VRefl_ a x)  = Refl_ (quote_ ii a) (quote_ ii x)

quote_ ii (VFin_ n)           = Inf_ (Fin_ (quote_ ii n))
quote_ ii (VFZero_ n)         = FZero_ (quote_ ii n)
quote_ ii (VFSucc_ n f)       = FSucc_  (quote_ ii n) (quote_ ii f)

neutralQuote_ :: Int -> Neutral_ -> ITerm_
neutralQuote_ ii (NFree_ v)
     = boundfree_ ii v
neutralQuote_ ii (NApp_ n v)
     = neutralQuote_ ii n :$: quote_ ii v

neutralQuote_ ii (NNatElim_ m z s n)
     = NatElim_ (quote_ ii m) (quote_ ii z) (quote_ ii s) (Inf_ (neutralQuote_ ii n))

neutralQuote_ ii (NVecElim_ a m mn mc n xs)
     = VecElim_ (quote_ ii a) (quote_ ii m)
                (quote_ ii mn) (quote_ ii mc)
                (quote_ ii n) (Inf_ (neutralQuote_ ii xs))

neutralQuote_ ii (NEqElim_ a m mr x y eq)
     = EqElim_  (quote_ ii a) (quote_ ii m) (quote_ ii mr)
                (quote_ ii x) (quote_ ii y)
                (Inf_ (neutralQuote_ ii eq))

neutralQuote_ ii (NFinElim_ m mz ms n f)
     = FinElim_ (quote_ ii m)
                (quote_ ii mz) (quote_ ii ms)
                (quote_ ii n) (Inf_ (neutralQuote_ ii f))

boundfree_ :: Int -> Name -> ITerm_
boundfree_ ii (Quote k) = Bound_ ((ii - k - 1) `max` 0)
boundfree_  _ x         = Free_ x

instance Show Value_ where
    show = show . quote0_

type Type_     = Value_
type Context_  = [(Name, Type_)]

quote0_ :: Value_ -> CTerm_
quote0_  = quote_ 0

iType0_ :: (NameEnv Value_,Context_) -> ITerm_ -> Result Type_
iType0_ = iType_ 0

iType_ :: Int -> (NameEnv Value_,Context_) -> ITerm_ -> Result Type_
iType_ ii g (Ann_ e tyt )
    =    do cType_ ii g tyt VStar_
            let ty = cEval_ tyt (fst g, [])
            cType_ ii g e ty
            return ty
iType_  _ _ Star_
    = return VStar_
iType_ ii g (Pi_ tyt tyt')
    = do cType_ ii g tyt VStar_
         let ty = cEval_ tyt (fst g, [])
         cType_  (ii + 1) ((\(d,g') -> (d,  (Local ii, ty) : g')) g)
                 (cSubst_ 0 (Free_ (Local ii)) tyt') VStar_
         return VStar_
iType_  _ g (Free_ x)
    =    case lookup x (snd g) of
            Just ty        -> return ty
            Nothing        -> throwError ("unknown identifier: " ++ PP.render (iPrint_ 0 0 (Free_ x)))
iType_ ii g (e1 :$: e2)
    =    do si <- iType_ ii g e1
            case si of
                VPi_  ty ty'  -> do cType_ ii g e2 ty
                                    return ( ty' (cEval_ e2 (fst g, [])))
                _             -> throwError "illegal application"

iType_  _ _ Nat_                  = return VStar_
iType_ ii g (NatElim_ m mz ms n)  =
    do cType_ ii g m (VPi_ VNat_ (const VStar_))
       let mVal = cEval_ m (fst g, [])
       cType_ ii g mz (mVal `vapp_` VZero_)
       cType_ ii g ms (VPi_ VNat_ (\k -> VPi_ (mVal `vapp_` k) (\_ -> mVal `vapp_` VSucc_ k)))
       cType_ ii g n VNat_
       let nVal = cEval_ n (fst g, [])
       return (mVal `vapp_` nVal)

iType_ ii g (Vec_ a n) =
    do cType_ ii g a  VStar_
       cType_ ii g n  VNat_
       return VStar_
iType_ ii g (VecElim_ a m mn mc n vs) =
    do cType_ ii g a VStar_
       let aVal = cEval_ a (fst g, [])
       cType_ ii g m
         (  VPi_ VNat_ (\n' -> VPi_ (VVec_ aVal n') (const VStar_)))
       let mVal = cEval_ m (fst g, [])
       cType_ ii g mn (foldl vapp_ mVal [VZero_, VNil_ aVal])
       cType_ ii g mc
          (VPi_ VNat_ (\n' ->
           VPi_ aVal (\y ->
           VPi_ (VVec_ aVal n') (\ys ->
           VPi_ (foldl vapp_ mVal [n', ys]) (const
           (foldl vapp_ mVal [VSucc_ n', VCons_ aVal n' y ys]))))))
       cType_ ii g n VNat_
       let nVal = cEval_ n (fst g, [])
       cType_ ii g vs (VVec_ aVal nVal)
       let vsVal = cEval_ vs (fst g, [])
       return (foldl vapp_ mVal [nVal, vsVal])

iType_ i g (Eq_ a x y) =
    do cType_ i g a VStar_
       let aVal = cEval_ a (fst g, [])
       cType_ i g x aVal
       cType_ i g y aVal
       return VStar_
iType_ i g (EqElim_ a m mr x y eq) =
    do cType_ i g a VStar_
       let aVal = cEval_ a (fst g, [])
       cType_ i g m
         (VPi_ aVal (\x' ->
          VPi_ aVal (\y' ->
          VPi_ (VEq_ aVal x' y') (const VStar_))))
       let mVal = cEval_ m (fst g, [])
       cType_ i g mr
         (VPi_ aVal (\x' ->
          foldl vapp_ mVal [x', x']))
       cType_ i g x aVal
       let xVal = cEval_ x (fst g, [])
       cType_ i g y aVal
       let yVal = cEval_ y (fst g, [])
       cType_ i g eq (VEq_ aVal xVal yVal)
       -- let eqVal = cEval_ eq (fst g, [])
       return (foldl vapp_ mVal [xVal, yVal])

cType_ :: Int -> (NameEnv Value_,Context_) -> CTerm_ -> Type_ -> Result ()
cType_ ii g (Inf_ e) v
    = do v' <- iType_ ii g e
         unless ( quote0_ v == quote0_ v') (throwError ("type mismatch:\n" ++ "type inferred:  " ++ PP.render (cPrint_ 0 0 (quote0_ v')) ++ "\n" ++ "type expected:  " ++ PP.render (cPrint_ 0 0 (quote0_ v)) ++ "\n" ++ "for expression: " ++ PP.render (iPrint_ 0 0 e)))
cType_ ii g0 (Lam_ e) ( VPi_ ty ty')
    =    cType_  (ii + 1) ((\(d,g) -> (d,  (Local ii, ty ) : g)) g0)
                 (cSubst_ 0 (Free_ (Local ii)) e) ( ty' (vfree_ (Local ii)))

cType_  _ _ Zero_      VNat_  = return ()
cType_ ii g (Succ_ k)  VNat_  = cType_ ii g k VNat_

cType_ ii g (Nil_ a) (VVec_ bVal VZero_) =
    do cType_ ii g a VStar_
       let aVal = cEval_ a (fst g, [])
       unless  (quote0_ aVal == quote0_ bVal)
               (throwError "type mismatch")
cType_ ii g (Cons_ a n x xs) (VVec_ bVal (VSucc_ k)) =
    do cType_ ii g a VStar_
       let aVal = cEval_ a (fst g, [])
       unless  (quote0_ aVal == quote0_ bVal)
               (throwError "type mismatch")
       cType_ ii g n VNat_
       let nVal = cEval_ n (fst g, [])
       unless  (quote0_ nVal == quote0_ k)
               (throwError "number mismatch")
       cType_ ii g x aVal
       cType_ ii g xs (VVec_ bVal k)

cType_ ii g (Refl_ a z) (VEq_ bVal xVal yVal) =
    do cType_ ii g a VStar_
       let aVal = cEval_ a (fst g, [])
       unless  (quote0_ aVal == quote0_ bVal)
               (throwError "type mismatch")
       cType_ ii g z aVal
       let zVal = cEval_ z (fst g, [])
       unless  (quote0_ zVal == quote0_ xVal && quote0_ zVal == quote0_ yVal)
               (throwError "type mismatch")

cType_ _ _ _ _
    =    throwError "type mismatch"
{-# ANN cType_ ("HLint: ignore Reduce duplication" :: Prelude.String) #-}

data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero n     = n
plus (Succ k) n = Succ (plus k n)
