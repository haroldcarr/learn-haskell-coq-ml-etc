{-# LANGUAGE LambdaCase #-}

module Repl where

------------------------------------------------------------------------------
import           Parse
import           PiEval
import           PiLib
import           PiTypes
import           Print
import           STEval
import           STTypes
------------------------------------------------------------------------------
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                       hiding (print)
import qualified System.IO                     as SIO
import           Text.ParserCombinators.Parsec hiding (State, parse)
import qualified Text.PrettyPrint.HughesPJ     as PP
------------------------------------------------------------------------------

--  read-eval-print loop
readevalprint :: Interpreter i c v t tinf inf -> State v inf -> IO ()
readevalprint int0 state0@(interactive, _out, _ve, _te) = do
  when interactive $
    putStrLn ("Interpreter for " ++ iname int0 ++ ".\n" ++ "Type :? for help.")
  loop int0 state0
 where
  loop int state = do
    putStr (iprompt int); SIO.hFlush SIO.stdout
    x0 <- Just <$> getLine
    case x0 of
      Nothing -> pure ()
      Just "" -> loop int state
      Just x  -> do
        -- when inter (addHistory x x) -- HC/TODO
        c      <- interpretCommand x
        state' <- handleCommand int state c
        maybe (pure ()) (loop int) state'

data Command
  = TypeOf  String
  | Compile CompileForm
  | Browse
  | Quit
  | Help
  | Noop

data CompileForm
  = CompileInteractive  String
  | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":type"]      "<expr>"  TypeOf                  "print type of expression"
  , Cmd [":browse"]    ""        (const Browse)          "browse names in scope"
  , Cmd [":load"]      "<file>"  (Compile . CompileFile) "load program from file"
  , Cmd [":quit"]      ""        (const Quit)            "exit interpreter"
  , Cmd [":help",":?"] ""        (const Help)            "display this list of commands" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs0 =
  "List of commands:  Any command may be abbreviated to :c where\n" ++
  "c is the first character in the full name.\n\n" ++
  "<expr>                  evaluate expression\n" ++
  "let <var> = <expr>      define variable\n" ++
  "assume <var> :: <expr>  assume variable\n\n"
  ++
  unlines (map (\(Cmd cs a _ d) ->
                   let ct = intercalate ", " (map (++ if null a then "" else " " ++ a) cs)
                    in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs0)

interpretCommand :: String -> IO Command
interpretCommand x =
  if ":" `isPrefixOf` x then do
    let (cmd,t0) = break     isSpace x
        t        = dropWhile isSpace t0
    --  find matching commands
        matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
       []  -> do
         putStrLn ("Unknown command `" ++ cmd ++ "'. Type :? for help.")
         pure Noop
       [Cmd _ _ f _] ->
         pure (f t)
       _x  -> do
         putStrLn (   "Ambiguous command, could be "
                   ++ intercalate ", " [ head cs | Cmd cs _ _ _ <- matching ] ++ "." )
         pure Noop
  else
     pure (Compile (CompileInteractive x))

handleCommand :: Interpreter i c v t tinf inf -> State v inf -> Command -> IO (Maybe (State v inf))
handleCommand int state@(inter, _out, ve, te) = \case
  Quit      -> unless inter (putStrLn "!@#$^&*") >> pure Nothing
  Noop      -> pure (Just state)
  Help      -> putStr (helpTxt commands) >> pure (Just state)
  TypeOf x0 -> do
               x <- parseIO "<interactive>" (iiparse int) x0
               case x of
                 Nothing -> pure ()
                 Just x' -> case iinfer int ve te x' of
                              Left actions -> doActions actions
                              Right  t     -> (putStrLn . PP.render . itprint int) t
               pure (Just state)
  Browse    -> do
               putStr (unlines [ s | Global s <- reverse (nub (map fst te)) ])
               pure (Just state)
  Compile c -> do
               state' <- case c of
                           CompileInteractive s -> compilePhrase int state s
                           CompileFile f        -> compileFile int state f
               pure (Just state')

compileFile :: Interpreter i c v t tinf inf -> State v inf -> String -> IO (State v inf)
compileFile int state f = do
  x     <- readFile f
  stmts <- parseIO f (many (isparse int)) x
  maybe (error "compileFile") {-(pure state)-} (foldM (handleStmt int) state) stmts

compilePhrase :: Interpreter i c v t tinf inf -> State v inf -> String -> IO (State v inf)
compilePhrase int state x0 = do
  x <- parseIO "<interactive>" (isparse int) x0
  maybe (error "compilePhrase") {-(pure state)-} (handleStmt int state) x

data Interpreter i c v t tinf inf =
    I { iname    :: String
      , iprompt  :: String
      , iitype   :: NameEnv v -> Ctx inf -> i -> Either String t
      , iquote   :: v -> c
      , ieval    :: NameEnv v -> i -> v
      , ihastype :: t -> inf
      , icprint  :: c -> PP.Doc
      , itprint  :: t -> PP.Doc
      , iiparse  :: CharParser () i
      , isparse  :: CharParser () (Stmt i tinf)
      , iassume  :: State v inf -> (String, tinf) -> ([Action], State v inf) }

st :: Interpreter ITerm CTerm Value Type Info Info
st = I { iname    = "the simply typed lambda calculus"
       , iprompt  = "ST> "
       , iitype   = \_v c -> iType 0 c
       , iquote   = quote0
       , ieval    = \e x -> iEval x (e, [])
       , ihastype = HasType
       , icprint  = cPrint 0 0
       , itprint  = tPrint 0
       , iiparse  = parseITerm 0 []
       , isparse  = parseStmt []
       , iassume  = \s (x, t) -> stassume s x t }

lp :: Interpreter ITerm_ CTerm_ Value_ Value_ CTerm_ Value_
lp = I { iname    = "lambda-Pi"
       , iprompt  = "LP> "
       , iitype   = curry (iType_ 0)
       , iquote   = quote0_
       , ieval    = \e x -> iEval_ x (e, [])
       , ihastype = id
       , icprint  = cPrint_ 0 0
       , itprint  = cPrint_ 0 0 . quote0_
       , iiparse  = parseITerm_ 0 []
       , isparse  = parseStmt_ []
       , iassume  = \s (x, t) -> lpassume s x t }

repLP :: Bool -> IO ()
repLP b = readevalprint lp (b, [], lpve, lpte)

repST :: Bool -> IO ()
repST b = readevalprint st (b, [], [], [])

iinfer
  :: Interpreter i c v a tinf inf
  -> NameEnv v
  -> Ctx inf
  -> i
  -> Either [Action] a
iinfer int d g t = case iitype int d g t of
  Left  e -> Left  [APutStrLn e]
  Right v -> Right v

data Action
  = APutStrLn  String
  | AWriteFile String String
  deriving (Eq, Show)

doActions :: [Action] -> IO ()
doActions actions =
  forM_ actions $ \case
    APutStrLn s      -> putStrLn s
    AWriteFile _o _s -> undefined

handleStmt
  :: Interpreter i c v t tinf inf
  -> State v inf
  -> Stmt i tinf
  -> IO (State v inf)
handleStmt int state stmt = do
  let (actions, state') = handleStmtPure int state stmt
  doActions actions
  pure state'

handleStmtPure
  :: Interpreter i c v t tinf inf
  -> State v inf
  -> Stmt i tinf
  -> ([Action], State v inf)
handleStmtPure int state@(inter, out, ve, te) = \case
  Assume ass -> foldM (iassume int) state ass
  Let x e    -> checkEval x e
  Eval e     -> checkEval it e
  PutStrLn x -> ([APutStrLn x], state)
  Out f      -> pure (inter, f, ve, te)
 where
  -- checkEval :: String -> i -> IO (State v inf)
  checkEval i t =
    check int state i t
      (\(y, v) ->
          --  TODO : the bound identifier *and* the result of evaluation
          let outtext =
                if i == it
                then PP.render (icprint int (iquote int v) <> PP.text " :: " <> itprint int y)
                else PP.render (PP.text i <> PP.text " :: " <> itprint int y)
           in APutStrLn outtext : if null out then [] else [AWriteFile out (process outtext)])
      (\(y, v) ->
          (inter, "", (Global i, v) : ve, (Global i, ihastype int y) : te))

check
  :: Interpreter i c v t tinf inf
  -> State v inf
  -> String
  -> i
  -> ((t, v) -> [Action])
  -> ((t, v) -> State v inf)
  -> ([Action], State v inf)
check int state@(_inter, _out, ve, te) _i t kp k =
  --  typecheck and evaluate
  case iinfer int ve te t of
    Left actions -> (actions ++ [APutStrLn "type error"], state)
    Right y      -> do
      let v = ieval int ve t
      (kp (y, v), k (y, v))

stassume
  :: State v inf
  -> String
  -> inf
  -> ([Action], State v inf)
stassume (inter, out, ve, te) x t =
  ([], (inter, out, ve, (Global x, t) : te))

lpassume :: State Value_ Value_ -> String -> CTerm_ -> ([Action], State Value_ Value_)
lpassume state@(inter, out, ve, te) x t =
  check lp state x (Ann_ t (Inf_ Star_))
        (\(_, v) -> [APutStrLn (PP.render (PP.text x <> PP.text " :: " <> cPrint_ 0 0 (quote0_ v)))])
        (\(_, v) -> (inter, out, ve, (Global x, v) : te))

it :: String
it  = "it"

process :: String -> String
process = unlines . map (\x -> "< " ++ x) . lines

mainST :: IO ()
mainST = repST True

mainLP :: IO ()
mainLP = repLP True

