
{-# LANGUAGE PackageImports #-}

-- module Transformers where

import           "mtl" Control.Monad.Identity
import           "mtl" Control.Monad.Error
import           "mtl" Control.Monad.Reader
import           "mtl" Control.Monad.State
import           "mtl" Control.Monad.Writer

import                 Data.Maybe
import qualified       Data.Map as Map

import qualified       Test.HUnit      as T
import qualified       Test.HUnit.Util as U
import                 System.IO.Unsafe -- for one unit test

type Name   =  String                -- variable names

data Exp    =  Lit  Integer          -- expressions
            |  Var  Name
            |  Plus Exp  Exp
            |  Abs  Name Exp
            |  App  Exp  Exp
            deriving (Eq, Show)

data Value  =  IntVal Integer        -- values
            |  FunVal Env Name Exp
            deriving (Eq, Show)

type Env    =  Map.Map Name Value    -- from names to values

eval0                 :: Env -> Exp -> Value
eval0 env (Lit i)      = IntVal i
eval0 env (Var n)      = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let  IntVal i1  = eval0 env e1
                              IntVal i2  = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs  n  e)  = FunVal env n e
eval0 env (App  e1 e2) = let  val1  = eval0 env e1
                              val2  = eval0 env e2
                         in case val1 of
                              FunVal env' n body ->
                                  eval0 (Map.insert n val2 env') body

exampleExp = Plus (Lit 12) (App (Abs "x" (Var "x")) (Plus (Lit 4) (Lit 2)))

t00 = U.t "t00"
     (eval0 Map.empty exampleExp)
     (IntVal 18)

t01 = U.e "t01"
     (eval0 Map.empty (Var "x"))
     "Maybe.fromJust: Nothing"

eval0e                 :: Env -> Exp -> Either String Value
eval0e env (Lit  i)     = Right $ IntVal i
eval0e env (Var  n)     = case Map.lookup n env of
                              Nothing -> Left $ "unbound var: " ++ n
                              Just v  -> Right v
eval0e env (Plus e1 e2) = let  Right (IntVal i1)  = eval0e env e1
                               Right (IntVal i2)  = eval0e env e2
                          in Right $ IntVal (i1 + i2)
eval0e env (Abs  n  e)  = Right $ FunVal env n e
eval0e env (App  e1 e2) = let  Right val1  = eval0e env e1
                               Right val2  = eval0e env e2
                          in case val1 of
                              FunVal env' n body ->
                                  eval0e (Map.insert n val2 env') body

t0e00 = U.t "t0e00"
     (eval0e Map.empty (Var "x"))
     (Left "unbound var: x")

type Eval1 alpha  =   Identity alpha

runEval1          ::  Eval1 alpha -> alpha
runEval1 ev       =   runIdentity ev

eval1                 :: Env -> Exp -> Eval1 Value
eval1 env (Lit  i)     = return $ IntVal i
eval1 env (Var  n)     = return $ fromJust (Map.lookup n env)
eval1 env (Plus e1 e2) = do  IntVal i1  <- eval1 env e1
                             IntVal i2  <- eval1 env e2
                             return $ IntVal (i1 + i2)
eval1 env (Abs  n  e)  = return $ FunVal env n e
eval1 env (App  e1 e2) = do  val1  <- eval1 env e1
                             val2  <- eval1 env e2
                             case val1 of
                                 FunVal env' n body ->
                                     eval1 (Map.insert n val2 env') body

t10 = U.t "t10"
     (runEval1 (eval1 Map.empty exampleExp))
     (IntVal 18)

t11 = U.e "t11"
     (runEval1 (eval1 Map.empty (Var "x")))
     "Maybe.fromJust: Nothing"

-- String is the type arg to ErrorT : the type of exceptions in example
type Eval2 alpha = ErrorT String Identity alpha

runEval2     :: Eval2 alpha -> Either String alpha
runEval2 ev  = runIdentity (runErrorT ev)

eval2a                 :: Env -> Exp -> Eval2 Value
eval2a env (Lit  i)     = return $ IntVal i

-- eval1 / eval2a diff:
eval2a env (Var  n)     = case Map.lookup n env of
                              Nothing -> fail $ "unbound var: " ++ n
                              Just v  -> return v

eval2a env (Plus e1 e2) = do  IntVal i1  <- eval2a env e1
                              IntVal i2  <- eval2a env e2
                              return $ IntVal (i1 + i2)
eval2a env (Abs  n  e)  = return $ FunVal env n e
eval2a env (App  e1 e2) = do  val1  <- eval2a env e1
                              val2  <- eval2a env e2
                              case val1 of
                                  FunVal env' n body -> eval2a (Map.insert n val2 env') body

t2a0 = U.t "t2a0"
     (runEval2 (eval2a Map.empty exampleExp))
     (Right (IntVal 18))

t2a1 = U.t "t2a1"
     (runEval2 (eval2a Map.empty (Var "no-way")))
     (Left "unbound var: no-way")

t2a2 = U.t "t2a2"
     -- 12 + (\x -> x)
     (runEval2 (eval2a Map.empty (Plus (Lit 12) (Abs "x" (Var "x")))))
     (Left "Pattern match failure in do expression at concrete-monads-0-motivating-example.hs:117:31-39")

eval2b                 :: Env -> Exp -> Eval2 Value
eval2b env (Lit  i)     = return $ IntVal i
eval2b env (Var  n)     = case Map.lookup n env of
                              Nothing -> fail $ "unbound var: " ++ n
                              Just v  -> return v
eval2b env (Plus e1 e2) = do  e1'  <- eval2b env e1
                              e2'  <- eval2b env e2
                              -- eval2a / eval2b diff:
                              case (e1', e2') of
                                  (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                  _                      -> throwError "type error in Plus"
eval2b env (Abs  n  e)  = return $ FunVal env n e
eval2b env (App  e1 e2) = do  val1  <- eval2b env e1
                              val2  <- eval2b env e2
                              -- eval2a / eval2b diff:
                              case val1 of
                                  FunVal env' n body -> eval2b (Map.insert n val2 env') body
                                  _                  -> throwError "type error in App"

t2b0 = U.t "t2b0"
     (runEval2 (eval2b Map.empty (Plus (Lit 12) (Abs "x" (Var "x")))))
     (Left "type error in Plus")

t2b1 = U.t "t2b1"
     (runEval2 (eval2b Map.empty (App (Lit 12) (Lit 0))))
     (Left "type error in App")

type Eval3 alpha = ReaderT Env (ErrorT String Identity) alpha

runEval3     :: Env -> Eval3 alpha -> Either String alpha
runEval3 env ev  = runIdentity (runErrorT (runReaderT ev env))

eval3             :: Exp -> Eval3 Value
eval3 (Lit  i)     = return $ IntVal i
eval3 (Var  n)     = do env <- ask                -- eval2b / eval3 diff
                        case Map.lookup n env of
                            Nothing  -> throwError ("unbound variable: " ++ n)
                            Just val -> return val
eval3 (Plus e1 e2) = do e1'  <- eval3 e1
                        e2'  <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in Plus"
eval3 (Abs  n  e)  = do env <- ask
                        return $ FunVal env n e
eval3 (App  e1 e2) = do val1  <- eval3 e1
                        val2  <- eval3 e2
                        case val1 of
                                                  -- eval2b / eval3 diff
                            FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                            _                  -> throwError "type error in App"

t30 = U.t "t30"
     (runEval3 Map.empty (eval3 exampleExp))
     (Right (IntVal 18))

type Eval4 alpha = ReaderT Env (ErrorT String (StateT Integer Identity)) alpha

-- returns evaluation result (error or value) and state
-- give initial state arg for flexibility
runEval4            ::  Env -> Integer -> Eval4 alpha -> (Either String alpha, Integer)
runEval4 env st ev  =   runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

-- tick type not same as =Eval4= so it can reused elsewhere.
tick :: (Num s, MonadState s m) => m ()
tick = do  st <- get
           put (st + 1)

-- eval4          :: Exp -> Eval4 Value
eval4 (Lit i)      = do tick
                        return $ IntVal i
eval4 (Var n)      = do tick
                        env <- ask
                        case Map.lookup n env of
                            Nothing -> throwError ("unbound variable: " ++ n)
                            Just val -> return val
eval4 (Plus e1 e2) = do tick
                        e1'  <- eval4 e1
                        e2'  <- eval4 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) ->
                                return $ IntVal (i1 + i2)
                            _ -> throwError "type error in addition"
eval4 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval4 (App e1 e2)  = do tick
                        val1  <- eval4 e1
                        val2  <- eval4 e2
                        case val1 of
                            FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
                            _ -> throwError "type error in application"

t40 = U.t "t40"
     (runEval4 Map.empty 0 (eval4 exampleExp))
     (Right (IntVal 18),8) -- 8 reduction steps

type Eval5 alpha = ReaderT Env  (ErrorT String (WriterT [String] (StateT Integer Identity))) alpha

runEval5            ::  Env -> Integer -> Eval5 alpha -> ((Either String alpha, [String]), Integer)
runEval5 env st ev  =   runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

eval5             :: Exp -> Eval5 Value
eval5 (Lit i)      = do tick
                        return $ IntVal i
eval5 (Var n)      = do tick
                        -- eval4 / eval5 diff
                        tell [n] -- collect name of each var encountered during evaluation
                        env <- ask
                        case Map.lookup n env of
                            Nothing  -> throwError ("unbound variable: " ++ n)
                            Just val -> return val
eval5 (Plus e1 e2) = do tick
                        e1'  <- eval5 e1
                        e2'  <- eval5 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval5 (Abs n e)     = do tick
                         env <- ask
                         return $ FunVal env n e
eval5 (App e1 e2)   = do tick
                         val1  <- eval5 e1
                         val2  <- eval5 e2
                         case val1 of
                             FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body)
                             _                  -> throwError "type error in application"

t50 = U.t "t50"
      (runEval5 Map.empty 0 (eval5 exampleExp))
      ((Right (IntVal 18),["x"]),8)

type Eval6 alpha = ReaderT Env  (ErrorT String (WriterT [String] (StateT Integer IO))) alpha

runEval6           ::  Env -> Integer -> Eval6 alpha -> IO ((Either String alpha, [String]), Integer)
runEval6 env st ev  =  runStateT (runWriterT (runErrorT (runReaderT ev env))) st

eval6             :: Exp -> Eval6 Value
eval6 (Lit  i)     = do tick
                        -- eval5 / eval 6 diff
                        -- must use =liftIO= to lift into the currently running monad
                        liftIO $ print i -- print each int when evaluated
                        return $ IntVal i
eval6 (Var  n)     = do tick
                        tell [n]
                        env <- ask
                        case Map.lookup n env of
                            Nothing  -> throwError ("unbound variable: " ++ n)
                            Just val -> return val
eval6 (Plus e1 e2) = do tick
                        e1'  <- eval6 e1
                        e2'  <- eval6 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval6 (Abs  n  e)  = do tick
                        env <- ask
                        return $ FunVal env n e
eval6 (App  e1 e2) = do tick
                        val1  <- eval6 e1
                        val2  <- eval6 e2
                        case val1 of
                            FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                            _                  -> throwError "type error in application"

t60 = U.t "t60"
      (unsafePerformIO (runEval6 Map.empty 0 (eval6 exampleExp)))
      ((Right (IntVal 18),["x"]),8)

runTests =
    T.runTestTT $ T.TestList $ t00   ++ t01  ++
                               t0e00 ++
                               t10   ++ t11  ++
                               t2a0  ++ t2a1 ++ t2a2 ++
                               t2b0  ++ t2b1 ++
                               t30   ++
                               t40   ++
                               t50   ++ t60
