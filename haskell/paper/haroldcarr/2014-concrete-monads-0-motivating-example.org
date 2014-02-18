{-
Created       : 2013 Dec 01 (Sun) 11:18:09 by carr.
Last Modified : 2014 Feb 11 (Tue) 12:40:49 by Harold Carr.

http://www.cs.virginia.edu/~wh5a/personal/Transformers.lhs

About *using* monad transformers.

Monads for flexible, extensible structuring of programs.

Hides book-keeping/plumbing in monads, removing clutter from main algorithm.

Monad transformers : for composing monad transformers.
-}

------------------------------------------------------------------------------
-- ** setup

{-# LANGUAGE PackageImports #-}

-- module Transformers where

import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map


import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U
import System.IO.Unsafe -- for one unit test

------------------------------------------------------------------------------
-- ** non-monadic

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

-- 12 + (\x -> x) (4 + 2)
exampleExp = Plus (Lit 12) (App (Abs "x" (Var "x")) (Plus (Lit 4) (Lit 2)))

t0 = U.t "t0"
     (eval0 Map.empty exampleExp)
     (IntVal 18)

------------------------------------------------------------------------------
-- ** conversion to monadic structure

{-
Conversion to monadic form : use =return= to wrap results; sequencing via =do= (i.e., =bind=).

Once restructured then relatively easy to add, remove or change monads.

=Control.Monad.Identity= defines =return=, =>>==, =runIdentity=, etc.,
Used as "base case" around which other monad transformers can be
wrapped.
-}

type Eval1 alpha  =   Identity alpha

-- For readability:
runEval1          ::  Eval1 alpha -> alpha
runEval1 ev       =   runIdentity ev

eval1                 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)      = return $ IntVal i
eval1 env (Var n)      = return $ fromJust (Map.lookup n env)
eval1 env (Plus e1 e2) = do  IntVal i1  <- eval1 env e1
                             IntVal i2  <- eval1 env e2
                             return $ IntVal (i1 + i2)
eval1 env (Abs n e)    = return $ FunVal env n e
eval1 env (App e1 e2)  = do  val1  <- eval1 env e1
                             val2  <- eval1 env e2
                             case val1 of
                                 FunVal env' n body ->
                                     eval1 (Map.insert n val2 env') body
{-
=Lit=, =Plus=, =Abs= use =return=

=Var= does not need =fromJust= : =Map.lookup= calls monad's =fail= -- NOT TRUE

=Maybe.fail= is =Nothing=

=Identity.fail= throws exception

=Plus=, =App= use =do=
-}

t1 = U.t "t1"
     (runEval1 (eval1 Map.empty exampleExp))
     (IntVal 18)

{-
------------------------------------------------------------------------------
-- ** adding error handling

evaluation is partial : will terminate with an error message for some inputs (e.g., unbound variables)
-}

-- result of evaluation now =Either String alpha=
type Eval2 alpha = ErrorT String Identity alpha -- String is type arg to ErrorT - type of exceptions in example

runEval2     :: Eval2 alpha -> Either String alpha
runEval2 ev  = runIdentity (runErrorT ev)

eval2a                 :: Env -> Exp -> Eval2 Value
eval2a env (Lit i)      = return $ IntVal i
eval2a env (Var n)      = case (Map.lookup n env) of
                              Nothing -> fail $ "unbound var: " ++ n
                              Just v  -> return v
eval2a env (Plus e1 e2) = do  IntVal i1  <- eval2a env e1
                              IntVal i2  <- eval2a env e2
                              return $ IntVal (i1 + i2)
eval2a env (Abs n e)    = return $ FunVal env n e
eval2a env (App e1 e2)  = do  val1  <- eval2a env e1
                              val2  <- eval2a env e2
                              case val1 of
                                  FunVal env' n body ->
                                      eval2a (Map.insert n val2 env') body

t2 = U.t "t2"
     (runEval2 (eval2a Map.empty exampleExp))
     (Right (IntVal 18))

t3 = U.t "t3"
     (runEval2 (eval2a Map.empty (Var "no-way")))
     (Left "unbound var: no-way")

t4 = U.t "t4"
     (runEval2 (eval2a Map.empty (Plus (Lit 12) (Abs "x" (Var "x")))))
     (Left "Pattern match failure in do expression at transformers.hs:138:34-42")

-- Error reporting of the =ErrorT= not used in =t4=, therefore:

eval2b                 :: Env -> Exp -> Eval2 Value
eval2b env (Lit i)      = return $ IntVal i
eval2b env (Var n)      = case (Map.lookup n env) of
                              Nothing -> fail $ "unbound var: " ++ n
                              Just v  -> return v
eval2b env (Plus e1 e2) = do  e1'  <- eval2b env e1
                              e2'  <- eval2b env e2
                              case (e1', e2') of
                                  (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                  _                      -> throwError "type error in Plus"
eval2b env (Abs n e)    = return $ FunVal env n e
eval2b env (App e1 e2)  = do  val1  <- eval2b env e1
                              val2  <- eval2b env e2
                              case val1 of
                                  FunVal env' n body -> eval2b (Map.insert n val2 env') body
                                  _                  -> throwError "type error in App"

t5 = U.t "t5"
     (runEval2 (eval2b Map.empty (Plus (Lit 12) (Abs "x" (Var "x")))))
     (Left "type error")

-- monadic binding in a |do| expression uses the |fail| function whenever a pattern match fails, so:

eval2c                 :: Env -> Exp -> Eval2 Value
eval2c env (Lit i)      = return $ IntVal i
eval2c env (Var n)      = case (Map.lookup n env) of
                               Nothing -> fail $ "unbound var: " ++ n
                               Just v  -> return v
eval2c env (Plus e1 e2) = do  IntVal i1  <- eval2c env e1
                              IntVal i2  <- eval2c env e2
                              return $ IntVal (i1 + i2)
eval2c env (Abs n e)     = return $ FunVal env n e
eval2c env (App e1 e2)   = do  FunVal env' n body  <- eval2c env e1
                               val2                <- eval2c env e2
                               eval2c (Map.insert n val2 env') body

-- but error message says "pattern match failure" - no specific information
t6 = U.t "t6"
     (runEval2 (eval2c Map.empty (Plus (Lit 12) (Abs "x" (Var "x")))))
     (Left "Pattern match failure in do expression at transformers.hs:189:34-42")


-- provide specific error messages:
eval2                  :: Env -> Exp -> Eval2 Value
eval2  env (Lit i)      = return $ IntVal i
eval2  env (Var n)      = case Map.lookup n env of
                              Nothing  -> throwError ("unbound variable: " ++ n)
                              Just val -> return val
eval2  env (Plus e1 e2) = do  e1'  <- eval2  env e1
                              e2'  <- eval2  env e2
                              case (e1', e2') of
                                  (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                  _                      -> throwError "type error in addition"
eval2  env (Abs n e)    = return $ FunVal env n e
eval2  env (App e1 e2)  = do  val1  <- eval2  env e1
                              val2  <- eval2  env e2
                              case val1 of
                                  FunVal env' n body -> eval2  (Map.insert n val2 env') body
                                  _                  -> throwError "type error in application"

{-
------------------------------------------------------------------------------
-- ** hiding the environment

=Env= only extended in =App= and used in =Var= and =Abs=

So hide it via =ReaderT= : passes a value into a computation and its sub-computations.
Value can be read by all enclosed computations and get modified for nested computations.

In contrast to state monads, an encapsulated computation cannot change
the value used by surrounding computations.
-}

type Eval3 alpha = ReaderT Env (ErrorT String Identity) alpha

runEval3     :: Env -> Eval3 alpha -> Either String alpha
runEval3 env ev  = runIdentity (runErrorT (runReaderT ev env))

eval3             :: Exp -> Eval3 Value
eval3 (Lit i)      = return $ IntVal i
eval3 (Var n)      = do env <- ask
                        case Map.lookup n env of
                            Nothing  -> throwError ("unbound variable: " ++ n)
                            Just val -> return val
eval3 (Plus e1 e2) = do e1'  <- eval3 e1
                        e2'  <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _                      -> throwError "type error in addition"
eval3 (Abs n e)    = do env <- ask
                        return $ FunVal env n e
eval3 (App e1 e2)  = do val1  <- eval3 e1
                        val2  <- eval3 e2
                        case val1 of
                            FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                            _                  -> throwError "type error in application"

t7 = U.t "t7"
     (runEval3 Map.empty (eval3 exampleExp))
     (Right (IntVal 18))

{-
when =Env= needed, use =ask=

when =Env= extended for recursive all, use =local :: (r -> r) -> m a -> m a=

NOTE: the "local" environment does not depend on the current environment, so use =const=

aside: =asks= expects a function mapping env to value.  Used to extract individual components via record selectors


When =ReaderT= added on top of =ErrorT can call =ask= and =local= in =eval3=

But need to call functions of inner monads (e.g., =throwError=) via:

class MonadTrans t where
    lift :: Monad m => m a -> t m a

=lift=
- lifts the return value of a function up by one layer in the monad stack
- sends your command inwards by one layer
- To access =foo= provided three layers down stack: =lift $ lift $ lift $ foo=

Why did call to =throwError= in =eval3= work  without lifting?
- because =ReaderT= is an instance of =MonadError=
- note: =ErrorT= is a =MonadReader= too

mtl programmers made the monad transformers instances of each other (n^2 instances)!

If you need to build a new monad transformer yourself, think carefully
about the design of all the plumbing behind the scene.

But: we must call =MonadIO.liftIO=  in =eval6= because there is no =IO=
class. But only once: do not need to worry about how many times to compose =lift=

------------------------------------------------------------------------------
-- ** return types of examples

=runEval4=
- ignoring =ReaderT= as it does not affect the return value
- peels off =ErrorT= and constructs a value of type =Either String a=
- peels off =StateT= and constructs a pair whose
  - first component is the value being computed
  - second component is the side effect, i.e., the state
- =(Either String a, Integer)=

=runEval4'=
- peels off =StateT= and then =ErrorT=
- =Either String (a, Integer)=

See:
- \href{http://www.haskell.org/all_about_monads/}{All About Monads},
- \href{http://en.wikibooks.org/wiki/Haskell/Monad_transformers}{Monad transformers on WikiBooks},
- \href{http://haskell.org/haskellwiki/Category:Monad}{Monad on haskell.org}, and
- \href{http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours}{Write Yourself a Scheme in 48 Hours}.

For more category theory stuff, start with
- \href{http://en.wikibooks.org/wiki/Haskell/Category_theory}{Category theory on WikiBooks}
- \href{http://stefan-klinger.de/files/monadGuide.pdf}{The Haskell Programmer's Guide to the IO Monad --- Don't Panic}
- \href{ftp://ftp.cs.wpi.edu/pub/techreports/pdf/03-21.pdf}{Monads for Programming Languages}

------------------------------------------------------------------------------
-- ** adding state

=State= : simulated mutability.

example : add profiling to interpreter : state is just integer counting calls to =eval4=

wrap =StateT= around the innermost monad =Identity= (order of =State= and =Error= matters).
-}

type Eval4 alpha = ReaderT Env (ErrorT String (StateT Integer Identity)) alpha

-- returns evaluation result (error or value) and state
-- give initial state arg for flexibility
runEval4            ::  Env -> Integer -> Eval4 alpha -> (Either String alpha, Integer)
runEval4 env st ev  =   runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

-- type not same as =Eval4= so it can reused elsewhere.
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

t8 = U.t "t8"
     (runEval4 Map.empty 0 (eval4 exampleExp))
     (Right (IntVal 18),8) -- 8 reduction steps

{-
Note: swap =StateT= and =ErrorT= then instead of:

type Eval4 alpha = ReaderT Env (ErrorT String (StateT Integer Identity)) alpha
runEval4            ::  Env -> Integer -> Eval4 alpha -> (Either String alpha, Integer)
runEval4 env st ev  = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

we get:

type Eval4' alpha    =   ReaderT Env (StateT Integer (ErrorT String Identity)) alpha
runEval4'           ::  Env -> Integer -> Eval4' alpha -> (Either String (alpha, Integer))
runEval4' env st ev = runIdentity (runErrorT (runStateT (runReaderT ev env) st))

Position of reader monad does not matter, since it does not contribute to the final result.

Note:

=State.gets= applies projection function to state before returning it.
=State.modify= applying function on state

------------------------------------------------------------------------------
-- ** adding logging

=WriterT= (kind of a dual to =ReaderT=): can add values to result of
computation (instead of using some values passed in like =ReaderT=).
-}

type Eval5 alpha = ReaderT Env  (ErrorT String (WriterT [String] (StateT Integer Identity))) alpha

{-
Similar to =StateT=, =WriterT= interacts with =ErrorT= because it produces output.

So depending on order of =ErrorT= and =WriterT=, result will include
the values written out or not when an error occurs.

Type of =WriterT= output values restricted to be a =Monoid=.  Because
class methods used internally to construct initial value and to
combine several values written out.
-}

runEval5            ::  Env -> Integer -> Eval5 alpha -> ((Either String alpha, [String]), Integer)
runEval5 env st ev  =   runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

eval5             :: Exp -> Eval5 Value
eval5 (Lit i)      = do tick
                        return $ IntVal i
eval5 (Var n)      = do tick
                        tell [n] -- write out name of each var encountered during evaluation
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

t9 = U.t "t9"
      (runEval5 Map.empty 0 (eval5 exampleExp))
      ((Right (IntVal 18),["x"]),8)

------------------------------------------------------------------------------
-- ** IO

{-
Not possible to define an IO monad transformer because IO execution
cannot be arbitrarily nested into other functions or monads.  Only
allowed in the monad =IO=.

mtl provides infrastructure to integrate IO: substitute =IO= for =Identity=

Because =Identity= is base monad, =runIdentity= applied last.
-}
type Eval6 alpha = ReaderT Env  (ErrorT String (WriterT [String] (StateT Integer IO))) alpha

{-
=runEval6= wrapped in =IO= constructor, so does not directly yield
result.  Yields IO computation which must be run to get result.

Also =runIdentity= invocation gone.
-}

runEval6           ::  Env -> Integer -> Eval6 alpha -> IO ((Either String alpha, [String]), Integer)
runEval6 env st ev  =  runStateT (runWriterT (runErrorT (runReaderT ev env))) st

-- must use =liftIO= to lift into the currently running monad

eval6             :: Exp -> Eval6 Value
eval6 (Lit i)      = do tick
                        liftIO $ print i -- print each int when evaluated
                        return $ IntVal i
eval6 (Var n)      = do tick
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
eval6 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval6 (App e1 e2)  = do tick
                        val1  <- eval6 e1
                        val2  <- eval6 e2
                        case val1 of
                            FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                            _                  -> throwError "type error in application"

t10 = U.t "t10"
      (unsafePerformIO (runEval6 Map.empty 0 (eval6 exampleExp)))
      ((Right (IntVal 18),["x"]),8)

rt =
    T.runTestTT $ T.TestList $ t0 ++ t1 ++ t2 ++ t3 ++ t4 ++ t5 ++ t6 ++ t7 ++ t8 ++ t9 ++ t10

-- End of file.
