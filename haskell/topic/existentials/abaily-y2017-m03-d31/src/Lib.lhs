> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs                     #-}
> {-# LANGUAGE OverloadedStrings         #-}
> {-# LANGUAGE TypeFamilies              #-}
>
> module Lib where
>
> import Control.Monad.State
> import Data.Text as T (Text, pack)

Existential Types - March 31, 2017

Example use-case : quiz made up of different types of questions

> data OpenQuestion
>   = OpenQuestion   { oquestion ::  Text,  ocorrect :: Text }
> data MCQuestion
>   = MCQuestion     { mquestion :: [Text], mcorrect :: Int }
> data RatingQuestion
>   = RatingQuestion { rquestion ::  Text,  rcorrect :: Float }

Key issue:
- define a list of different types of questions
  that work with current and future question types in a uniform way.

Possible solution:
- wrap in constructor : "tags" each question with type:

> data QuestionX
>   = MCQ    MCQuestion
>   | Open   OpenQuestion
>   | Rating RatingQuestion

PRO: simple; amenable to pattern-matching
CON: expression problem

Solution using type class and existentials :

> class Questionable q where
>   type Answer    q  :: *
>   type TQuestion q  :: *
>   isCorrectAnswer   :: Answer q -> q -> Bool
>   question          :: q -> TQuestion q
>   qprint            :: q -> IO ()
>   convertUserAnswer :: q -> String -> Answer q

Make each type of question an instance of Questionable interface.

> instance Questionable OpenQuestion where
>   type Answer         OpenQuestion         = Text
>   type TQuestion      OpenQuestion         = Text
>   isCorrectAnswer t1 (OpenQuestion _ t2)   = t1 == t2
>   question           (OpenQuestion q _)    = q
>   qprint             (OpenQuestion q _)    = print q
>   convertUserAnswer _                      = T.pack

> instance Questionable MCQuestion where
>   type Answer         MCQuestion           = Int
>   type TQuestion      MCQuestion           = [Text]
>   isCorrectAnswer i1 (MCQuestion _ i2)     = i1 == i2
>   question           (MCQuestion q _)      = q
>   qprint             (MCQuestion q _)      = print q
>   convertUserAnswer _                      = read

> instance Questionable RatingQuestion where
>   type Answer         RatingQuestion       = Float
>   type TQuestion      RatingQuestion       = Text
>   isCorrectAnswer f1 (RatingQuestion _ f2) = f1 == f2
>   question           (RatingQuestion q _)  = q
>   qprint             (RatingQuestion q _)  = print q
>   convertUserAnswer _                      = read

Wrap them with Question using existential quantification.

ADT syntax:

> data QuestionADT =
>   forall q . Questionable q => QuestionADT q

GADT syntax:

> data Question where
>   Question :: Questionable q => q -> Question

Existential limits scope of type variable q.
Ensures `q`, whatever its type, stays within scope of its appearance.
E.g., : can't do (gives compiler error) : getQ (Question question) = question
because type of result question :: q would let type variable q escape context of constructor where `getQ` gets used.

Use it:

> questions :: [Question]
> questions = [ Question $ OpenQuestion   "What is the open question?" "D-"
>             , Question $ MCQuestion     ["1","2"]                    2
>             , Question $ RatingQuestion "How does this rate?"        6.9
>             ]

> loop :: [Question] -> IO ()
> loop qs0 = evalStateT (loop' qs0) (0,0)
>  where
>   loop' :: [Question] -> StateT (Int,Int) IO ()
>   loop' [] = do
>     (c,t) <- get
>     lift $ putStr (show c); lift $ putStr "/"; lift $ print t;
>   loop' (Question q:qs) = do
>     lift $ qprint q
>     a <- lift getLine
>     let a' = convertUserAnswer q a
>     (c,t) <- get
>     if isCorrectAnswer a' q then do
>       put (c+1,t+1)
>       lift $ putStrLn "YES"
>     else do
>       put (c,t+1)
>       lift $ putStrLn "NO"
>     loop' qs

> lq :: IO ()
> lq = loop questions

Why called existential? (Especially since introduced by forall keyword?)
- comes from the fact that : ∀x.Q(x)⟹P = (∃x.Q(x)) ⟹ P.
- See StackOverflow
    - explains how intuitionistic logic rules relate ∀ and ∃ quantifiers in the case of type constructors.

References:
- Roman Cheplyaka’s 24 Days of GHC Extensions : how existentials work in Haskell
- TAPL chapter 24
- GHC manual

