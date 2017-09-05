> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs                     #-}
> {-# LANGUAGE OverloadedStrings         #-}
> {-# LANGUAGE TypeFamilies              #-}
>
> module Lib where
>
> import Data.Proxy
> import Data.Text as T (Text, unpack)

Existential Types - March 31, 2017

Example use-case : quiz made up of different types of questions

> data OpenQuestion   = OpenQuestion   { oquestion ::  Text,  ocorrect :: Text }
> data MCQuestion     = MCQuestion     { mquestion :: [Text], mcorrect :: Int }
> data RatingQuestion = RatingQuestion { rquestion ::  Text,  rcorrect :: Float }
>
> type UserAnswer = Text
>
> data Quiz = Quiz { questions :: [ Question ] }

Key issue:
- define `Question` to enable it to work with current and future question types in a uniform way.

Possible solution:
- wrap in constructor : "tags" each question with type:

> data QuestionX
>   = MCQ    MCQuestion
>   | Open   OpenQuestion
>   | Rating RatingQuestion

PRO: simple; amenable to pattern-matching
CON: expression problem

Solution using existentials and type class :

> class Questionable q where
>   type Answer q     :: *
>   type TQuestion q  :: *
>   isCorrectAnswer   :: Answer q -> q -> Bool
>   question          :: q -> TQuestion q
>   convertUserAnswer :: Proxy q -> Text -> Answer q

Make each type of question is an instance of Questionnable interface.

> instance Questionable OpenQuestion where
>   type Answer OpenQuestion                 = Text
>   type TQuestion OpenQuestion              = Text
>   isCorrectAnswer t1 (OpenQuestion _ t2)   = t1 == t2
>   question (OpenQuestion q _)              = q
>   convertUserAnswer _                      = read . T.unpack
>
> e1 = isCorrectAnswer "foo" (OpenQuestion "q" "foo")

> instance Questionable MCQuestion where
>   type Answer MCQuestion                   = Int
>   type TQuestion MCQuestion                = [Text]
>   isCorrectAnswer i1 (MCQuestion _ i2)     = i1 == i2
>   question (MCQuestion q _)                = q
>   convertUserAnswer _                      = read . T.unpack
>
> e2 = isCorrectAnswer 101 (MCQuestion [] 101)

> instance Questionable RatingQuestion where
>   type Answer RatingQuestion               = Float
>   type TQuestion RatingQuestion            = Text
>   isCorrectAnswer f1 (RatingQuestion _ f2) = f1 == f2
>   question (RatingQuestion q _)            = q
>   convertUserAnswer _                      = read . T.unpack
>
> e3 = isCorrectAnswer 10.1 (RatingQuestion "t" 10.1)
> e32 = convertUserAnswer (Proxy :: Proxy RatingQuestion) "1.2"

Wrap them with Quetsion using existential quantification.

ADT syntax:

> data QuestionADT =
>   forall q . Questionable q => QuestionADT q

GADT syntax:

> data Question where
>   Question :: Questionable q => q -> Question

Above limits scope of type variable q.
Ensures question itself, whatever its type, stay within scope of its appearance.
E.g., : can't do (gives compiler error) : getQ (Question question _) = question
because type of result question :: q would let type variable q escape context of constructor where `getQ` gets used.

Why called existential? (Especially since introduced by forall keyword?)
- comes from the fact that : ∀x.Q(x)⟹P = (∃x.Q(x)) ⟹ P.
- See StackOverflow
    - explains how intuitionistic logic rules relate ∀ and ∃ quantifiers in the case of type constructors.

References:
- Roman Cheplyaka’s 24 Days of GHC Extensions : how existentials work in Haskell
- TAPL chapter 24
- GHC manual

