{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text               as T
import           Numeric.Natural
-- import           Data.Validation
import           Data.Either.Validation
import           Data.Time.Calendar
import           Hedgehog
import qualified Hedgehog.Gen            as HHGen
import qualified Hedgehog.Range          as HHRange
import qualified Hedgehog.Internal.Range as HHIRange
import           Protolude

------------------------------------------------------------------------------
{-
Time Travelling and Fixing Bugs with Property-Based Testing (using Hedgehog)
Oskar Wickström
November 17, 2019

------------------------------------------------------------------------------
Example User Signup Validation

validation rules :

 0 ≤ length(name) ≤  50
18 ≤          age ≤ 150
-}

data SignupForm = SignupForm
  { formName :: Text
  , formAge  :: Int
  } deriving (Eq, Show)

data SignupError
  = NameTooShort Text
  | NameTooLong  Text
  | AgeTooYoung  Int
  | AgeTooOld    Int
  deriving (Eq, Show)

data Signup = Signup
  { name :: Text
  , age  :: Natural
  } deriving (Eq, Show)

causes :: Applicative f => Bool -> a -> Validation (f a) ()
True  `causes` err = Failure $ pure err
False `causes` _   = Success ()

validateName :: Text -> Validation (NonEmpty SignupError) Text
validateName n =
  Success n
  <* (0 == T.length n) `causes` NameTooShort n
  <* (T.length n > 50) `causes` NameTooLong  n

validateAge :: Int -> Validation (NonEmpty SignupError) Natural
validateAge a =
  Success (fromIntegral a)
  <* (a <  18) `causes` AgeTooYoung a
  <* (a > 150) `causes` AgeTooOld   a

validateSignup :: SignupForm -> Validation (NonEmpty SignupError) Signup
validateSignup (SignupForm n a) =
  Signup
  <$> validateName n
  <*> validateAge  a

{-
------------------------------------------------------------------------------
Validation type (from validation package) parameterized by
- type of validation failures
- type of a successfully validated value

similar to Either type except it accumulates failures, rather than short-circuiting on 1s failure)

Using a NonEmpty list for failures with Validation is common practice
- guarantees that if the validation fails, there’s at least one error value

------------------------------------------------------------------------------
test valid input data
-}

prop_valid_signup_form_succeeds :: Property
prop_valid_signup_form_succeeds  = property $ do
  let genForm = SignupForm <$> validName <*> validAge  -- generator with valid names and ages
  form <- forAll genForm                               -- generate values using generator
  case validateSignup form of                          -- apply validateSignup to generated values
    Success {}       -> pure ()
    Failure failure' -> do
      annotateShow failure'                            -- print the failure
      failure                                          -- fail the test

validName :: Gen Text
validName  = HHGen.text (HHRange.linear 1 50) HHGen.alphaNum

validAge :: Gen Int
validAge  = HHGen.integral (HHRange.linear 18 150)

{-
λ> check prop_valid_signup_form_succeeds
  ✓ <interactive> passed 100 tests.

------------------------------------------------------------------------------
test invalid input data
-}

prop_invalid_name_fails :: Property
prop_invalid_name_fails  = property $ do
  let genForm = SignupForm <$> invalidName <*> validAge -- only diff 'invalidName'
  form <- forAll genForm

  case validateSignup form of
    Failure (NameTooLong  {} :| []) -> pure ()
    Failure (NameTooShort {} :| []) -> pure ()
    other                           -> do
      annotateShow other
      failure

prop_invalid_age_fails :: Property
prop_invalid_age_fails  = property $ do
  let genForm = SignupForm <$> validName <*> invalidAge
  form <- forAll genForm
  case validateSignup form of
    Failure (AgeTooYoung {} :| []) -> pure ()
    Failure (AgeTooOld   {} :| []) -> pure ()
    other                          -> do
      annotateShow other
      failure

invalidName :: Gen Text
invalidName  =
  HHGen.choice [mempty, HHGen.text (HHRange.linear 51 100) HHGen.alphaNum]

invalidAge :: Gen Int
invalidAge  = HHGen.integral (HHRange.linear minBound 17) -- TODO only checks lower end

{-
λ> check prop_invalid_name_fails
  ✓ <interactive> passed 100 tests.

λ> check prop_invalid_age_fails
  ✓ <interactive> passed 100 tests.

------------------------------------------------------------------------------
Accumulating All Failures

Validation accumulates failures when combined with Applicative.

Need to test validations are correctly combined.

First try : weak
- nothing about which failures should be returned
-}

prop_two_failures_are_returned :: Property
prop_two_failures_are_returned  = property $ do
  let genForm = SignupForm <$> invalidName <*> invalidAge
  form <- forAll genForm
  case validateSignup form of
    Failure failures | length failures == 2 -> pure ()
    other -> do
      annotateShow other
      failure

-- slightly stronger : no duplicates

prop_two_different_failures_are_returned :: Property
prop_two_different_failures_are_returned  = property $ do
  let genForm = SignupForm <$> invalidName <*> invalidAge
  form <- forAll genForm
  case validateSignup form of
    Failure (failure1 :| [failure2]) ->
      failure1 /== failure2
    other                            -> do
      annotateShow other
      failure

{-
λ> check prop_two_failures_are_returned
  ✓ <interactive> passed 100 tests.

λ> check prop_two_different_failures_are_returned
  ✓ <interactive> passed 100 tests.

------------------------------------------------------------------------------
The Value of a Property

weak property tests might catch a lot of mistakes, but not all

weak properties better than no properties at all

------------------------------------------------------------------------------
Testing Generators

above Negative Property Tests do not cover all validation rules

problem in generator for invalidAge

need to test generator

define property testing the values it generates

e.g., generate only positive integers
-}

positive :: Gen Int
positive  = HHGen.integral (HHRange.linear 1 maxBound)

prop_integers_are_positive :: Property
prop_integers_are_positive  = property $ do
  n <- forAll positive
  assert (n >= 1)

{-
λ> check prop_integers_are_positive
  ✓ <interactive> passed 100 tests.

could use this to check validAge

cannot check invalidAge by checking it generates values such that
all boundaries of our validation function are hit
(properties can only find problems with individual generated values)

Instead, capturing statistics on generated values then perform global assertions.

Hedgehog can measure the occurences of user-defined labels.

LABEL : Text value, declared with an associated condition.

Hedgehog records the percentage of tests in which the condition evaluates to True.

Can have Hedgehog fail unless a certain percentage is met.

Adding Coverage Checks

ensure values are generated outside the boundaries of valid ages.
5% used here (but realistically they could both get close to 50%)
-}

prop_invalid_age_fails' :: Property
prop_invalid_age_fails'  = property $ do
  let genForm = SignupForm <$> validName <*> invalidAge
  form <- forAll genForm
  cover 5 "too young" (formAge form <=  17)
  cover 5 "too old"   (formAge form >= 151)
  case validateSignup form of
    Failure (AgeTooYoung {} :| []) -> pure ()
    Failure (AgeTooOld   {} :| []) -> pure ()
    other                          -> do
      annotateShow other
      failure

{-
λ> check prop_invalid_age_fails'
  ✗ <interactive> failed
    after 100 tests.
    too young 100% ████████████████████ ✓ 5%
  ⚠ too old     0% ···················· ✗ 5%
-}

invalidAge' :: Gen Int
invalidAge'  = HHGen.choice
  [ HHGen.integral (HHRange.linear minBound 17)
  , HHGen.integral (HHRange.linear 151 maxBound)
  ]

prop_invalid_age_fails'' :: Property
prop_invalid_age_fails''  = property $ do
  let genForm = SignupForm <$> validName <*> invalidAge'
  form <- forAll genForm
  cover 5 "too young" (formAge form <=  17)
  cover 5 "too old"   (formAge form >= 151)
  case validateSignup form of
    Failure (AgeTooYoung {} :| []) -> pure ()
    Failure (AgeTooOld   {} :| []) -> pure ()
    other                          -> do
      annotateShow other
      failure

{-
λ> check prop_invalid_age_fails''
  ✓ <interactive> passed 100 tests.
    too young 50% ██████████·········· ✓ 5%
    too old   50% ██████████·········· ✓ 5%

------------------------------------------------------------------------------
From Ages to Birth Dates : instead of age, users enter their birth date

validation must check, based on birth date, if user old enough
-}

data SignupForm' = SignupForm'
  { formName'      :: Text
  , formBirthDate' :: Day
  } deriving (Eq, Show)

data Signup' = Signup'
  { name'      :: Text
  , birthDate' :: Day
  } deriving (Eq, Show)

data SignupError'
  = NameTooShort' Text
  | NameTooLong'  Text
  | TooYoung'     Day
  | TooOld'       Day
  | NotYetBorn'   Day
  deriving (Eq, Show)

yearsBefore :: Integer -> Day -> Day
yearsBefore years = addGregorianYearsClip (negate years)

yearsAfter  :: Integer -> Day -> Day
yearsAfter  years = addGregorianYearsClip         years

validateName' :: Text -> Validation (NonEmpty SignupError') Text
validateName' n =
  Success n
  <* (0 == T.length n) `causes` NameTooShort' n
  <* (T.length n > 50) `causes` NameTooLong'  n

validateBirthDate :: Day -> Day -> Validation (NonEmpty SignupError') Day
validateBirthDate today bd =
  trace ((show today
          <> " " <> show bd
          <> " " <> show (yearsBefore  18 today)
          <> " " <> show (yearsAfter  150 today))::Text) $
  Success bd
  <* (bd < yearsBefore  18 today) `causes` TooYoung' bd
  <* (bd > yearsAfter  150 today) `causes` TooOld'   bd

validateSignup' :: Day -> SignupForm' -> Validation (NonEmpty SignupError') Signup'
validateSignup' today (SignupForm' n bd) =
  Signup'
  <$> validateName' n
  <*> validateBirthDate today bd

{-
------------------------------------------------------------------------------
Generating Dates
-}

-- construct a day range
linearDay' :: Day -> Day -> Range Day
linearDay' d1@(ModifiedJulianDay jd1) (ModifiedJulianDay jd2) =
  HHIRange.Range d1 $ \sz ->
    let
      jd1_sized =
        HHIRange.clamp jd1 jd2 $ HHIRange.scaleLinear sz jd1 jd1

      jd2_sized =
        HHIRange.clamp jd1 jd2 $ HHIRange.scaleLinear sz jd1 jd2
    in
      (ModifiedJulianDay jd1_sized, ModifiedJulianDay jd2_sized)

linearDay :: Day -> Day -> Range Day
linearDay (ModifiedJulianDay jd1) (ModifiedJulianDay jd2) =
  let HHIRange.Range d1 f = HHIRange.linear jd1 jd2
   in HHIRange.Range
      (ModifiedJulianDay d1)
      (\sz -> let (x, y) = f sz in (ModifiedJulianDay x, ModifiedJulianDay y))

{-
r = linearDay (fromGregorian 1900  1  1) (fromGregorian 2100 12 31)
-}

{-
instance  Integral Day where
    toInteger (ModifiedJulianDay n) = n

    {-# INLINE quot #-}
    _ `quot` (ModifiedJulianDay 0) = divZeroError
    (ModifiedJulianDay n) `quot` (ModifiedJulianDay d) = ModifiedJulianDay (n `quotInteger` d)

    {-# INLINE rem #-}
    _ `rem` (ModifiedJulianDay 0) = divZeroError
    (ModifiedJulianDay n) `rem` (ModifiedJulianDay d) = ModifiedJulianDay (n `remInteger` d)

    {-# INLINE div #-}
    _ `div` (ModifiedJulianDay 0) = divZeroError
    (ModifiedJulianDay n) `div` (ModifiedJulianDay d) = ModifiedJulianDay (n `divInteger` d)

    {-# INLINE mod #-}
    _ `mod` (ModifiedJulianDay 0) = divZeroError
    (ModifiedJulianDay n) `mod` (ModifiedJulianDay d) = ModifiedJulianDay (n `modInteger` d)

    {-# INLINE divMod #-}
    _ `divMod` (ModifiedJulianDay 0) = divZeroError
    (ModifiedJulianDay n) `divMod` (ModifiedJulianDay d) =
      case n `divModInteger`  d of (# x, y #) -> (ModifiedJulianDay x, ModifiedJulianDay y)

    {-# INLINE quotRem #-}
    _ `quotRem` (ModifiedJulianDay 0) = divZeroError
    (ModifiedJulianDay n) `quotRem` (ModifiedJulianDay d) =
      case n `quotRemInteger` d of (# q, r #) -> (ModifiedJulianDay q, ModifiedJulianDay r)
-}

-- generates a day within the given range
--genDay :: Range Day -> Gen Day
-- usage : genDay (fromGregorian 1900  1  1) (fromGregorian 2100 12 31)
genDay :: Day -> Day -> Gen Day
genDay (ModifiedJulianDay jd1) (ModifiedJulianDay jd2) = do
  let r = HHIRange.linear jd1 jd2
  jd   <- HHGen.integral r
  pure (ModifiedJulianDay jd)

-- arbitrary two centuries
anyDay :: Gen Day
anyDay = genDay (fromGregorian 1900 1 1) (fromGregorian 2100 12 31)

{-
------------------------------------------------------------------------------
Properties that deal with Day
-}

validBirthDate :: Day -> Gen Day
validBirthDate today = do
  n <- HHGen.integral (HHRange.linear 18 150)
  pure (n `yearsBefore` today)

prop_valid_signup_form_succeeds' :: Property
prop_valid_signup_form_succeeds'  = property $ do
  today <- forAll anyDay
  form  <- forAll (SignupForm' <$> validName <*> validBirthDate today)
  case validateSignup' today form of
    Success {}       -> do
      pure ()
    Failure failure' -> do
      annotateShow failure'
      failure

{- TODO FAIL
λ> check prop_valid_signup_form_succeeds'

check prop_valid_signup_form_succeeds'
1900-01-01 1882-01-01 1882-01-01 2050-01-01
1901-01-06 1882-01-06 1883-01-06 2051-01-06
1900-01-01 1881-01-01 1882-01-01 2050-01-01
1900-01-01 1881-01-01 1882-01-01 2050-01-01
1900-01-01 1882-01-01 1882-01-01 2050-01-01
  ✗ <interactive> failed at /Volumes/HC/.sync/.esync/openhc/2019-11-oskar-wickstrom-tt-and-fixing-bugs-with-property-based-testing/src/Lib.hs:433:7
    after 2 tests and 2 shrinks.

        ┏━━ src/Lib.hs ━━━
    424 ┃ prop_valid_signup_form_succeeds' :: Property
    425 ┃ prop_valid_signup_form_succeeds'  = property $ do
    426 ┃   today <- forAll anyDay
        ┃   │ 1900-01-01
    427 ┃   form  <- forAll (SignupForm' <$> validName <*> validBirthDate today)
        ┃   │ SignupForm' { formName' = "a" , formBirthDate' = 1881-01-01 }
    428 ┃   case validateSignup' today form of
    429 ┃     Success {}       -> do
    430 ┃       pure ()
    431 ┃     Failure failure' -> do
    432 ┃       annotateShow failure'
        ┃       │ TooYoung' 1881-01-01 :| []
    433 ┃       failure
        ┃       ^^^^^^^

    This failure can be reproduced by running:
    > recheck (Size 1) (Seed 13375056142721898540 10292551208289462325) <property>

------------------------------------------------------------------------------
TODO

Let’s move on to the next property, checking that invalid birth dates do not pass validation. Here, we use the same pattern as before, generating today’s date, but use invalidBirthDate instead:

prop_invalid_age_fails = property $ do
  today <- forAll anyDay
  form <- forAll (SignupForm <$> validName <*> invalidBirthDate today)

  cover 5 "not yet born" (formBirthDate form > today)
  cover 5 "too young" (formBirthDate form > 18 `yearsBefore` today)
  cover 5 "too old" (formBirthDate form < 150 `yearsBefore` today)

  case validateSignup today form of
    Failure (TooYoung{}   :| []) -> pure ()
    Failure (NotYetBorn{} :| []) -> pure ()
    Failure (TooOld{}     :| []) -> pure ()
    other                        -> do
      annotateShow other
      failure
Notice that we’ve also adjusted the coverage checks. There’s a new label, “not born yet,” for birth dates in the future. Running tests, we see the label in action:

λ> check prop_invalid_age_fails
  ✓ <interactive> passed 100 tests.
    not yet born 18% ███▌················ ✓ 5%
    too young    54% ██████████▊········· ✓ 5%
    too old      46% █████████▏·········· ✓ 5%
Good coverage, all tests passing. We’re not quite done, though. There’s a particular set of dates that we should be sure to cover: “today” dates and birth dates that are close to, or exactly, 18 years apart.

Within our current property test for invalid ages, we’re only sure that generated birth dates include at least 5% too old, and at least 5% too young. We don’t know how far away from the “18 years” validation boundary they are.

We could tweak our existing generators to produce values close to that boundary. Given a date
T
, exactly 18 years before today’s date, then:

invalidBirthDate would need to produce birth dates just after but not equal to
T
validBirthDate would need to produce birth dates just before or equal to
T
There’s another option, though. Instead of defining separate properties for valid and invalid ages, we’ll use a single property for all cases. This way, we only need a single generator.

A Single Validation Property
In Building on developers’ intuitions to create effective property-based tests, John Hughes talks about “one property to rule them all.” Similarly, we’ll define a single property prop_validates_age for birth date validation. We’ll base our new property on prop_invalid_age_fails, but generalize to cover both positive and negative tests:

prop_validates_age = property $ do
  today <- forAll anyDay
  form  <- forAll (SignupForm <$> validName <*> anyBirthDate today)

  let tooYoung        = formBirthDate form > 18 `yearsBefore` today
      notYetBorn      = formBirthDate form > today
      tooOld          = formBirthDate form < 150 `yearsBefore` today
      oldEnough       = formBirthDate form <= 18 `yearsBefore` today
      exactly age = formBirthDate form == age `yearsBefore` today
      closeTo age =
        let diff' =
                diffDays (formBirthDate form) (age `yearsBefore` today)
        in  abs diff' `elem` [0 .. 2]

  cover 10 "too young"    tooYoung
  cover 1  "not yet born" notYetBorn
  cover 1  "too old"      tooOld

  cover 20 "old enough"   oldEnough
  cover 1  "exactly 18"   (exactly 18)
  cover 5  "close to 18"  (closeTo 18)

  case validateSignup today form of
    Failure (NotYetBorn{} :| []) | notYetBorn -> pure ()
    Failure (TooYoung{} :| []) | tooYoung -> pure ()
    Failure (TooOld{} :| []) | tooOld -> pure ()
    Success{} | oldEnough             -> pure ()
    other                             -> annotateShow other >> failure
There are a few new things going on here:

Instead of generating exclusively invalid or valid birth dates, we’re now generating any birth date based on today’s date
The boolean expressions are used both in coverage checks and in asserting, so we separate them in a let binding
We add three new labels for the valid cases
Finally, we assert on both valid and invalid cases, based on the same expressions used in coverage checks
Note that our assertions are more specific than in prop_invalid_age_fails. The failure cases only pass if the corresponding label expressions are true. The oldEnough case covers all valid birth dates. Any result other than the four expected cases is considered incorrect.

The anyBirthDate generator is based on today’s date:

anyBirthDate :: Day -> Gen Day
anyBirthDate today =
  let
      inPast range = do
        years <- HHGen.integral range
        pure (years `yearsBefore` today)
      inFuture = do
        years <- HHGen.integral (HHRange.linear 1 5)
        pure (addGregorianYearsRollOver years today)
      daysAroundEighteenthYearsAgo = do
        days <- HHGen.integral (HHRange.linearFrom 0 (-2) 2)
        pure (addDays days (18 `yearsBefore` today))
  in
      HHGen.frequency
        [ (5, inPast (HHRange.exponential 1 150))
        , (1, inPast (HHRange.exponential 151 200))
        , (2, inFuture)
        , (2, daysAroundEighteenthYearsAgo)
        ]
We defines helper functions (❶) for generating dates in the past, in the future, and close to 18 years ago. Using those helper functions, we combine four generators, with different date ranges, using a HHGen.frequency distribution (❷). The weights we use are selected to give us a good coverage.

Let’s run some tests:

λ> check prop_validates_age
  ✓ <interactive> passed 100 tests.
    too young    62% ████████████▍······· ✓ 10%
    not yet born 20% ████················ ✓  1%
    too old       4% ▊··················· ✓  1%
    old enough   38% ███████▌············ ✓ 20%
    exactly 18   16% ███▏················ ✓  1%
    close to 18  21% ████▏··············· ✓  5%
Looks good! We’ve gone from testing positive and negative cases separately, to instead have a single property covering all cases, based on a single generator. It’s now easier to generate values close to the valid/invalid boundary of our SUT, i.e. around 18 years from today’s date.

February 29th
For the fun of it, let’s run some more tests. We’ll crank it up to 20000.

λ> check (withTests 20000 prop_validates_age)
  ✗ <interactive> failed at test/Validation/V3Test.hs:141:64
    after 17000 tests and 25 shrinks.
    too young    60% ████████████········ ✓ 10%
    not yet born 20% ███▉················ ✓  1%
    too old       9% █▉·················· ✓  1%
    old enough   40% ███████▉············ ✓ 20%
    exactly 18   14% ██▊················· ✓  1%
    close to 18  21% ████▎··············· ✓  5%

        ┏━━ test/Validation/V3Test.hs ━━━
    114 ┃ prop_validates_age = property $ do
    115 ┃   today <- forAll anyDay
        ┃   │ 1956 - 02 - 29
    116 ┃   form  <- forAll (SignupForm <$> validName <*> anyBirthDate today)
        ┃   │ SignupForm { formName = "aa" , formBirthDate = 1938 - 03 - 01 }
    117 ┃
    118 ┃   let tooYoung        = formBirthDate form > 18 `yearsBefore` today
    119 ┃       notYetBorn      = formBirthDate form > today
    120 ┃       tooOld          = formBirthDate form < 150 `yearsBefore` today
    121 ┃       oldEnough       = formBirthDate form <= 18 `yearsBefore` today
    122 ┃       exactlyEighteen = formBirthDate form == 18 `yearsBefore` today
    123 ┃       closeToEighteen =
    124 ┃         let diff' =
    125 ┃                 diffDays (formBirthDate form) (18 `yearsBefore` today)
    126 ┃         in  abs diff' `elem` [0 .. 2]
    127 ┃
    128 ┃   cover 10 "too young"    tooYoung
    129 ┃   cover 1  "not yet born" notYetBorn
    130 ┃   cover 1  "too old"      tooOld
    131 ┃
    132 ┃   cover 20 "old enough"   oldEnough
    133 ┃   cover 1  "exactly 18"   exactlyEighteen
    134 ┃   cover 5  "close to 18"  closeToEighteen
    135 ┃
    136 ┃   case validateSignup today form of
    137 ┃     Failure (NotYetBorn{} :| []) | notYetBorn -> pure ()
    138 ┃     Failure (TooYoung{} :| []) | tooYoung -> pure ()
    139 ┃     Failure (TooOld{} :| []) | tooOld -> pure ()
    140 ┃     Success{} | oldEnough             -> pure ()
    141 ┃     other                             -> annotateShow other >> failure
        ┃     │ Success Signup { name = "aa" , birthDate = 1938 - 03 - 01 }
        ┃     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Failure! Chaos! What’s going on here? Let’s examine the test case:

Today’s date is 1956-02-29
The birth date is 1938-03-01
The validation function considers this valid (it returns a Success value)
The test does considers this invalid (oldEnough is False)
This means that when the validation runs on a leap day, February 29th, and the person would turn 18 years old the day after (on March 1st), the validation function incorrectly considers the person old enough. We’ve found a bug.

Test Count and Coverage
Two things led us to find this bug:

Most importantly, that we generate today’s date and pass it as a parameter. Had we used the actual date, retrieved with an IO action, we’d only be able to find this bug every 1461 days. Pure functions are easier to test.
That we ran more tests than the default of 100. We might not have found this bug until much later, when the generated dates happened to trigger this particular bug. In fact, running 20000 tests does not always trigger the bug.
Our systems are often too complex to be tested exhaustively. Let’s use our form validation as an example. Between 1900-01-01 and 2100-12-31 there are 73,413 days. Selecting today’s date and the birth date from that range, we have more than five billion combinations. Running that many Hedgehog tests in GHCi on my laptop (based on some quick benchmarks) would take about a month. And this is a simple pure validation function!

To increase coverage, even if it’s not going to be exhaustive, we can increase the number of tests we run. But how many should we run? On a continuous integration server we might be able to run more than we do locally, but we still want to keep a tight feedback loop. And what if our generators never produce inputs that reveal existing bugs, regardless of the number of tests we run?

If we can’t test exhaustively, we need to ensure our generators cover interesting combinations of inputs. We need to carefully design and measure our tests and generators, based on the edge cases we already know of, as well as the ones that we discover over time. PBT without measuring coverage easily turns into a false sense of security.

In the case of our leap day bug, we can catch it with fewer tests, and on every test run. We need to make sure we cover leap days, used both as today’s date and as the birth date, even with a low number of tests.

Covering Leap Days
To generate inputs that cover certain edge cases, we combine specific generators using HHGen.frequency:

(today, birthDate') <- forAll
  (HHGen.frequency
    [ (5, anyDayAndBirthDate)

    , (2, anyDayAndBirthDateAroundYearsAgo 18)
    , (2, anyDayAndBirthDateAroundYearsAgo 150)

    , (1, leapDayAndBirthDateAroundYearsAgo 18)
    , (1, leapDayAndBirthDateAroundYearsAgo 150)

    , (1, commonDayAndLeaplingBirthDateAroundYearsAgo 18)
    , (1, commonDayAndLeaplingBirthDateAroundYearsAgo 150)
    ]
  )
Arbitrary values for today’s date and the birth date are drawn most frequently (❶), with a weight of 5. Next, with weights of 2, are generators for cases close to the boundaries of the validation function (❷). Finally, with weights of 1, are generators for special cases involving leap days as today’s date (❸) and leap days as birth date (❹).

Note that these generators return pairs of dates. For most of these generators, there’s a strong relation between today’s date and the birth date. For example, we can’t first generate any today’s date, pass that into a generator function, and expect it to always generate a leap day that occured 18 years ago. Such a generator would have to first generate the leap day and then today’s date.

Let’s define the generators. The first one, anyDayAndBirthDate, picks any today’s date within a wide date range. It also picks a birth date from an even wider date range, resulting in some future birth dates and some ages above 150.

anyDayAndBirthDate :: Gen (Day, Day)
anyDayAndBirthDate = do
  today <- Time.day
    (Time.linearDay (fromGregorian 1900 1 1)
                    (fromGregorian 2020 12 31)
    )
  birthDate' <- Time.day
    (Time.linearDay (fromGregorian 1850 1 1)
                    (fromGregorian 2050 12 31)
    )
  pure (today, birthDate')
Writing automated tests with a hard-coded year 2020 might scare you. Won’t these tests fail when run in the future? No, not these tests. Remember, the validation function is deterministic. We control today’s date. The actual date on which we run these tests doesn’t matter.

Similar to the previous generator is anyDayAndBirthDateAroundYearsAgo. First, it generates any date as today’s date (❶). Next, it generates an arbitrary date approximately some number of years ago (❷), where the number of years is an argument of the generator.

anyDayAndBirthDateAroundYearsAgo :: Integer -> Gen (Day, Day)
anyDayAndBirthDateAroundYearsAgo years = do
  today <- Time.day
    (Time.linearDay (fromGregorian 1900 1 1)
                    (fromGregorian 2020 12 31)
    )
  birthDate' <- addingApproxYears (negate years) today
  pure (today, birthDate')
The addingApproxYearsAgo generator adds a number of years to a date, and offsets it between two days back and two days forward in time.

addingApproxYears :: Integer -> Day -> Gen Day
addingApproxYears years today = do
  days <- HHGen.integral (HHRange.linearFrom 0 (-2) 2)
  pure (addDays days (addGregorianYearsRollOver years today))
The last two generators used in our frequency distribution cover leap day edge cases. First, let’s define the leapDayAndBirthDateAroundYearsAgo generator. It generates a leap day used as today’s date, and a birth date close to the given number of years ago.

leapDayAndBirthDateAroundYearsAgo :: Integer -> Gen (Day, Day)
leapDayAndBirthDateAroundYearsAgo years = do
  today      <- leapDay (HHRange.linear 1904 2020)
  birthDate' <- addingApproxYears (negate years) today
  pure (today, birthDate')
The leapDay generator uses mod to only generate years divisible by 4 and constructs dates on February 29th. That alone isn’t enough to only generate valid leap days, though. Years divisible by 100 but not by 400 are not leap years. To keep the generator simple, we discard those years using the already existing isLeapDay predicate as a filter.

leapDay :: Range Integer -> Gen Day
leapDay yearRange = HHGen.filter isLeapDay $ do
  year <- HHGen.integral yearRange
  pure (fromGregorian (year - year `mod` 4) 2 29)
In general, we should be careful about discarding generated values using filter. If we discard too much, Hedgehog gives up and complains loudly. In this particular case, discarding a few generated dates is fine. Depending on the year range we pass it, we might not discard any date.

Finally, we define the commonDayAndLeaplingBirthDateAroundYearsAgo generator. It first generates a leap day used as the birth date, and then a today’s date approximately the given number of years after the birth date.

commonDayAndLeaplingBirthDateAroundYearsAgo :: Integer -> Gen (Day, Day)
commonDayAndLeaplingBirthDateAroundYearsAgo years = do
  birthDate' <- leapDay (HHRange.linear 1904 2020)
  today <- addingApproxYears years birthDate'
  pure (today, birthDate')
That’s it for the generators. Now, how do we know that we’re covering the edge cases well enough? With coverage checks!


cover 5
      "close to 18, validated on common day"
      (closeTo 18 && not (isLeapDay today))
cover 1
      "close to 18, validated on leap day"
      (closeTo 18 && isLeapDay today)

cover 5
      "close to 150, validated on common day"
      (closeTo 150 && not (isLeapDay today))
cover 1
      "close to 150, validated on leap day"
      (closeTo 150 && isLeapDay today)

cover 5
      "exactly 18 today, born on common day"
      (exactly 18 && not (isLeapDay birthDate'))
cover
  1
  "legally 18 today, born on leap day"
  (  isLeapDay birthDate'
  && (addGregorianYearsRollOver 18 birthDate' == today)
  )

We add new checks to the property test, checking that we hit both leap day and regular day cases around the 18th birthday (❶) and the 150th birthday (❷). Notice that we had similar checks before, but we were not discriminating between leap days and common days.

Finally, we check the coverage of two leap day scenarios that can occur when a person legally turns 18: a person born on a common day turning 18 on a leap day (❸), and a leapling turning 18 on a common day (❹).

Running the modified property test, we get the leap day counter-example every time, even with as few as a hundred tests. For example, we might see today’s date being 1904-02-29 and the birth date being 1886-03-01. The validation function deems the person old enough. Again, this is incorrect.

Now that we can quickly and reliably reproduce the failing example we are in a great position to find the error. While we could use a fixed seed to reproduce the particular failing case from the 20000 tests run, we are now more confident that the property test would catch future leap day-related bugs, if we were to introduce new ones. Digging into the implementation, we’ll find a boolean expression in a pattern guard being the culprit:

birthDate' <= addGregorianYearsRollOver (-18) today
The use of addGregorianYearsRollOver together with adding a negative number of years is the problem, rolling over to March 1st instead of clipping to February 28th. Instead, we should use addGregorianYearsClip:

birthDate' <= addGregorianYearsClip (-18) today
Running 100 tests again, we see that they all pass, and that our coverage requirements are met.

λ> Hedgehog.check prop_validates_age
  ✓ <interactive> passed 100 tests.
    too young                             17% ███▍················ ✓ 10%
    not yet born                           7% █▍·················· ✓  1%
    too old                               19% ███▊················ ✓  1%
    old enough                            83% ████████████████▌··· ✓ 20%
    close to 18, validated on common day  30% ██████·············· ✓  5%
    close to 18, validated on leap day     2% ▍··················· ✓  1%
    close to 150, validated on common day 31% ██████▏············· ✓  5%
    close to 150, validated on leap day    6% █▏·················· ✓  1%
    exactly 18 today, born on common day  17% ███▍················ ✓  5%
    legally 18 today, born on leap day     5% █··················· ✓  1%

Summary

The trade-off between multiple disjoint properties and a single more
complicated property is hard.

With multiple properties, for example split between positive and
negative tests, both generators and assertions can be simpler and more
targeted.

But you run a risk of missing certain inputs. The set of properties
might not cover the entire space of inputs. Furthermore, performing
coverage checks across multiple properties, using multiple targeted
generators, can be problematic.

Ensuring coverage of generators in a single property is easier.

The drawback of using a single property is that the assertion not only
becomes more complicated, it’s also likely to mirror the
implementation of the SUT.

choice between single or multiple properties comes down to how you
want to cover the boundaries of the SUT. Ultimately, both approaches
can achieve the same coverage, in different ways. They both suffer
from the classic problem of a test suite mirroring the system it’s
testing.
-}
