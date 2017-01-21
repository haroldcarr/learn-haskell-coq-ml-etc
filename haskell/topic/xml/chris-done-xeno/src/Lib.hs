{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib where

import           Control.Monad           (when)
import           Data.ByteString         as BS
import           Data.HashMap.Strict     as M
import           Data.String.Conversions
import           Prelude                 as P
import           Text.RawString.QQ
import           Xeno.SAX
import           Xeno.Types

x :: ByteString
x = "Text<tag prop='value'>Hello, World!</tag><x><y prop=\"x\">Content!</y></x>Trailing."

f1 = fold const           (\m _ _ -> m + 1) const const const 0 x -- Count elements.

f2 = fold (\m _ -> m + 1) (\m _ _ -> m)     const const const 0 x -- Count attributes.

p = process
  (\x   -> P.putStrLn $ "open tag: "       <> cs' x)
  (\x y -> P.putStrLn $ "tag attributed: " <> cs' x <> " " <> cs' y)
  (\x   -> P.putStrLn $ "end open tag: "   <> cs' x)
  (\x   -> P.putStrLn $ "text: "           <> cs' x)
  (\x   -> P.putStrLn $ "close tag: "      <> cs' x)

p1 = p x

po = p opa

lfo = let (Right (_,_,_,kvs)) = fo opa in P.length kvs

fo :: ByteString -> Either XenoException (Bool, Bool, ByteString, HashMap ByteString ByteString)
fo =
  fold
    openTag
    attributeKeyValue
    endTag
    textValue
    closeTag
    (False, False, "", M.empty) -- (inside read element, store text value, previous tag, map)
 where
  openTag           state tag  =
    case (state, tag) of
      -- transition to inside read element
      ((False,  _, _, kvs), "read") -> (True,   False, tag, kvs)
      -- set current tag
      ((isRead, _, _, kvs), tag)    -> (isRead, False, tag, kvs)

  -- not using attributes
  attributeKeyValue state _ _ = state

  endTag            state tag =
    case (state, tag) of
      -- do not store the text value of "read"
      ((True  , _,  "read", kvs), "read") -> (True,   False, tag, kvs)
      ((True  , _, openTag, kvs), _) | openTag /= tag
                                          -> error ("bad: startTag: " <> cs' openTag <> " " <> "endTag: " <> cs' tag)
      -- set "store text value" to True so next textValue gets stored in map with current tag
      ((isRead, _,       _, kvs), _)      -> (isRead, True,  tag, kvs)

  textValue         state val =
    case state of
      -- store current tag/texValue and turn off store flag
      (True, True, tag, kvs) -> (True, False, tag, M.insert tag val kvs)
      _                      -> state

  closeTag          state tag  =
    case (state, tag) of
      ((isRead, _, _, kvs), "read") -> (False,  False, tag, kvs)
      ((isRead, _, _, kvs), _)      -> (isRead, False, tag, kvs)

cs' = convertString

opa :: BS.ByteString
opa =
  [r|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<data>
	<userid>1.0</userid>
	<caseid>11.0</caseid>
	<read>
		<global0:loan_amount>300000.0</global0:loan_amount>
		<global0:customer_name>Chris</global0:customer_name>
		<global0:customer_resident>true</global0:customer_resident>
		<global0:income_source0:income_amount>1500.0</global0:income_source0:income_amount>
		<global0:loan_asset_value>500000.0</global0:loan_asset_value>
		<global0:customer_previous_job>Professional</global0:customer_previous_job>
		<global0:asset0:asset_value>15000.0</global0:asset0:asset_value>
		<global0:asset0:asset>Car</global0:asset0:asset>
		<global0:income_source0:income_source>Salary</global0:income_source0:income_source>
		<global0:customer_employed>true</global0:customer_employed>
		<global0:customer_age>32.0</global0:customer_age>
		<global0:customer_previous_job_start_date>2000-01-01</global0:customer_previous_job_start_date>
	</read>
	<write>
		<global0:customer_minimum_eligibility>true</global0:customer_minimum_eligibility>
		<global0:customer_total_assets>15000.0</global0:customer_total_assets>
		<global0:maximum_loan_amount>500000.0</global0:maximum_loan_amount>
		<global0:customer_supplied_sufficient_supporting_documentation>true</global0:customer_supplied_sufficient_supporting_documentation>
		<global0:customer_weekly_income>1500.0</global0:customer_weekly_income>
		<global0:loan_value_ratio>60.0</global0:loan_value_ratio>
		<global0:loan_within_thresholds>true</global0:loan_within_thresholds>
		<global0:customer_eligible_loan>true</global0:customer_eligible_loan>
		<global0:minimum_loan_amount>5000.0</global0:minimum_loan_amount>
		<global0:maximum_loan_value_ratio>95.0</global0:maximum_loan_value_ratio>
		<global0:customer_offer_sufficient_security>true</global0:customer_offer_sufficient_security>
		<global0:customer_passes_employment_tests>true</global0:customer_passes_employment_tests>
		<global0:customer_meets_loan_serviceability_tests>true</global0:customer_meets_loan_serviceability_tests>
		<global0:weekly_loan_repayment_amount>300.0</global0:weekly_loan_repayment_amount>
		<global0:customer_passes_credit_history_tests_applicable>true</global0:customer_passes_credit_history_tests_applicable>
		<global0:minimum_age>18.0</global0:minimum_age>
		<global0:credit_history_test_waived>true</global0:credit_history_test_waived>
		<global0:customer_earnings_sufficient>true</global0:customer_earnings_sufficient>
		<global0:residency_tests_waived>false</global0:residency_tests_waived>
	</write>
</data>|]

vor = validate oparead

oparead :: BS.ByteString
oparead =
  [r|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <data>
      <read>
        <g:loan_amount>300000.0</g:loan_amount>
     </read>
    </data>|]
