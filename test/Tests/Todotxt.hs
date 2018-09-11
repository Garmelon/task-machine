module Tests.Todotxt
  ( testTodotxt
  ) where

import           Data.Time.Calendar
import           Test.Hspec
import           Test.QuickCheck
import           Text.Megaparsec

import           TaskMachine.Todotxt

{- Dates properties -}


instance Bounded Day where -- year stays within 4 digits
  minBound = fromGregorian 1000 1 1
  maxBound = fromGregorian 9999 12 31

instance Arbitrary Day where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Dates where
  arbitrary = do
    which <- arbitrary
    case which of
      True  -> CrDate <$> arbitrary
      False -> CoCrDate <$> arbitrary <*> arbitrary

prop_DatesFormatParse :: Dates -> Bool
prop_DatesFormatParse d = parseMaybe dates (formatDates d) == Just d

prop_DatesParseCr :: Day -> Bool
prop_DatesParseCr d = parseMaybe dates (show d) == Just (CrDate d)

prop_DatesParseCoCr :: Day -> Day -> Bool
prop_DatesParseCoCr d1 d2 = parseMaybe dates (show d1 ++ " " ++ show d2) == Just (CoCrDate d1 d2)

{- Priority properties -}

instance Arbitrary Priority where
  arbitrary = arbitraryBoundedEnum

prop_PrioCharPrio :: Priority -> Bool
prop_PrioCharPrio p = (charToPriority . priorityToChar) p == Just p

prop_PrioFormatParse :: Priority -> Bool
prop_PrioFormatParse p = parseMaybe priority (formatPriority p) == Just p

prop_PrioToUppercase :: Priority -> Bool
prop_PrioToUppercase p = priorityToChar p `elem` ['A'..'Z']

{- The tests themselves -}

testTodotxt :: SpecWith ()
testTodotxt = describe "Todotxt" $ do
  describe "Priority" $ do
    it "can be converted to a Char and back"         $ forAll arbitrary prop_PrioCharPrio
    it "can be formatted and parsed again"           $ forAll arbitrary prop_PrioFormatParse
    it "is only converted into uppercase characters" $ forAll arbitrary prop_PrioToUppercase
  describe "Dates" $ do
    it "can be formatted and parsed again" $ property prop_DatesFormatParse
    it "parses single dates"               $ property prop_DatesParseCr
    it "parses double dates"               $ property prop_DatesParseCoCr

--parseEvalInt :: String -> Day -> Maybe Integer
--parseEvalInt str day = do
--  expr <- TM.parseIntExpr str
--  TM.evalIntExpr expr day
--
--parseEvalBool :: String -> Day -> Maybe Bool
--parseEvalBool str day = do
--  expr <- TM.parseBoolExpr str
--  TM.evalBoolExpr expr day
--
--toDay :: Integer -> Day
--toDay = ModifiedJulianDay
--
--anyDay :: Day
--anyDay = toDay 0
--
--{-
-- - IntExpr properties
-- -}
--
--prop_ParseInteger :: Integer -> Property
--prop_ParseInteger n = parseEvalInt (show n) anyDay === Just n
--
--prop_ParseAddSub :: Integer -> Integer -> Integer -> Integer -> Property
--prop_ParseAddSub a b c d =
--  let formula = show a ++ "+" ++ show b ++ "- (" ++ show c ++ "+" ++ show d ++ ")"
--      expected = a + b - (c + d)
--  in  parseEvalInt formula anyDay === Just expected
--
--prop_ParseMultDiv :: Integer -> Integer -> Integer -> Integer -> Property
--prop_ParseMultDiv a b c d =
--  let formula = show a ++ "*" ++ show b ++ "/ (" ++ show c ++ "*" ++ show d ++ ")"
--      expected = a * b `div` (c * d)
--  in  (c * d /= 0) ==> parseEvalInt formula anyDay === Just expected
--
--prop_ParseComplicated :: Integer -> Integer -> Integer -> Integer -> Integer -> Property
--prop_ParseComplicated a b c d e =
--  let formula =  show a ++ "% -(" ++ show b ++ "/" ++ show c ++ ") +"
--              ++ show d ++ "* ((" ++ show e ++ "))"
--      expected = a `mod` (-(b `div` c)) + d * e
--  in (c /= 0 && (b `div` c) /= 0) ==> parseEvalInt formula anyDay === Just expected
--
--{-
-- - BoolExpr properties
-- -}
--
--prop_FindWeekends :: Integer -> Property
--prop_FindWeekends a =
--  let formula = "(weekday == saturday || weekday == sunday) == isweekend"
--  in  parseEvalBool formula (toDay a) === Just True
--
--prop_FindLeapYears :: Integer -> Property
--prop_FindLeapYears a =
--  let formula = "(year%400 == 0 || (year%4 == 0 && year%100 != 0)) == isleapyear"
--  in  parseEvalBool formula (toDay a) === Just True
--
--testDateExpr :: SpecWith ()
--testDateExpr = describe "Date expressions" $ do
--  describe "IntExpr" $ do
--    it "parses integers"                       $ property prop_ParseInteger
--    it "evaluates addition and subtraction"    $ property prop_ParseAddSub
--    it "evaluates multiplication and division" $ property prop_ParseMultDiv
--    it "evaluates a complicated equation"      $ property prop_ParseComplicated
--  describe "BoolExpr" $ do
--    it "parses \"true\""  $ parseEvalBool "true"  anyDay `shouldBe` Just True
--    it "parses \"false\"" $ parseEvalBool "false" anyDay `shouldBe` Just False
--    it "finds weekends"   $ property prop_FindWeekends
--    it "finds leap years" $ property prop_FindLeapYears
