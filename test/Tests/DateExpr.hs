module Tests.DateExpr
  ( testDateExpr
  ) where

import           Data.Time.Calendar

import           Test.Hspec
import           Test.QuickCheck

import qualified TaskMachine.DateExpr as TM

parseEvalInt :: String -> Day -> Maybe Integer
parseEvalInt str day = do
  expr <- TM.parseIntExpr str
  TM.evalIntExpr expr day

parseEvalBool :: String -> Day -> Maybe Bool
parseEvalBool str day = do
  expr <- TM.parseBoolExpr str
  TM.evalBoolExpr expr day

toDay :: Integer -> Day
toDay = ModifiedJulianDay

anyDay :: Day
anyDay = toDay 0

{-
 - IntExpr properties
 -}

prop_ParseInteger :: Integer -> Property
prop_ParseInteger n = parseEvalInt (show n) anyDay === Just n

prop_ParseAddSub :: Integer -> Integer -> Integer -> Integer -> Property
prop_ParseAddSub a b c d =
  let formula = show a ++ "+" ++ show b ++ "- (" ++ show c ++ "+" ++ show d ++ ")"
      expected = a + b - (c + d)
  in  parseEvalInt formula anyDay === Just expected

prop_ParseMultDiv :: Integer -> Integer -> Integer -> Integer -> Property
prop_ParseMultDiv a b c d =
  let formula = show a ++ "*" ++ show b ++ "/ (" ++ show c ++ "*" ++ show d ++ ")"
      expected = a * b `div` (c * d)
  in  (c * d /= 0) ==> parseEvalInt formula anyDay === Just expected

prop_ParseComplicated :: Integer -> Integer -> Integer -> Integer -> Integer -> Property
prop_ParseComplicated a b c d e =
  let formula =  show a ++ "% -(" ++ show b ++ "/" ++ show c ++ ") +"
              ++ show d ++ "* ((" ++ show e ++ "))"
      expected = a `mod` (-(b `div` c)) + d * e
  in (c /= 0 && (b `div` c) /= 0) ==> parseEvalInt formula anyDay === Just expected

{-
 - BoolExpr properties
 -}

prop_ParseLeapYears :: Integer -> Property
prop_ParseLeapYears a =
  let formula = "((year%400 == 0) || ((year%4 == 0) && (year%100 != 0))) == isleapyear"
  in  parseEvalBool formula (toDay a) === Just True

testDateExpr :: SpecWith ()
testDateExpr = describe "Date expressions" $ do
  describe "IntExpr" $ do
    it "parses integers" $ property prop_ParseInteger
    it "parses addition and subtraction" $ property prop_ParseAddSub
    it "parses multiplication and division" $ property prop_ParseMultDiv
    it "parses a complicated equation" $ property prop_ParseComplicated
  describe "BoolExpr" $ do
    it "parses \"true\""  $ parseEvalBool "true"  anyDay `shouldBe` Just True
    it "parses \"false\"" $ parseEvalBool "false" anyDay `shouldBe` Just False
    it "parses leap years" $ property prop_ParseLeapYears
