import           Test.Hspec

import Tests.DateExpr

{-
parseEval :: String -> Maybe Integer
parseEval str =
  let day  = ModifiedJulianDay 0
      expr = TM.parseIntExpr str
  in  expr >>= flip TM.evalIntExpr day

prop_ParseInteger :: Integer -> Bool
prop_ParseInteger n = parseEval (show n) == Just n

prop_ParseAddSub :: Integer -> Integer -> Integer -> Integer -> Bool
prop_ParseAddSub a b c d =
  let formula = show a ++ "+" ++ show b ++ "- (" ++ show c ++ "+" ++ show d ++ ")"
      expected = a + b - (c + d)
  in  parseEval formula == Just expected

prop_ParseMultDiv :: Integer -> Integer -> Integer -> Integer -> Property
prop_ParseMultDiv a b c d =
  let formula = show a ++ "*" ++ show b ++ "/ (" ++ show c ++ "*" ++ show d ++ ")"
      expected = a * b `div` (c * d)
  in  (c * d /= 0) ==> parseEval formula == Just expected

main :: IO ()
main = hspec $ do
  describe "Expressions" $ do
    describe "IntExpr" $ do
      it "parses integers" $ property $ prop_ParseInteger
      it "parses addition and subtraction" $ property $ prop_ParseAddSub
      it "parses multiplication and division" $ property $ prop_ParseMultDiv
      it "parses a complicated equation" $
        parseEval "12 - (2 / 5) * 3 + ((37))" `shouldBe` Just (12 - (2 `div` 5) * 3 + 37)
    describe "BoolExpr" $ do
-}

main :: IO ()
main = hspec $ do
  testDateExpr
