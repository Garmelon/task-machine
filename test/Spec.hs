import           Test.Hspec

import           Tests.Todotxt

main :: IO ()
main = hspec $ do
  testTodotxt
