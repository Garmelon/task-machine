module Task where

import Data.Time.Calendar
import DateExpressions

data Priority = Must
              | Should
              | Can
  deriving (Eq, Ord, Enum, Show)

type Description = String
type Amount = Int
type Duration = Int

data When = Forever
          | Until Day
          | During Duration Day
          | While Duration BoolExpr

data Task = Task Priority Description Amount When
