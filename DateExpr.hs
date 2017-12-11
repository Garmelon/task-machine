module DateExpr
  ( BoolExpr
  , IntExpr
  ) where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.Easter

data BoolExpr = BValue Bool
              | BStatement DateStatement
              | BLeapYear
              | BNot BoolExpr
              | BAnd BoolExpr BoolExpr
              | BOr BoolExpr BoolExpr
              | BEq IntExpr IntExpr
              | BGt IntExpr IntExpr
              | BLt IntExpr IntExpr

data IntExpr = IValue Integer
             | IDate SpecialDate
             | INeg IntExpr
             | IAdd IntExpr IntExpr
             | IMul IntExpr IntExpr
             | IDiv IntExpr IntExpr -- div, not quot!
             | IMod IntExpr IntExpr -- mod, not rem!

data DateStatement = IsLeapYear
                   | IsWeekend

data SpecialDate = SJulianDay
                 | SYear | SMonth | SDay
                 | SDayOfYear
                 | SDayOfWeek
                 | SYearCount
                 | SMonthCount
                 | SEaster

evalBoolExpr :: BoolExpr -> Day -> Bool
evalBoolExpr (BValue v)     _ = v
evalBoolExpr (BStatement s) d = evalDateStatement s d
evalBoolExpr (BNot a)       d = not $ evalBoolExpr a d
evalBoolExpr (BAnd a b)     d = evalBoolExpr a d && evalBoolExpr b d
evalBoolExpr (BOr a b)      d = evalBoolExpr a d || evalBoolExpr b d
evalBoolExpr (BEq a b)      d = evalIntExpr a d == evalIntExpr b d
evalBoolExpr (BGt a b)      d = evalIntExpr a d > evalIntExpr b d
evalBoolExpr (BLt a b)      d = evalIntExpr a d < evalIntExpr b d

evalIntExpr :: IntExpr -> Day -> Integer
evalIntExpr (IValue v) _ = v
evalIntExpr (IDate s)  d = evalSpecialDate s d
evalIntExpr (INeg a)   d = - evalIntExpr a d
evalIntExpr (IAdd a b) d = evalIntExpr a d + evalIntExpr b d
evalIntExpr (IMul a b) d = evalIntExpr a d * evalIntExpr b d
evalIntExpr (IDiv a b) d = evalIntExpr a d `div` evalIntExpr b d
evalIntExpr (IMod a b) d = evalIntExpr a d `mod` evalIntExpr b d

evalDateStatement :: DateStatement -> Day -> Bool
evalDateStatement IsLeapYear d = isLeapYear $ year d
evalDateStatement IsWeekend  d = weekday d `elem` [6,7]

evalSpecialDate :: SpecialDate -> Day -> Integer
evalSpecialDate SJulianDay  d = julian d
evalSpecialDate SYear       d = year d
evalSpecialDate SMonth      d = month d
evalSpecialDate SDay        d = day d
evalSpecialDate SDayOfYear  d = yearday d
evalSpecialDate SDayOfWeek  d = weekday d
evalSpecialDate SYearCount  d = ((yearday d - 1) `div` 7) + 1
evalSpecialDate SMonthCount d = ((day d - 1) `div` 7) + 1
evalSpecialDate SEaster     d = diffDays d $ orthodoxEaster $ year d

julian :: Day -> Integer
julian = flip diffDays (fromGregorian 1858 11 17)

year :: Day -> Integer
year d = let (r,_,_) = toGregorian d in r

month :: Day -> Integer
month d = let (_,r,_) = toGregorian d in toInteger r

day :: Day -> Integer
day d = let (_,_,r) = toGregorian d in toInteger r

weekday :: Day -> Integer
weekday d = let (_,_,r) = toWeekDate d in toInteger r

yearday :: Day -> Integer
yearday day = let (y,m,d)   = toGregorian day
                  dayofyear = monthAndDayToDayOfYear (isLeapYear y) m d
              in  toInteger dayofyear
