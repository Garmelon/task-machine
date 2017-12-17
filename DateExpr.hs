module DateExpr
  ( DateExpr
  , parseDateExpr
  , firstMatchingDay
  , evalDateExpr
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.Easter
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

type DateExpr = BoolExpr

data BoolExpr = BValue Bool
              | BStatement DateStatement
              | BLeapYear
              | BNot BoolExpr
              | BAnd BoolExpr BoolExpr
              | BOr BoolExpr BoolExpr
              | BEq IntExpr IntExpr
              | BGt IntExpr IntExpr
              | BLt IntExpr IntExpr
  deriving (Show)

data IntExpr = IValue Integer
             | IDate SpecialDate
             | INeg IntExpr
             | IAdd IntExpr IntExpr
             | IMul IntExpr IntExpr
             | IDiv IntExpr IntExpr -- div, not quot!
             | IMod IntExpr IntExpr -- mod, not rem!
  deriving (Show)

data DateStatement = IsLeapYear
                   | IsWeekend
  deriving (Show)
-- possible additions: IsEaster

data SpecialDate = SJulianDay
                 | SYear | SMonth | SDay
                 | SDayOfYear
                 | SDayOfWeek
                 | SYearCount
                 | SMonthCount
                 | SEaster
  deriving (Show)

{-
 - Evaluating expressions
 -}

evalDateExpr :: DateExpr -> Day -> Bool
evalDateExpr expr = fromMaybe False . evalBoolExpr expr

firstMatchingDay :: DateExpr -> Int -> Day -> Maybe Day
firstMatchingDay expr duration = find (evalDateExpr expr)
                               . take duration . iterate (addDays 1)

evalBoolExpr :: BoolExpr -> Day -> Maybe Bool
evalBoolExpr (BValue v)     _ = Just v
evalBoolExpr (BStatement s) d = Just $ evalDateStatement s d
evalBoolExpr (BNot a)       d = not <$> evalBoolExpr a d
evalBoolExpr (BAnd a b)     d = liftA2 (&&) (evalBoolExpr a d) (evalBoolExpr b d)
evalBoolExpr (BOr a b)      d = liftA2 (||) (evalBoolExpr a d) (evalBoolExpr b d)
evalBoolExpr (BEq a b)      d = liftA2 (==) (evalIntExpr a d) (evalIntExpr b d)
evalBoolExpr (BGt a b)      d = liftA2 (>) (evalIntExpr a d) (evalIntExpr b d)
evalBoolExpr (BLt a b)      d = liftA2 (<) (evalIntExpr a d) (evalIntExpr b d)

evalIntExpr :: IntExpr -> Day -> Maybe Integer
evalIntExpr (IValue v) _ = Just v
evalIntExpr (IDate s)  d = Just $ evalSpecialDate s d
evalIntExpr (INeg a)   d = negate <$> evalIntExpr a d
evalIntExpr (IAdd a b) d = liftA2 (+) (evalIntExpr a d) (evalIntExpr b d)
evalIntExpr (IMul a b) d = liftA2 (*) (evalIntExpr a d) (evalIntExpr b d)
evalIntExpr (IDiv a b) d = do
  x <- evalIntExpr a d
  y <- evalIntExpr b d
  guard $ y /= 0
  return $ x `div` y
evalIntExpr (IMod a b) d = do
  x <- evalIntExpr a d
  y <- evalIntExpr b d
  guard $ y /= 0
  return $ x `mod` y

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

{-
 - Helper functions for evaluation
 -}

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

{-
 - Parsing DateExpr
 -}

--               error ↓   ↓ input
type Parser = Parsec Void String

parseDateExpr :: Parser DateExpr
parseDateExpr = boolExpr

sc :: Parser () -- oddly necessary
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

bool :: Parser Bool
bool = (True <$ symbol "true") <|> (False <$ symbol "false")

-- helper functions for creating tables
prefix name f = Prefix (f <$ symbol name)
infixL name f = InfixL (f <$ symbol name)

-- parse IntExpr
intExpr :: Parser IntExpr
intExpr = makeExprParser intTerm intTable

intTable :: [[Operator Parser IntExpr]]
intTable = [ [prefix "+" id, prefix "-" INeg]
           , [infixL "*" IMul, infixL "/" IDiv, infixL "%" IMod]
           , [infixL "+" IAdd, infixL "-" (IAdd . INeg)]
           ]

intTerm :: Parser IntExpr
intTerm =   parens intExpr
        <|> IValue <$> integer
        <|> IDate <$> pSpecialDate
        <?> "integer expression"

-- parse BoolExpr
boolExpr :: Parser BoolExpr
boolExpr = makeExprParser boolTerm boolTable

boolTable :: [[Operator Parser BoolExpr]]
boolTable = [ [prefix "!" BNot]
            , [infixL "&&" BAnd, infixL "||" BOr]
            ]

boolTerm :: Parser BoolExpr
boolTerm =   parens boolExpr
         <|> BValue <$> bool
         <|> BStatement <$> pDateStatement
         <|> relExpr
         <?> "boolean expression"
  -- comparison (==, <, >, <=, >=)

relExpr = do
  a <- intExpr
  b <- relation
  c <- intExpr
  return $ b a c

relation =   (BEq <$ symbol "==")
         <|> ((\a b -> BNot (BEq a b)) <$ symbol "!=")
         <|> ((\a b -> BNot (BLt a b)) <$ symbol ">=")
         <|> ((\a b -> BNot (BGt a b)) <$ symbol "<=")
         <|> (BGt <$ symbol ">")
         <|> (BLt <$ symbol "<")

pSpecialDate =   (SJulianDay  <$ symbol "julian")
             <|> (SDayOfYear  <$ symbol "yearday")
             <|> (SYearCount  <$ symbol "yearcount")
             <|> (SYear       <$ symbol "year")
             <|> (SMonthCount <$ symbol "monthcount")
             <|> (SMonth      <$ symbol "month")
             <|> (SDay        <$ symbol "day")
             <|> (SDayOfWeek  <$ symbol "weekday")
             <|> (SEaster     <$ symbol "easter")
             <?> "special date" -- necessary?

pDateStatement =   (IsWeekend  <$ symbol "isweekend")
               <|> (IsLeapYear <$ symbol "isleapyear")
               <?> "date statement" -- necessary?
