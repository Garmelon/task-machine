{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.DateExpr
  ( BoolExpr
  , parseBoolExpr
  --, saveBoolExpr
  , evalBoolExpr
  , IntExpr
  , parseIntExpr
  --, saveIntExpr
  , evalIntExpr
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Void
--import           Data.List
--import           Data.Maybe

--import qualified Data.Text                        as T
--import qualified Database.SQLite.Simple           as DB
--import qualified Database.SQLite.Simple.FromField as DB
--import qualified Database.SQLite.Simple.Ok        as DB
--import qualified Database.SQLite.Simple.ToField   as DB
import           Data.Time.Calendar
import           Data.Time.Calendar.Easter
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar.WeekDate
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Expr

data BoolExpr
  = BValue     Bool
  | BStatement DateStatement
  | BNot       BoolExpr
  | BAnd       BoolExpr BoolExpr
  | BOr        BoolExpr BoolExpr
  | BSame      BoolExpr BoolExpr
  | BEqual     IntExpr IntExpr
  | BGreater   IntExpr IntExpr
  | BLess      IntExpr IntExpr
  deriving (Show)

data DateStatement
  = IsLeapYear
  | IsWeekend
  | IsEaster -- same as: easter == 0
  deriving (Show)

data IntExpr
  = IValue    Integer
  | ISDate    SpecialDate
  | INegate   IntExpr
  | IAdd      IntExpr IntExpr
  | ISubtract IntExpr IntExpr -- same as (\a b -> IAdd a (INeg b))
  | IMultiply IntExpr IntExpr
  | IDivide   IntExpr IntExpr -- div, not quot
  | IModulo   IntExpr IntExpr -- mod, not rem
  deriving (Show)

data SpecialDate
  = SJulianDay
  | SYear
  | SMonth
  | SDay
  | SDayOfYear
  | SDayOfWeek
  | SYearCount  -- nth <day of week> of the year
  | SMonthCount -- nth <day of week> of the month
  | SEaster
  deriving (Show)

parseBoolExpr :: String -> Maybe BoolExpr
parseBoolExpr = parseMaybe boolExpr

--saveBoolExpr :: BoolExpr -> String
--saveBoolExpr = bToString

parseIntExpr :: String -> Maybe IntExpr
parseIntExpr = parseMaybe intExpr

--saveIntExpr :: IntExpr -> String
--saveIntExpr = iToString

--instance DB.ToField BoolExpr where
--  toField = DB.SQLText . T.pack . saveBoolExpr
--
--instance DB.FromField BoolExpr where
--  fromField f = case DB.fromField f of
--    DB.Errors e -> DB.Errors e
--    DB.Ok text -> case parseBoolExpr (T.unpack text) of
--      Nothing   -> DB.Errors [] -- TODO: Use proper exception?
--      Just expr -> DB.Ok expr
--
--instance DB.ToField IntExpr where
--  toField = DB.SQLText . T.pack . saveIntExpr
--
--instance DB.FromField IntExpr where
--  fromField f = case DB.fromField f of
--    DB.Errors e -> DB.Errors e
--    DB.Ok text -> case parseIntExpr (T.unpack text) of
--      Nothing   -> DB.Errors [] -- TODO: Use proper exception?
--      Just expr -> DB.Ok expr

{-
 - Evaluating expressions
 -}

evalBoolExpr :: BoolExpr -> Day -> Maybe Bool
evalBoolExpr (BValue v)     _ = pure v
evalBoolExpr (BStatement s) d = pure $ evalDateStatement s d
evalBoolExpr (BNot     a)   d = not  <$> evalBoolExpr a d
evalBoolExpr (BAnd     a b) d = (&&) <$> evalBoolExpr a d <*> evalBoolExpr b d
evalBoolExpr (BOr      a b) d = (||) <$> evalBoolExpr a d <*> evalBoolExpr b d
evalBoolExpr (BSame    a b) d = (==) <$> evalBoolExpr a d <*> evalBoolExpr b d
evalBoolExpr (BEqual   a b) d = (==) <$> evalIntExpr a d <*> evalIntExpr b d
evalBoolExpr (BGreater a b) d = (>)  <$> evalIntExpr a d <*> evalIntExpr b d
evalBoolExpr (BLess    a b) d = (<)  <$> evalIntExpr a d <*> evalIntExpr b d

evalDateStatement :: DateStatement -> Day -> Bool
evalDateStatement IsLeapYear d = isLeapYear $ year d
evalDateStatement IsWeekend  d = weekday d `elem` [6,7]
evalDateStatement IsEaster   d = orthodoxEaster (year d) == d

evalIntExpr :: IntExpr -> Day -> Maybe Integer
evalIntExpr (IValue v)      _ = pure v
evalIntExpr (ISDate s)      d = pure $ evalSpecialDate s d
evalIntExpr (INegate   a)   d = negate <$> evalIntExpr a d
evalIntExpr (IAdd      a b) d = (+) <$> evalIntExpr a d <*> evalIntExpr b d
evalIntExpr (ISubtract a b) d = (-) <$> evalIntExpr a d <*> evalIntExpr b d
evalIntExpr (IMultiply a b) d = (*) <$> evalIntExpr a d <*> evalIntExpr b d
evalIntExpr (IDivide   a b) d = do
  x <- evalIntExpr a d
  y <- evalIntExpr b d
  guard $ y /= 0
  return $ x `div` y
evalIntExpr (IModulo   a b) d = do
  x <- evalIntExpr a d
  y <- evalIntExpr b d
  guard $ y /= 0
  return $ x `mod` y

evalSpecialDate :: SpecialDate -> Day -> Integer
evalSpecialDate SJulianDay  d = julian d
evalSpecialDate SYear       d = year d
evalSpecialDate SMonth      d = month d
evalSpecialDate SDay        d = day d
evalSpecialDate SDayOfYear  d = weekday d
evalSpecialDate SDayOfWeek  d = yearday d
evalSpecialDate SYearCount  d = ((yearday d - 1) `div` 7) + 1
evalSpecialDate SMonthCount d = ((day d - 1) `div` 7) + 1
evalSpecialDate SEaster     d = diffDays (orthodoxEaster $ year d) d -- days after easter

{-
 - Helper functions for evaluation
 -}

julian :: Day -> Integer
julian = toModifiedJulianDay

year :: Day -> Integer
year d = let (r,_,_) = toGregorian d in r

month :: Day -> Integer
month d = let (_,r,_) = toGregorian d in toInteger r

day :: Day -> Integer
day d = let (_,_,r) = toGregorian d in toInteger r

weekday :: Day -> Integer
weekday d = let (_,_,r) = toWeekDate d in toInteger r

yearday :: Day -> Integer
yearday d = let (_,r) = toOrdinalDate d in toInteger r

{-
 - Converting to string
 -}

{-
parenthesizeIf :: (a -> String) -> [(a -> Bool)] -> a -> String
parenthesizeIf toString conditions expr =
  if any ($expr) conditions
    then "(" ++ toString expr ++ ")"
    else        toString expr

parenthesizeIfNot :: (a -> String) -> [(a -> Bool)] -> a -> String
parenthesizeIfNot toString conditions expr =
  if any ($expr) conditions
    then        toString expr
    else "(" ++ toString expr ++ ")"

iParenthesizeIf :: [(IntExpr -> Bool)] -> IntExpr -> String
iParenthesizeIf = parenthesizeIf iToString

iParenthesizeIfNot :: [(IntExpr -> Bool)] -> IntExpr -> String
iParenthesizeIfNot = parenthesizeIfNot iToString

isIAdd :: IntExpr -> Bool
isIAdd (IAdd _ _) = True
isIAdd _          = False

isISubtract :: IntExpr -> Bool
isISubtract (ISubtract _ _) = True
isISubtract _               = False

isINegate :: IntExpr -> Bool
isINegate (INegate _) = True
isINegate _           = False

isIValue :: IntExpr -> Bool
isIValue (IValue _) = True
isIValue _          = False

isISDate :: IntExpr -> Bool
isISDate (ISDate _) = True
isISDate _          = False

iToString :: IntExpr -> String
iToString (IValue a)      = show a
iToString (ISDate a)      = specialDateToString a
iToString (INegate a)     = '-' : iParenthesizeIfNot [isIValue, isISDate] a
iToString (IAdd a b)      = iToString a ++ " + " ++ iParenthesizeIf [isINegate] b
iToString (ISubtract a b) = iToString a ++ " - " ++ iParenthesizeIf [isINegate, isIAdd, isISubtract] b
iToString (IMultiply a b) = iParenthesizeIf [isIAdd, isISubtract] a ++ " * " ++ iParenthesizeIf [isIAdd, isISubtract, isINegate] b
iToString (IDivide a b)   = iParenthesizeIf [isIAdd, isISubtract] a ++ " / " ++ iParenthesizeIf [isIAdd, isISubtract, isINegate] b
iToString (IModulo a b)   = iParenthesizeIf [isIAdd, isISubtract] a ++ " % " ++ iParenthesizeIf [isIAdd, isISubtract, isINegate] b

specialDateToString :: SpecialDate -> String
specialDateToString SJulianDay  = "julian"
specialDateToString SYear       = "year"
specialDateToString SMonth      = "month"
specialDateToString SDay        = "day"
specialDateToString SDayOfYear  = "yearday"
specialDateToString SDayOfWeek  = "weekday"
specialDateToString SYearCount  = "yearcount"
specialDateToString SMonthCount = "monthcount"
specialDateToString SEaster     = "easter"

bParenthesizeIf :: [(BoolExpr -> Bool)] -> BoolExpr -> String
bParenthesizeIf = parenthesizeIf bToString

bParenthesizeIfNot :: [(BoolExpr -> Bool)] -> BoolExpr -> String
bParenthesizeIfNot = parenthesizeIfNot bToString

isBValue :: BoolExpr -> Bool
isBValue (BValue _) = True
isBValue _          = False

isBStatement :: BoolExpr -> Bool
isBStatement (BStatement _) = True
isBStatement _              = False

isBComparison :: BoolExpr -> Bool
isBComparison (BEqual _ _)   = True
isBComparison (BGreater _ _) = True
isBComparison (BLess _ _)    = True
isBComparison _              = False

bToString :: BoolExpr -> String
bToString (BValue True)  = "true"
bToString (BValue False) = "false"
bToString (BStatement a) = dateStatementToString a
bToString (BNot a)       = '!' : bParenthesizeIfNot [isBValue, isBStatement] a
bToString (BAnd a b)     = bToString a ++ " && " ++ bParenthesizeIf [isBSame, isBComparison] b

dateStatementToString :: DateStatement -> String
dateStatementToString = undefined
-}

{-
 - Parsing
 -}

type Parser = Parsec Void String

sc :: Parser ()
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

-- Helper functions for defining tables
prefix :: String -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

infixL :: String -> (a -> a -> a) -> Operator Parser a
infixL name f = InfixL (f <$ symbol name)

-- Parse IntExpr
intExpr :: Parser IntExpr
intExpr = makeExprParser intTerm intTable

intTable :: [[Operator Parser IntExpr]]
intTable =
  [ [ prefix "+" id
    , prefix "-" INegate
    ]
  , [ infixL "*" IMultiply
    , infixL "/" IDivide
    , infixL "%" IModulo
    ]
  , [ infixL "+" IAdd
    , infixL "-" ISubtract
    ]
  ]

intTerm :: Parser IntExpr
intTerm =   parens intExpr
        <|> IValue <$> integer
        <|> ISDate <$> pSpecialDate
        <?> "integer expression"

-- Parse BoolExpr
boolExpr :: Parser BoolExpr
boolExpr = makeExprParser boolTerm boolTable

boolTable :: [[Operator Parser BoolExpr]]
boolTable =
  [ [ prefix "!" BNot
    ]
  , [ infixL "&&" BAnd
    , infixL "||" BOr
    ]
  , [ infixL "==" BSame
    , infixL "!=" (\a b -> BNot (BSame a b))
    ]
  ]

boolTerm :: Parser BoolExpr
boolTerm =   parens boolExpr
         <|> BValue <$> bool
         <|> BStatement <$> pDateStatement
         <|> relIntExpr
         <?> "boolean expression"

relIntExpr :: Parser BoolExpr
relIntExpr = do
  first  <- intExpr
  rel    <- intRelation
  second <- intExpr
  return $ rel first second

intRelation :: Parser (IntExpr -> IntExpr -> BoolExpr)
intRelation =   (BEqual <$ symbol "==")
            <|> ((\a b -> BNot (BEqual a b))   <$ symbol "!=")
            <|> ((\a b -> BNot (BLess a b))    <$ symbol ">=")
            <|> ((\a b -> BNot (BGreater a b)) <$ symbol "<=")
            <|> (BGreater <$ symbol ">")
            <|> (BLess    <$ symbol "<")
            <?> "integer comparison"

pSpecialDate :: Parser SpecialDate
pSpecialDate =   name SJulianDay  "julian"
             <|> name SYear       "year"
             <|> name SMonth      "month"
             <|> name SDay        "day"
             <|> name SDayOfYear  "yearday"
             <|> name SDayOfWeek  "weekday"
             <|> name SYearCount  "yearcount"
             <|> name SMonthCount "monthcount"
             <|> name SEaster     "easter"
  where name a b = (a <$ symbol b)

pDateStatement :: Parser DateStatement
pDateStatement =   name IsLeapYear "isleapyear"
               <|> name IsWeekend  "isweekend"
               <|> name IsEaster   "iseaster"
  where name a b = (a <$ symbol b)
