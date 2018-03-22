{-# LANGUAGE OverloadedStrings #-}

module TaskMachine.DateExpr
  ( BoolExpr
  , parseBoolExpr
  , saveBoolExpr
  , IntExpr
  , parseIntExpr
  , saveIntExpr
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Void

import qualified Data.Text                        as T
import qualified Database.SQLite.Simple           as DB
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.Ok        as DB
import qualified Database.SQLite.Simple.ToField   as DB
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
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

saveBoolExpr :: BoolExpr -> String
saveBoolExpr = bToString

parseIntExpr :: String -> Maybe IntExpr
parseIntExpr = parseMaybe intExpr

saveIntExpr :: IntExpr -> String
saveIntExpr = iToString

instance DB.ToField BoolExpr where
  toField = DB.SQLText . T.pack . saveBoolExpr

instance DB.FromField BoolExpr where
  fromField f = case DB.fromField f of
    DB.Errors e -> DB.Errors e
    DB.Ok text -> case parseBoolExpr (T.unpack text) of
      Nothing   -> DB.Errors [] -- TODO: Use proper exception?
      Just expr -> DB.Ok expr

instance DB.ToField IntExpr where
  toField = DB.SQLText . T.pack . saveIntExpr

instance DB.FromField IntExpr where
  fromField f = case DB.fromField f of
    DB.Errors e -> DB.Errors e
    DB.Ok text -> case parseIntExpr (T.unpack text) of
      Nothing   -> DB.Errors [] -- TODO: Use proper exception?
      Just expr -> DB.Ok expr

{-
 - Evaluating expressions
 -}

-- TODO

{-
 - Converting to string
 -}

iParenthesizeIf :: [(IntExpr -> Bool)] -> IntExpr -> String
iParenthesizeIf conditions expr =
  if or (map ($expr) conditions)
    then "(" ++ iToString expr ++ ")"
    else        iToString expr

iParenthesizeIfNot :: [(IntExpr -> Bool)] -> IntExpr -> String
iParenthesizeIfNot conditions expr =
  if or (map ($expr) conditions)
    then        iToString expr
    else "(" ++ iToString expr ++ ")"

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

bToString :: BoolExpr -> String
bToString = undefined

dateStatementToString :: DateStatement -> String
dateStatementToString = undefined

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
--prefix :: String -> a -> Parser a
prefix name f = Prefix (f <$ symbol name)

--infixL :: String -> a -> Parser a
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
