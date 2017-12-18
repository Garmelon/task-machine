module Task where

import DateExpr
import Control.Applicative
import Control.Monad
import Data.Time.Calendar
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Priority = Must
              | Should
              | Can
  deriving (Eq, Ord, Enum, Show)

type Description = String
type Amount = Integer
type Duration = Integer

data When = Forever
          | Until Day
          | During Duration Day
          | Whenever Duration DateExpr
  deriving (Show)

data Task = Task Priority Description Amount When
  deriving (Show)

{-
 - Parse Tasks
 -}

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: (Integral i) => Parser i
integer = lexeme L.decimal

options :: (a -> Parser a) -> [a] -> Parser a
options f = choice . map f

pDateIso :: Parser Day
pDateIso = do
  y <- integer
  char '-'
  m <- integer
  char '-'
  d <- integer
  return $ fromGregorian y m d

pDateDots :: Parser Day
pDateDots = do
  d <- integer
  char '.'
  m <- integer
  char '.'
  y <- integer
  return $ fromGregorian y m d

pDate :: Parser Day
pDate = try pDateIso <|> pDateDots

pDuration :: Parser Duration
pDuration = do
  symbol "for"
  n <- integer
  symbol "days" <|> symbol "day"
  return n

pUntil = symbol "until" *> pDate
pWhenever = symbol "whenever" *> parseDateExpr

pWhenShortcut :: Parser When
pWhenShortcut =   Whenever 1 daily  <$ symbol "daily"
              <|> Whenever 7 weekly <$ symbol "weekly"
              <?> "when"

pWhen :: Parser When
pWhen =   pWhenShortcut
      <|> liftA Until pUntil
      <|> try (liftA2 During pDuration pUntil)
      <|> try (liftA2 Whenever pDuration pWhenever)
      <|> liftA2 Whenever (return 1) pWhenever
     -- <|> return Forever
      <?> "when"

pPriority :: Parser Priority
pPriority =   Must   <$ symbol "must"
          <|> Should <$ symbol "should"
          <|> Can    <$ options symbol ["can", "might", "may"]

pAmount :: Parser Amount
pAmount =   1 <$ symbol "once"
        <|> 2 <$ symbol "twice"
        <|> integer <* (symbol "times" <|> symbol "time")
        <?> "amount"

pDesc :: Parser Description
-- pDesc = between (symbol "<") (symbol ">")
--       $ takeWhile1P (Just "description") (not . (`elem` "<>"))
pDesc = someTill anyChar (try $ lookAhead pAmountOrWhen) <* sc
  where pAmountOrWhen =   try (sc <* pAmount)
                      <|> sc <* pWhen

parseTask :: Parser Task
-- because liftA only goes up to liftA3
parseTask =   try (liftM4 Task pPriority pDesc pAmount pWhen)
          <|> try (liftM4 Task pPriority pDesc (return 1) pWhen)
          <|> try (liftM4 Task pPriority pDesc pAmount (return Forever))
          <|> liftM4 Task pPriority (some anyChar) (return 1) (return Forever)
