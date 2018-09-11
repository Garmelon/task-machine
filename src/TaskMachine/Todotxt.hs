-- | Read, parse and write tasks in the <https://github.com/todotxt/todo.txt todo.txt> format.

module TaskMachine.Todotxt
  (
  -- * Tasks
    Task(..)
  , formatTask
  , parseTasks
  -- * Creation and deletion dates
  , Dates(..)
  , formatDates
  -- * Task priority
  , Priority(..)
  , formatPriority
  , priorityToChar
  , charToPriority
  -- * Parsing
  , Parser
  , task
  , tasks
  , day
  , dates
  , priorityChar
  , priority
  ) where

import           Control.Monad
import           Data.List.NonEmpty
import           Data.Void

import           Data.Set              as Set
import           Data.Time.Calendar
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Error

type Parser = Parsec Void String

{- Dates -}

data Dates
  = CrDate Day
  | CoCrDate Day Day
  deriving (Eq, Show)

formatDates :: Dates -> String
formatDates (CrDate cr)      = show cr
formatDates (CoCrDate cr co) = show cr ++ " " ++ show co

{- Dates parsing -}

day :: Parser Day
day = label "date" $ fromGregorian
    <$> integer
    <*> (char '-' *> int)
    <*> (char '-' *> int)
  where
    integer :: Parser Integer
    integer = read <$> count 4 digitChar
    int :: Parser Int
    int = read <$> count 2 digitChar

dates :: Parser Dates
dates = try datesCrCo <|> datesCr
  where
    datesCrCo :: Parser Dates
    datesCrCo = CoCrDate <$> (day <* char ' ') <*> day
    datesCr :: Parser Dates
    datesCr = CrDate <$> day

{- Priority -}

data Priority
  = PrioA | PrioB | PrioC | PrioD | PrioE | PrioF | PrioG
  | PrioH | PrioI | PrioJ | PrioK | PrioL | PrioM | PrioN
  | PrioO | PrioP | PrioQ | PrioR | PrioS | PrioT | PrioU
  | PrioV | PrioW | PrioX | PrioY | PrioZ
  deriving (Bounded, Enum, Eq, Show, Ord)

priorityToChar :: Priority -> Char
priorityToChar p = toEnum (fromEnum 'A' + fromEnum p)

charToPriority :: Char -> Maybe Priority
charToPriority c
  | min_value <= value && value <= max_value = Just $ toEnum value
  | otherwise                                = Nothing
  where
    value = fromEnum c - fromEnum 'A'
    min_value = fromEnum (minBound :: Priority)
    max_value = fromEnum (maxBound :: Priority)

formatPriority :: Priority -> String
formatPriority p = '(' : priorityToChar p : ")"

{- Priority parsing -}

priorityChar :: Parser Priority
priorityChar = do
  c <- anyChar
  case charToPriority c of
    Just p -> pure p
    Nothing -> failure (Just $ Tokens $ c :| [])
                       (Set.singleton $ Label $ 'p' :| "riority character")

priority :: Parser Priority
priority = char '(' *> priorityChar <* char ')'

{- Task -}

data Task = Task
  { taskCompleted   :: Bool
  , taskPriority    :: Maybe Priority
  , taskDates       :: Maybe Dates
  , taskDescription :: String -- might change in the future
  }
  deriving (Show)

--instance Show Task where
--  show = formatTask

formatTask :: Task -> String
formatTask (Task done prio dates desc)
  =  (if done then "x " else "")
  ++ maybe "" ((++" ") . formatPriority) prio
  ++ maybe "" ((++" ") . formatDates) dates
  ++ desc

parseTasks :: FilePath -> String -> Either (ParseError Char Void) [Task]
parseTasks = parse tasks -- hehe

{- Task parsing -}

andSpace :: Parser a -> Parser a
andSpace p = p <* char ' '

completed :: Parser ()
completed = void $ char 'x'

boolParse :: Parser a -> Parser Bool
boolParse p = (True <$ try p) <|> pure False

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse p = (Just <$> try p) <|> pure Nothing

untilEndOfLine :: Parser String
untilEndOfLine = takeWhile1P (Just "description") (/='\n')

task :: Parser Task
task =   Task
     <$> boolParse  (andSpace completed)
     <*> maybeParse (andSpace priority)
     <*> maybeParse (andSpace dates)
     <*> untilEndOfLine

tasks :: Parser [Task]
tasks = many $ task <* (eof <|> void (char '\n'))
