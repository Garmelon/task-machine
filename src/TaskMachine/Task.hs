-- | Read, parse and write tasks in a human-readable format
--
-- The format used by this module is inspired by the
-- <https://github.com/todotxt/todo.txt/blob/master/README.md todo.txt format>
-- and attempts to follow similar goals:
--
-- 1. A single line represents a single task
-- 2. Should be entirely human-readable and easily editable using a simple text viewer/editor
-- 3. When sorted alphabetically, should yield useful results
-- 4. Completing a task is as simple as changing the @-@ into a @x@

module TaskMachine.Task
  ( Task(..)
  , Completion(..)
  , Priority(..)
  , priorityToChar
  , charToPriority
  , Description
  , Snippet(..)
  -- * Formatting
  , formatTask
  , formatDate
  , formatCompletion
  , formatPriority
  , formatDescription
  -- * Parsing
  -- ** Utilities
  , Parser
  , andSpace
  , maybeParse
  -- ** Objects
  , pTask
  , pTasks
  , pDate
  , pCompletion
  , pPriorityChar
  , pPriority
  , pDueDate
  , pCreationDate
  , pDescription
  ) where

import           Control.Applicative  (liftA2)
import           Control.Monad
import qualified Data.List.NonEmpty   as NE
import           Data.Void

import qualified Data.Set             as Set
import           Data.Time.Calendar
import           Text.Megaparsec
import           Text.Megaparsec.Char

{- Task -}

data Task = Task
  { taskCompleted   :: Completion
  , taskPriority    :: Maybe Priority
  , taskDue         :: Maybe Day
  , taskCreated     :: Maybe Day
  , taskDescription :: Description
  } deriving (Show)

formatTask :: Task -> String
formatTask t
  =  formatCompletion (taskCompleted t) ++ " "
  ++ maybeWithSpace formatPriority (taskPriority t)
  ++ maybeWithSpace formatDate     (taskDue t)
  ++ maybeWithSpace formatDate     (taskCreated t)
  ++ formatDescription (taskDescription t)
  where
    maybeWithSpace :: (a -> String) -> Maybe a -> String
    maybeWithSpace _ Nothing  = ""
    maybeWithSpace f (Just a) = f a ++ " "

--parseTasks :: FilePath -> String -> Either (ParseError Char Void) [Task]
--parseTasks = parse pTasks -- That's easy!

{- Dates -}

formatDate :: Day -> String
formatDate = show

{- Completion -}

data Completion
  = Incomplete
  | Complete (Maybe Day)
  deriving (Show)

formatCompletion :: Completion -> String
formatCompletion Incomplete            = "-"
formatCompletion (Complete Nothing)    = "x"
formatCompletion (Complete (Just day)) = "x " ++ formatDate day

{- Priority -}

data Priority
  = PrioA | PrioB | PrioC | PrioD | PrioE | PrioF | PrioG
  | PrioH | PrioI | PrioJ | PrioK | PrioL | PrioM | PrioN
  | PrioO | PrioP | PrioQ | PrioR | PrioS | PrioT | PrioU
  | PrioV | PrioW | PrioX | PrioY | PrioZ
  deriving (Bounded, Enum, Eq, Show, Ord)

formatPriority :: Priority -> String
formatPriority p = '(' : priorityToChar p : ")"

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

{- Description -}

type Description = [Snippet]

data Snippet
  = Word String
  | Space String
  | Project String
  | Context String
  | KeyValue String String
  deriving (Eq, Show)

formatDescription :: Description -> String
formatDescription = concatMap toString
  where
    toString :: Snippet -> String
    toString (Word s)       = s
    toString (Space s)      = s
    toString (Project s)    = '+' : s
    toString (Context s)    = '@' : s
    toString (KeyValue k v) = k ++ ":" ++ v

{- Parsing -}

type Parser = Parsec Void String

-- Completion

pDate :: Parser Day
pDate = label "date" $ fromGregorian
    <$> integer
    <*> (char '-' *> int)
    <*> (char '-' *> int)
  where
    integer :: Parser Integer
    integer = read <$> count 4 digitChar
    int :: Parser Int
    int = read <$> count 2 digitChar

pCompletion :: Parser Completion
pCompletion =   Incomplete <$ char '-'
            <|> char 'x' *> (Complete <$> maybeParse pDate)

-- Priority
pPriorityChar :: Parser Priority
pPriorityChar = do
  c <- anyChar
  case charToPriority c of
    Just p -> pure p
    Nothing -> failure (Just $ Tokens $ c NE.:| [])
                       (Set.singleton $ Label $ 'p' NE.:| "riority character")

pPriority :: Parser Priority
pPriority = char '(' *> pPriorityChar <* char ')'

-- Dates

pDueDate :: Parser Day
pDueDate = char 'd' *> pDate

pCreationDate :: Parser Day
pCreationDate = char 'c' *> pDate

-- Description

wordBody :: Parser String
wordBody = takeWhile1P (Just "word character") (not . (`elem` " \n"))

pWord :: Parser Snippet
pWord = Word <$> wordBody

pSpace :: Parser Snippet
pSpace = Space <$> takeWhile1P (Just "space") (==' ')

pProject :: Parser Snippet
pProject = char '+' *> (Project <$> wordBody)

pContext :: Parser Snippet
pContext = char '@' *> (Context <$> wordBody)

pKeyValue :: Parser Snippet
pKeyValue = KeyValue <$> (keyBody <* char ':') <*> wordBody
  where
    keyBody = takeWhile1P (Just "key character")   (not . (`elem` ": \n"))

pDescription :: Parser Description
pDescription = pSnippet
  where
    pEnd :: Parser Description
    pEnd
      =   [] <$ (eof <|> void (char '\n'))
      <|> pSnippet
    pSnippet :: Parser Description
    pSnippet
      =   try (liftA2 (:) pSpace    pEnd)
      <|> try (liftA2 (:) pProject  pEnd)
      <|> try (liftA2 (:) pContext  pEnd)
      <|> try (liftA2 (:) pKeyValue pEnd)
      <|>      liftA2 (:) pWord     pEnd
      <?> "description"

-- Task

andSpace :: Parser a -> Parser a
andSpace = (<* char ' ')

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse p = Just <$> try p <|> pure Nothing

pTask :: Parser Task
pTask
  =   Task
  <$> andSpace pCompletion
  <*> maybeParse (andSpace pPriority)
  <*> maybeParse (andSpace pDueDate)
  <*> maybeParse (andSpace pCreationDate)
  <*> pDescription

pTasks :: Parser [Task]
pTasks = many pTask <* eof
