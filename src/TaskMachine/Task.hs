-- | Read, parse and write tasks in a human-readable format
--
-- The format used by this module is inspired by the
-- <https://github.com/todotxt/todo.txt/blob/master/README.md todo.txt format>
-- and attempts to follow similar goals:
--
-- 1. A single line represents a single task
-- 2. Should be entirely human-readable and easily editable using a simple text viewer/editor
-- 3. When sorted alphabetically, should yield useful results
-- 4. Completing a task is as simple as changing the @"-"@ into @"x"@
--
-- [Incomplete task] @"-[ (\<priority\>)][ d\<due date\>][ c\<creation date\>] \<description\>"@
-- [Complete task] @"x[\<completion date\>][ (\<priority\>)][ d\<due date\>][ c\<creation date\>] \<description\>"@

module TaskMachine.Task
  ( Task(..)
  , Completion(..)
  , Priority(..)
  , priorityToChar
  , charToPriority
  , Description
  , Snippet(..)
  -- * Misc stuff
  , emptyTask
  , newTask
  , compareTasks
  -- * Formatting
  , formatTask
  , formatTaskHalves
  , formatTasks
  , formatDate
  , formatDue
  , formatCreated
  , formatCompletion
  , formatPriority
  , formatDescription
  , formatSnippet
  -- * Parsing
  -- ** Utilities
  , Parser
  , andSpace
  , maybeParse
  -- ** Objects
  , pTask
  , pTasks
  , pCompletion
  , pPriorityChar
  , pPriority
  , pDate
  , pDue
  , pCreated
  , pDescription
  , pSnippet
  ) where

import           Control.Monad
import           Data.Function
import qualified Data.List.NonEmpty   as NE
import           Data.Void

import qualified Data.Set             as Set
import           Data.Time.Calendar
import           Text.Megaparsec
import           Text.Megaparsec.Char

{- Task -}

-- | A single task
data Task = Task
  { taskCompletion  :: Completion
  , taskPriority    :: Maybe Priority
  , taskDue         :: Maybe Day
  , taskCreated     :: Maybe Day
  , taskDescription :: Description
  } deriving (Show)

-- | Convert a 'Task' to its string representation.
-- This string representation is split into a pre-description and a description part.
--
-- For further detail, see 'formatTask'
formatTaskHalves :: Task -> (String, String)
formatTaskHalves t =
  (  formatCompletion (taskCompletion t) ++ " "
  ++ maybeWithSpace formatPriority (taskPriority t)
  ++ maybeWithSpace formatDue(taskDue t)
  ++ maybeWithSpace formatCreated (taskCreated t)
  , formatDescription (taskDescription t)
  )
  where
    maybeWithSpace :: (a -> String) -> Maybe a -> String
    maybeWithSpace _ Nothing  = ""
    maybeWithSpace f (Just a) = f a ++ " "

-- | Convert a 'Task' to its string representation, which can be parsed by 'pTask'.
--
-- If this string representation is parsed using 'pTask', it should yield the original task,
-- with a few exceptions:
-- 'taskPriority', 'taskDue' and/or 'taskCreated' might be @Nothing@, but the 'taskDescription'
-- could include the text version of these in the beginning, i. e. @taskDescription = "(A) hello"@.
-- In that case, converting the task to a string and back yields a different result.
formatTask :: Task -> String
formatTask t =
  let (predesc, desc) = formatTaskHalves t
  in  predesc ++ desc

-- | Convert a list of tasks to its string representation, which can be parsed by 'pTasks'.
formatTasks :: [Task] -> String
formatTasks = concatMap ((++"\n") . formatTask)

{- Dates -}

-- | Convert a 'Day' to @YYYY-MM-DD@ format.
--
-- Example: @"2018-09-08"@
formatDate :: Day -> String
formatDate = show

-- | Convert a 'Day' into the due date string representation, which can be parsed by 'pDue'.
--
-- Example: @"d2018-09-08"@
formatDue :: Day -> String
formatDue d = 'd' : formatDate d

-- | Convert a 'Day into the creation date string representation, which can be parsed by 'pCreation.
--
-- Example: @"c2018-09-08"@
formatCreated :: Day -> String
formatCreated d = 'c' : formatDate d

{- Completion -}

-- | Whether a task has been completed or not.
--
-- May include the completion date if the task is complete.
data Completion
  = Incomplete
  | Complete (Maybe Day)
  deriving (Eq, Ord, Show)

-- | Convert a 'Completion' to its string representation, which can be parsed by 'pCompletion'.
--
-- Examples: @"-"@, @"x"@, @"x2018-09-08"@
formatCompletion :: Completion -> String
formatCompletion Incomplete            = "-"
formatCompletion (Complete Nothing)    = "x"
formatCompletion (Complete (Just day)) = "x" ++ formatDate day

{- Priority -}

-- | A task's priority, which can be any uppercase character from A to Z.
data Priority
  = PrioA | PrioB | PrioC | PrioD | PrioE | PrioF | PrioG
  | PrioH | PrioI | PrioJ | PrioK | PrioL | PrioM | PrioN
  | PrioO | PrioP | PrioQ | PrioR | PrioS | PrioT | PrioU
  | PrioV | PrioW | PrioX | PrioY | PrioZ
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Convert a 'Priority' to its string representation, which can be parsed by 'pPriority'.
--
-- Example: @"(A)"@
formatPriority :: Priority -> String
formatPriority p = '(' : priorityToChar p : ")"

-- | Convert a Priority to the corresponding uppercase character from A to Z.
priorityToChar :: Priority -> Char
priorityToChar p = toEnum (fromEnum 'A' + fromEnum p)

-- | Convert a character to the corresponding Priority, if possible.
charToPriority :: Char -> Maybe Priority
charToPriority c
  | min_value <= value && value <= max_value = Just $ toEnum value
  | otherwise                                = Nothing
  where
    value = fromEnum c - fromEnum 'A'
    min_value = fromEnum (minBound :: Priority)
    max_value = fromEnum (maxBound :: Priority)

{- Description -}

-- | A bunch of snippets that make up the description of a task.
type Description = [Snippet]

-- | Part of a task's description.
data Snippet
  = Word String
  -- ^ A space-delimited word that is not any of the special variants listed below.
  | Space String
  -- ^ One or more spaces that delimit words.
  | Project String
  -- ^ A word beginning with @"+"@.
  | Context String
  -- ^ A word beginning with @"\@"@.
  | KeyValue String String
  -- ^ A word of the form @key:value@.
  -- The key and value cannot contain any spaces.
  -- The key cannot contain any @":"@ characters, but the value can.
  deriving (Show)

-- | Convert a 'Description' into its string representation, which can be parsed by 'pDescription'.
--
-- Example: @"task for +project \@context"@
formatDescription :: Description -> String
formatDescription = concatMap formatSnippet

-- | Convert a 'Snippet' into its string representation, which can be parsed by 'pSnippet'.
formatSnippet :: Snippet -> String
formatSnippet (Word s)       = s
formatSnippet (Space s)      = s
formatSnippet (Project s)    = '+' : s
formatSnippet (Context s)    = '@' : s
formatSnippet (KeyValue k v) = k ++ ":" ++ v

{- Parsing -}

-- | Simple megaparsec parser over 'String's.
type Parser = Parsec Void String

-- Dates

-- | Parse a date in @YYYY-MM-DD@ format (see 'formatDate').
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

-- | Parse a date in the due date format (see 'formatDue').
pDue :: Parser Day
pDue = label "due date" $ char 'd' *> pDate

-- | Parse a date in the creation date format (see 'formatCreated').
pCreated :: Parser Day
pCreated = label "creation date" $ char 'c' *> pDate

-- Completion

-- | Parse a 'Completion' (see 'formatCompletion').
pCompletion :: Parser Completion
pCompletion =   Incomplete <$ char '-'
            <|> char 'x' *> label "completion date" (Complete <$> maybeParse pDate)

-- Priority

-- | Parse the priority character inside the parentheses (see 'charToPriority').
pPriorityChar :: Parser Priority
pPriorityChar = do
  c <- anyChar
  case charToPriority c of
    Just p -> pure p
    Nothing -> failure (Just $ Tokens $ c NE.:| [])
                       (Set.singleton $ Label $ 'p' NE.:| "riority character")

-- | Parse a 'Priority' (see 'formatPriority').
pPriority :: Parser Priority
pPriority = label "priority" $ char '(' *> pPriorityChar <* char ')'

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
    keyBody = takeWhile1P (Just "key character") (not . (`elem` ": \n"))

-- | Parse a 'Description' (see 'formatDescription').
pDescription :: Parser Description
pDescription = label "description" snippet
  where
    end :: Parser Description
    end = [] <$ (eof <|> void (char '\n')) <|> snippet
    snippet :: Parser Description
    snippet = (:) <$> pSnippet <*> end

-- | Parse a 'Snippet' (see 'formatSnippet').
pSnippet :: Parser Snippet
pSnippet
  =   try pSpace
  <|> try pProject
  <|> try pContext
  <|> try pKeyValue
  <|>     pWord

-- Task

-- | Parse a parser and a single space afterwards.
andSpace :: Parser a -> Parser a
andSpace = (<* char ' ')

-- | If the parser succeeds, return its results warpped in @Just@.
-- Otherwise, return @Nothing@.
maybeParse :: Parser a -> Parser (Maybe a)
maybeParse p = Just <$> try p <|> pure Nothing

-- | Parse a 'Task' (see 'formatTask').
pTask :: Parser Task
pTask
  =   Task
  <$> andSpace pCompletion
  <*> maybeParse (andSpace pPriority)
  <*> maybeParse (andSpace pDue)
  <*> maybeParse (andSpace pCreated)
  <*> pDescription

-- | Parse a list of 'Task's (see 'formatTasks').
pTasks :: Parser [Task]
pTasks = many pTask <* eof

 {- Misc stuff -}

emptyTask :: Task
emptyTask = Task Incomplete Nothing Nothing Nothing []

-- | Create a new task with empty description and the given date as creation date
newTask :: Day -> Task
newTask day = Task Incomplete Nothing Nothing (Just day) []

-- Inverted compare for Maybes: Nothing comes after Just
compareMaybe :: Ord a => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing  Nothing  = EQ
compareMaybe (Just _) Nothing  = LT
compareMaybe Nothing  (Just _) = GT
compareMaybe (Just x) (Just y) = compare x y

compareDescription :: Description -> Description -> Ordering
compareDescription = compare `on` formatDescription

compareTasks :: Task -> Task -> Ordering
compareTasks a@(Task (Complete _) _ _ _ _) b@(Task (Complete _) _ _ _ _) = mconcat
  [ compare (taskCompletion b) (taskCompletion a)
  , compareMaybe (taskPriority a) (taskPriority b)
  , compareMaybe (taskDue a) (taskDue b)
  , compareDescription (taskDescription a) (taskDescription b)
  ]
compareTasks a b = mconcat
  [ compare (taskCompletion a) (taskCompletion b)
  , compareMaybe (taskPriority a) (taskPriority b)
  , compareMaybe (taskDue a) (taskDue b)
  , compareDescription (taskDescription a) (taskDescription b)
  ]
