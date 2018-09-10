-- | Read, parse and write files in the <https://github.com/todotxt/todo.txt todo.txt> format.


module TaskMachine.Todotxt.Dates
  ( Dates()
  , showDates
  -- * Modification
  , creationDate
  , completionDate
  , setCreationDate
  , setCompletionDate
  -- * Parsing
  , day
  , dates
  ) where

import           Control.Monad
import           Data.Void

import           Data.Time.Calendar
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | The combination of creation and completion date of a task.
--
-- These two dates were combined because, according to the
-- <https://github.com/todotxt/todo.txt/blob/master/README.md#todotxt-format-rules todo.txt format rules>,
-- the creation date "must be specified if completion date is".
data Dates
  = NoDate
  | CrDate Day
  | CrCoDate Day Day
  deriving (Show)

{- Modification -}

-- | Convert a 'Dates' to a string representation that can be used inside a todo.txt task
-- and parsed by 'dates'
showDates :: Dates -> String
showDates NoDate                         = ""
showDates (CrDate creation)              = show creation
showDates (CrCoDate creation completion) = show creation ++ " " ++ show completion

-- | Retrieve the creation date, if one exists
creationDate :: Dates -> Maybe Day
creationDate (CrCoDate day _) = Just day
creationDate (CrDate day)     = Just day
creationDate NoDate           = Nothing

-- | Retrieve the completion date, if one exists
completionDate :: Dates -> Maybe Day
completionDate (CrCoDate _ day) = Just day
completionDate _                = Nothing

-- | Set the creation date to a specific value
setCreationDate :: Day -> Dates -> Dates
setCreationDate creation (CrCoDate _ completion) = CrCoDate creation completion
setCreationDate creation _                       = CrDate creation

-- | Set the completion date to a specific value.
--
-- The first argument is a default creation date, in case none exists.
-- This is because a completion date can only exist in combination with a
-- creation date, as per the todo.txt format.
setCompletionDate :: Day -> Day -> Dates -> Dates
setCompletionDate _ completion (CrCoDate creation _) = CrCoDate creation completion
setCompletionDate creation completion _              = CrCoDate creation completion

{- Parsing -}

type Parser = Parsec Void String

-- | Parse one date of the format @YYYY-MM-DD@ (with no leading or trailing spaces).
day :: Parser Day
day = label "date" $ do
  y <- integer
  void $ char '-'
  m <- int
  void $ char '-'
  d <- int
  pure $ fromGregorian y m d
  where
    integer :: Parser Integer
    integer = read <$> count 4 digitChar
    int :: Parser Int
    int = read <$> count 2 digitChar

-- | Parse either zero, one or two dates of the format @YYYY-MM-DD@ (with no leading or trailing spaces).
--
-- If only one date is present, it is interpreted as the creation date.
dates :: Parser Dates
dates = try datesCrCo <|> try datesCr <|> pure NoDate
  where
    datesCrCo :: Parser Dates
    datesCrCo = CrCoDate <$> day <*> (char ' ' *> day)
    datesCr :: Parser Dates
    datesCr = CrDate <$> day
