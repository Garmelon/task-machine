-- | Read, parse and write files in the <https://github.com/todotxt/todo.txt todo.txt> format.

module TaskMachine.Todotxt.Priority
  ( Priority()
  , priorityToChar
  , charToPriority
  , showPriority
  -- * Parsing
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

-- | A task's priority.
--
-- Priorities are labeled using uppercase A to Z,
-- with priority A being the most important priority.
-- In the 'Priority' type, priority A is the smallest priority.
--
-- Tasks should be sorted from smallest to largest (i. e. least important) priority.
-- Tasks without priority should appear after tasks with priority.
data Priority
  = PrioA | PrioB | PrioC | PrioD | PrioE | PrioF | PrioG
  | PrioH | PrioI | PrioJ | PrioK | PrioL | PrioM | PrioN
  | PrioO | PrioP | PrioQ | PrioR | PrioS | PrioT | PrioU
  | PrioV | PrioW | PrioX | PrioY | PrioZ
  deriving (Bounded, Enum, Eq, Show, Ord)

-- | Convert a priority to its corresponding character of the alphabet
priorityToChar :: Priority -> Char
priorityToChar p = toEnum (fromEnum 'A' + fromEnum p)

-- | Convert a character of the alphabet (uppercase A to Z)
-- to its corresponding priority
charToPriority :: Char -> Maybe Priority
charToPriority c
  | min_value <= value && value <= max_value = Just $ toEnum value
  | otherwise                                = Nothing
  where
    value = fromEnum c - fromEnum 'A'
    min_value = fromEnum (minBound :: Priority)
    max_value = fromEnum (maxBound :: Priority)

-- | Convert a 'Priority' to a string representation that can be used inside a todo.txt task
-- and parsed by 'priority'
showPriority :: Priority -> String
showPriority p = '(' : priorityToChar p : ")"

{- Parsing -}

type Parser = Parsec Void String

-- | Parse a priority character (see 'priorityToChar') and return the corresponding priority
priorityChar :: Parser Priority
priorityChar = do
  c <- anyChar
  case charToPriority c of
    Just p -> pure p
    Nothing -> failure (Just $ Tokens $ c :| []) (Set.singleton $ Label $ 'p' :| "riority character")

-- | Parse a priority of the format @(*)@ where @*@ is a letter of the alphabet (uppercase A to Z)
priority :: Parser Priority
priority = char '(' *> priorityChar <* char ')'
