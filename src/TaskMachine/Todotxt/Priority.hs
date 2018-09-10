-- | Read, parse and write files in the <https://github.com/todotxt/todo.txt todo.txt> format.


module TaskMachine.Todotxt.Priority
  ( Priority()
  , priorityToChar
  , charToPriority
  , showPriority
  -- * Parsing
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

showPriority :: Priority -> String
showPriority p = '(' : priorityToChar p : ")"

{- Parsing -}

type Parser = Parsec Void String

priorityChar :: Parser Priority
priorityChar = do
  c <- anyChar
  case charToPriority c of
    Just p -> pure p
    Nothing -> failure (Just $ Tokens $ c :| []) (Set.singleton $ Label $ 'p' :| "riority character")

priority :: Parser Priority
priority = char '(' *> priorityChar <* char ')'
