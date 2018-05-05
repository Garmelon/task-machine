module TaskMachine.Subtask
  ( Subtask(..)
  , SubtaskID
  , allRepetitionsTotal
  , allRepetitionsDone
  ) where

import qualified Data.Text as T

type SubtaskID = Integer

data Subtask = Subtask
  { subID               :: SubtaskID
  , subLabel            :: T.Text
  , subRepetitionsTotal :: Integer
  , subRepetitionsDone  :: Integer
  }

allRepetitionsTotal :: [Subtask] -> Integer
allRepetitionsTotal = sum . map subRepetitionsTotal

allRepetitionsDone :: [Subtask] -> Integer
allRepetitionsDone = sum . map subRepetitionsDone
